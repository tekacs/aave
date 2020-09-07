(ns aave.code
  "Namespace providing utilities to parse and generate code"
  (:require [aave.config :as config]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.error :as me]))

(defn overloaded?
  "Returns whether the provided `params+body` list is overloaded in arities"
  [params+body]
  (not (vector? (first params+body))))

(defn map-body
  "Maps over the `body` portion of a `params+body` list with `f`. Calls `f` with both the param
  symbols and the body."
  [f params+body]
  (if (overloaded? params+body)
    (map #(map-body f %) params+body)
    (cons (first params+body) (f (first params+body) (rest params+body)))))

(defn pure?
  "Naively iterates over symbols in the provided `form` looking for whether any
  of them end with a literal `!`. If so, returns `false`.

  Does not count `persistent!`."
  [form]
  (let [form (if-not (seqable? form)
               (list form)
               form)]
    (->> (flatten form)
         (filter symbol?)
         (keep #(re-matches #".*!\??$" (str %)))
         (remove #(re-matches #"aave.code/.*|persistent!|volatile!|vreset!|vswap!" %))
         (empty?))))

(def impure?
  "The complement of `pure?`."
  (complement pure?))

(defn on-purity-fail!
  "Called upon impurity failure by default"
  [name_]
  (throw (ex-info (str "Function name " name_ " implies purity, but it calls impure code") {:name name_})))

(defn- extended-humanize [param-syms ex-data]
  (let [param-count (count (:value ex-data))
        humanize (me/humanize ex-data)
        long-params (take param-count (concat humanize (repeat nil)))]
    (zipmap param-syms long-params)))

(defn on-instrument-fail!
  "Called upon instrument failure by default"
  [name_ param-syms ex-data]
  (throw
    (ex-info
      (str "The function " name_ " was passed invalid parameters: " (extended-humanize param-syms ex-data))
      ex-data)))

(defn on-outstrument-fail!
  "Called upon outstrument failure by default"
  [name_ ex-data]
  (throw
    (ex-info
      (str "The function " name_ " returned an invalid value: " (me/humanize ex-data))
      ex-data)))

(defn extract-arg
  "Utility function for extracting arguments from a list.

  Returns a tuple of the value matching the `pred` if it returns logical true and the
  rest of the arg list. Otherwise returns the original arg list.

  Takes an optional `error` which will assert that the `pred` returns a truthy
  value.."
  ([args pred] (extract-arg args pred nil))
  ([args pred error]
   (let [[arg new-args] [(first args) (rest args)]]
     (if error
       (do (assert (pred arg) error)
           [arg new-args])
       (if (pred arg)
         [arg new-args]
         [nil args])))))

(defmacro generate
  "Generates code using the provided `generate-map`"
  {:arglists '([generate-map])}
  [{name_ :name :keys [doc params+body private param-schema ret-schema meta-map]}]
  (let [def-sym         (if private 'defn- 'defn)
        name-sym        (symbol (str *ns*) (name name_))
        settings        (merge @config/config meta-map)
        malli-opts      (:aave.core/malli-opts meta-map)
        new-meta        (-> (meta name_)
                            (assoc :doc doc :aave.core/generated true)
                            (merge meta-map))
        {:aave.core/keys [enforce-purity on-purity-fail on-instrument-fail on-outstrument-fail]} settings
        enforce-purity  (if-some [local-purity? (:aave.core/enforce-purity new-meta)] local-purity? enforce-purity)
        params+body     (cond->> params+body
                          (:aave.core/generate-stubs settings)
                          (map-body (fn [_ body]
                                      (if (empty? body)
                                        `((mg/generate ~ret-schema))
                                        body)))

                          (and (:aave.core/instrument settings) param-schema)
                          (map-body (fn [param-syms body]
                                      `((when-some [failure# ((aave.core/param-explainer '~name-sym) [~@param-syms])]
                                          (~on-instrument-fail '~name-sym '[~@param-syms] failure#))
                                        (do ~@body))))

                          (and (:aave.core/outstrument settings) ret-schema)
                          (map-body (fn [_ body]
                                      `((let [result# (do ~@body)]
                                          (if-some [failure# ((aave.core/return-explainer '~name-sym) result#)]
                                            (~on-outstrument-fail '~name-sym failure#)
                                            result#))))))]
    `(do
       (defmethod aave.core/param-schema '~name-sym [_#] ~param-schema)
       (defmethod aave.core/return-schema '~name-sym [_#] ~ret-schema)
       (defmethod aave.core/param-explainer '~name-sym [_#]
         (m/explainer ~param-schema ~malli-opts))
       (defmethod aave.core/return-explainer '~name-sym [_#]
         (m/explainer ~ret-schema ~malli-opts))
       (when (and ~enforce-purity ~(pure? name_) ~(impure? params+body))
         (~on-purity-fail '~name-sym))
       (~def-sym ~name_ ~new-meta
         ~@params+body))))
