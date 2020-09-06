(ns aave.config
  "Namespace for configuring aave")

(def default
  "The default settings used by aave"
  {:aave.core/generate-stubs             true
   :aave.core/outstrument                true
   :aave.core/instrument                 true
   :aave.core/enforce-purity             true
   :aave.core/malli-opts                 {}
   :aave.core/on-purity-fail             'aave.code/on-purity-fail!
   :aave.core/on-instrument-fail         'aave.code/on-instrument-fail!
   :aave.core/on-outstrument-fail        'aave.code/on-outstrument-fail!})

(def config
  "Configuration atom for aave"
  (atom default))

(defn set!
  "Sets aave's config to the new config"
  [new-config]
  (reset! config new-config))
