(ns aave.code-test
  (:require [aave.code :as sut]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest map-body
  (let [params    (atom [])
        bodies    (atom [])
        transform #(do (swap! params conj %1)
                       (swap! bodies conj %2)
                       '((+ 2 3)))]
    (testing "simple params+body"
      (is (= '([x] (+ 2 3))
             (sut/map-body transform '([x] (+ 1 2)))))
      (is (= '[x] (first @params)))
      (is (= '((+ 1 2)) (first @bodies))))

    (testing "overloaded params+body"
      (reset! bodies [])
      (reset! params [])
      (is (= '(([x] (+ 2 3))
               ([x y] (+ 2 3)))
             (sut/map-body transform '(([x] (+ 1 2))
                                       ([x y] (+ 3 4))))))

      (is (= '[[x]
               [x y]] @params))
      (is (= '[((+ 1 2))
               ((+ 3 4))]
             @bodies)))))
