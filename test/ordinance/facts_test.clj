(ns ordinance.facts-test
  (:require [kotoba.lang.text :as str]
            [clojure.test :refer [deftest is]]
            [ordinance.facts :as facts]))

(deftest prague-has-spec-basis
  (let [sb (facts/spec-basis "prague")]
    (is (= 2 (count sb)))
    (is (every? #(str/starts-with? (:ordinance/url %) "https://") sb))
    (is (every? :ordinance/number sb))))

(deftest unknown-municipality-has-no-spec-basis
  (is (nil? (facts/spec-basis "brno")))
  (is (nil? (facts/spec-basis "zzz"))))

(deftest coverage-is-honest
  (let [c (facts/coverage ["prague" "brno"])]
    (is (= 2 (:requested c)))
    (is (= 1 (:covered c)))
    (is (= ["brno"] (:missing-municipalities c)))))

(deftest by-topic-filters
  (is (= 2 (count (facts/by-topic "prague" :governance))))
  (is (empty? (facts/by-topic "prague" :labor)))
  (is (empty? (facts/by-topic "brno" :governance))))
