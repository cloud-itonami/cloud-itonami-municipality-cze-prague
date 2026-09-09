(ns ordinance.facts-test
  (:require [kotoba.lang.text :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [ordinance.facts :as facts]))

(def entries (facts/spec-basis "prague"))

(deftest prague-has-spec-basis
  (is (= 10 (count entries)))
  (is (every? #(str/starts-with? (:ordinance/url %) "https://") entries))
  (is (every? :ordinance/number entries)))

(deftest unknown-municipality-has-no-spec-basis
  (is (nil? (facts/spec-basis "brno")))
  (is (nil? (facts/spec-basis "zzz"))))

(deftest coverage-is-honest
  (let [c (facts/coverage ["prague" "brno"])]
    (is (= 2 (:requested c)))
    (is (= 1 (:covered c)))
    (is (= ["brno"] (:missing-municipalities c)))))

(deftest by-topic-filters
  (is (= 3 (count (facts/by-topic "prague" :governance))))
  (is (= 2 (count (facts/by-topic "prague" :education))))
  (is (= 1 (count (facts/by-topic "prague" :transport))))
  (is (empty? (facts/by-topic "prague" :labor)))
  (is (empty? (facts/by-topic "brno" :governance))))

(deftest ids-are-unique
  ;; :ordinance/id is :db.unique/identity, so a duplicate would not error --
  ;; it would silently overwrite the earlier entry on transact, and the catalog
  ;; would quietly hold fewer ordinances than it lists.
  (is (= (count entries) (count (distinct (map :ordinance/id entries))))))

(deftest urls-are-unique
  ;; Two entries citing one URL means at least one of them is not evidenced by
  ;; the document it points at.
  (is (= (count entries) (count (distinct (map :ordinance/url entries))))))

(deftest every-entry-is-checkable
  ;; An entry with no probe cannot be verified, and `unverifiable-citations`
  ;; exists so that "not checked" never reads as "checked and fine".
  (is (empty? (facts/unverifiable-citations)))
  (is (every? #(seq (:ordinance/citation-probe %)) entries)))

(deftest every-entry-declares-a-near-miss
  ;; The negative test is what makes a pass mean something: without it, a probe
  ;; that matches every page on the host would look identical to one that
  ;; identifies the document.
  (is (every? #(seq (:ordinance/citation-control %)) entries))
  (is (every? #(not= (:ordinance/url %) (:ordinance/citation-control %)) entries)))

(deftest probes-are-specific-enough-to-fail
  ;; A probe that is a substring of its own URL would be satisfied by anything
  ;; the host echoes back, and a blank probe matches everything.
  (is (every? (fn [e]
                (every? #(and (string? %) (>= (count %) 4)) (:ordinance/citation-probe e)))
              entries)))

(deftest prague-regulations-cite-the-statutory-collection
  ;; Prague's own ordinances must come from sbirkapp.gov.cz, the Interior
  ;; Ministry's collection -- not from a search engine's summary of it.
  (let [own (filter #(#{:ordinance :regulation} (:ordinance/kind %)) entries)]
    (is (= 8 (count own)))
    (is (every? #(= :official-sbirkapp-gov-cz (:ordinance/url-provenance %)) own))
    (is (every? #(str/starts-with? (:ordinance/url %) "https://sbirkapp.gov.cz/detail/") own))
    ;; and each must probe for the issuing authority, which is the field that
    ;; separates a Prague ordinance from another municipality's in the same
    ;; collection.
    (is (every? #(some #{"HLAVNÍ MĚSTO PRAHA"} (:ordinance/citation-probe %)) own))))

(deftest dates-are-ordered-and-well-formed
  (let [dated (filter :ordinance/effective-date entries)]
    (is (seq dated))
    (is (every? #(re-matches #"\d{4}-\d{2}-\d{2}" (:ordinance/effective-date %)) dated))
    ;; enacted <= published <= effective, for every entry that states them.
    (is (every? (fn [{:ordinance/keys [enacted-date published-date effective-date]}]
                  (and (or (nil? published-date) (<= (compare enacted-date published-date) 0))
                       (<= (compare (or published-date enacted-date) effective-date) 0)))
                dated))))

(deftest https-only
  (is (true? (facts/https-only?))))

(deftest tx-file-matches-catalog
  ;; data/datascript-tx.edn is generated from the catalog. If someone edits one
  ;; and not the other, the DataScript projection stops describing the source of
  ;; truth -- and nothing else in this repo would notice.
  (let [tx (edn/read-string (slurp (io/file "data/datascript-tx.edn")))
        norm (fn [e] (into (sorted-map)
                           (map (fn [[k v]] [k (if (set? v) (vec (sort v)) v)]) e)))]
    (is (= (mapv norm entries) (mapv norm tx)))))
