#!/usr/bin/env nbb
;; scripts/verify_citations.cljs — check that this catalog's citations still
;; serve the documents they claim, and that the check can tell when they do not.
;;
;;   nbb scripts/verify_citations.cljs             # verify every citation
;;   nbb scripts/verify_citations.cljs --control   # also prove the check discriminates
;;
;; Exit is three-valued on purpose:
;;   0  every citation fetched and carried its probe strings
;;   1  at least one citation did not
;;   2  REFUSED — the run could not measure (no catalog, no entries, host
;;      unreachable). Distinct from 1 because "the check failed" and "the check
;;      never ran" are different facts, and collapsing them is how a workspace
;;      accumulates silence as green.
;;
;; Why probe strings and not the status code: praha.eu and e-sbirka.gov.cz both
;; answer 200 with a JS shell for paths that do not exist (measured 2026-09-10),
;; so a status-only checker reports a rotted citation as healthy forever.

(ns verify-citations
  (:require [clojure.string :as string]
            [cljs.reader :as reader]
            ["fs" :as fs]
            ["process" :as process]))

(def args (vec (drop 2 (js->clj (.-argv process)))))
(def control? (some #{"--control"} args))

(def tx-path "data/datascript-tx.edn")

(defn die! [code & msg]
  (binding [*print-fn* *print-err-fn*] (apply println msg))
  (.exit process code))

(defn entries []
  (when-not (fs/existsSync tx-path)
    (die! 2 "REFUSED:" tx-path "is not here — nothing to verify, which is not the same as nothing wrong."))
  (let [rows (reader/read-string (str (fs/readFileSync tx-path "utf8")))]
    (when-not (sequential? rows)
      (die! 2 "REFUSED:" tx-path "did not read as a sequence of entries."))
    (vec rows)))

(defn charset-of
  "The charset the response declares, lowercased, defaulting to utf-8.

  Not cosmetic: www.psp.cz serves Windows-1250, and decoding it as UTF-8 mangles
  every diacritic — so a probe like \"hlavnim meste Praze\" (with diacritics)
  would report a live citation as dead. Trusting the default here would have the
  checker fail honest sources and, worse, teach the next reader to weaken the
  probe to ASCII until it stopped discriminating."
  [r]
  (let [ct (or (.get (.-headers r) "content-type") "")
        m (re-find #"(?i)charset=[\s\"']*([\w-]+)" ct)]
    (if m (string/lower-case (second m)) "utf-8")))

(defn fetch-body
  "Fetch url, returning {:status n :body s} or {:error msg}.

  Decodes with the charset the server declares rather than assuming UTF-8."
  [url]
  (-> (js/fetch url #js {:redirect "follow"
                         :headers #js {"User-Agent" "cloud-itonami-municipality-cze-prague citation check"}})
      (.then (fn [r]
               (-> (.arrayBuffer r)
                   (.then (fn [buf]
                            (let [cs (charset-of r)
                                  decoded (try
                                            (.decode (js/TextDecoder. cs) buf)
                                            (catch :default _
                                              ;; An unknown charset is a measurement failure, not a
                                              ;; body: say so instead of decoding as UTF-8 anyway.
                                              nil))]
                              (if (nil? decoded)
                                {:error (str "cannot decode charset " cs)}
                                {:status (.-status r) :body decoded :charset cs})))))))
      (.catch (fn [e] {:error (str (.-message e))}))))

(defn probes-present
  "Which of `probe` appear in `body`. Returned as a pair so the caller can name
  the missing ones rather than only saying that something was missing."
  [body probe]
  (let [present (filterv #(string/includes? body %) probe)]
    {:present present :missing (vec (remove (set present) probe))}))

(defn check-one [{:ordinance/keys [id url citation-probe]}]
  (-> (fetch-body url)
      (.then
       (fn [{:keys [status body error]}]
         (cond
           error {:id id :url url :ok? false :why (str "fetch failed: " error)}
           (not (<= 200 status 299)) {:id id :url url :ok? false :why (str "HTTP " status)}
           (empty? citation-probe)
           ;; No probe means this entry cannot be verified. It is NOT a pass.
           {:id id :url url :ok? false :why "no :ordinance/citation-probe — unverifiable, not verified"}
           :else
           (let [{:keys [missing]} (probes-present body citation-probe)]
             (if (seq missing)
               {:id id :url url :ok? false
                :why (str "HTTP " status " but body lacks " (pr-str missing))}
               {:id id :url url :ok? true :why (str "HTTP " status ", all "
                                                    (count citation-probe) " probe(s) present")})))))))

(defn control-cases
  "The declared negative test for each entry: a URL that must NOT satisfy that
  entry's probes.

  Declared by the catalog rather than derived here. An earlier version mutated
  the last path/query segment and, on www.psp.cz, happened to mutate the year --
  a parameter that server ignores -- so the control fetched an equivalent page,
  found the probes present, and would have been read as \"the check is blind\"
  when the real fault was that the mutation was meaningless. A control that
  guesses which part of a URL identifies the document tests the guess."
  [rows]
  (->> rows
       (keep (fn [{:ordinance/keys [id citation-probe citation-control]}]
               (when (and (seq citation-probe) (seq citation-control))
                 {:id id :url citation-control :probe (vec citation-probe)})))
       vec))

(defn check-control [{:keys [id url probe]}]
  (-> (fetch-body url)
      (.then (fn [{:keys [status body error]}]
               (let [satisfied? (and (not error) (<= 200 status 299) body
                                     (empty? (:missing (probes-present body probe))))]
                 {:id id :url url
                  :discriminates? (not satisfied?)
                  :why (cond error (str "fetch failed: " error " (counts as rejected)")
                             satisfied? (str "HTTP " status " AND all probes present — "
                                             "this probe cannot tell the two apart")
                             :else (str "HTTP " status ", probes absent — rejected as it should be"))})))))

(defn main []
  (let [rows (entries)
        n (count rows)]
    (println (str "SCANNED\t" n))
    (when (zero? n)
      (die! 2 "REFUSED: the catalog has no entries. An empty run is not a clean run."))
    (-> (js/Promise.all (clj->js (map check-one rows)))
        (.then
         (fn [results]
           (let [results (js->clj results :keywordize-keys true)
                 bad (remove :ok? results)]
             (doseq [{:keys [id ok? why]} results]
               (println (str (if ok? "  ok   " "  FAIL ") id "  — " why)))
             (-> (if control?
                   (js/Promise.all (clj->js (map check-control (control-cases rows))))
                   (js/Promise.resolve #js []))
                 (.then
                  (fn [ctrls]
                    (let [ctrls (js->clj ctrls :keywordize-keys true)
                          blind (remove :discriminates? ctrls)]
                      (when control?
                        (println "\n── control: each entry's declared near-miss must be rejected ──")
                        (doseq [{:keys [id discriminates? why]} ctrls]
                          (println (str (if discriminates? "  ok   " "  BLIND ") id "  — " why)))
                        (let [undeclared (remove :ordinance/citation-control rows)]
                          (when (seq undeclared)
                            (die! 2 (str "REFUSED: " (count undeclared)
                                         " entr(y/ies) declare no :ordinance/citation-control, "
                                         "so their passes are unproven."))))
                        (when (empty? ctrls)
                          (die! 2 "REFUSED: no control case could be built; the check is unproven.")))
                      (cond
                        (seq blind)
                        (die! 1 (str "\n" (count blind) " entr(y/ies) cannot tell their document "
                                     "from a near-miss. Those passes are not evidence."))
                        (seq bad)
                        (die! 1 (str "\n" (count bad) " of " n " citation(s) no longer serve their document."))
                        :else
                        (println (str "\nall " n " citation(s) verified"
                                      (when control?
                                        (str "; " (count ctrls) " near-miss(es) proved to be rejected")))))))))))))))

(main)
