(ns ordinance.facts
  "Municipal-ordinance compliance catalog for Prague -- the
  FORTY-FIFTH municipality-level entry (see cloud-itonami-municipality-jpn-tokyo,
  -usa-washington-dc, -gbr-london, -can-toronto, -deu-berlin, -fra-paris,
  -nld-amsterdam, -esp-madrid, -kor-seoul, -ita-roma, -aus-sydney,
  -arg-buenos-aires, -fin-helsinki, -dnk-copenhagen, -nor-oslo,
  -bel-brussels, -chl-santiago, -col-bogota, -cri-san-jose,
  -bra-sao-paulo, -ury-montevideo, -zaf-cape-town, -ecu-quito,
  -swe-gothenburg, -pry-asuncion, -mex-guadalajara, -fra-lyon,
  -ind-new-delhi, -pol-warsaw, -ken-nairobi, -tha-bangkok, -are-abu-dhabi,
  -vnm-hanoi, -idn-jakarta, -phl-manila, -egy-cairo, -tur-ankara,
  -nga-abuja, -sau-riyadh, -mys-kuala-lumpur, -aut-vienna, -che-bern,
  -irl-dublin, -nzl-wellington for the first forty-four) per
  ADR-2607141700 (cloud-itonami-compliance-fact-federation). The
  Czech Republic's first entry across any of the 3 axes.

  Prague is the Czech Republic's stable capital, with no ongoing
  ambiguity.

  Zákon č. 131/2000 Sb., o hlavním městě Praze (Act on the Capital
  City of Prague) -- title, act number, and date directly confirmed
  by reading praha.eu's (Prague's own official domain) hosted PDF
  text via the Read-tool saved-path fallback (WebFetch itself
  reported the PDF as illegible/binary), whose own header reads
  verbatim '131/2000 Sb. — ZÁKON — ze dne 13. dubna 2000 — o hlavním
  městě Praze' (dated 13 April 2000). This resolved a date
  discrepancy: one WebSearch synthesis had claimed '17 May 2000',
  which the directly-read primary source did not confirm -- the
  primary source's own '13. dubna 2000' (13 April 2000) was used
  instead, matching this session's established discipline of
  preferring directly-read primary sources over conflicting secondary
  synthesis.

  Unification of the four historic cities of Prague (Hradčany, Malá
  Strana, Staré Město, Nové Město) under Emperor Joseph II -- directly
  confirmed via en.wikipedia.org's History of Prague article, which
  states verbatim: 'In 1784, under Joseph II, the four municipalities
  of Hradčany, Malá Strana, Staré Město, and Nové Město were merged
  into a single entity.' (year-only, since the specific day was not
  confirmed in this directly-read source).

  An ordinance not in this table has NO spec-basis, full stop; extend
  `catalog`, do not invent an id/url/date.")

(def catalog
  "municipality-slug -> vector of ordinance entries."
  {"prague"
   [{:ordinance/id "prague.act-131-2000-on-the-capital-city-of-prague"
     :ordinance/title "Zákon č. 131/2000 Sb., o hlavním městě Praze (Act on the Capital City of Prague)"
     :ordinance/municipality "prague"
     :ordinance/country "CZE"
     :ordinance/kind :local-act
     :ordinance/number "131/2000 Sb."
     :ordinance/url "https://praha.eu/documents/d/praha/Zakon_o_hl_meste_Praze_1828311"
     :ordinance/url-provenance :official-praha-eu
     :ordinance/enacted-date "2000-04-13"
     :ordinance/retrieved-at "2026-07-17"
     :ordinance/topic #{:governance}}
    {:ordinance/id "prague.1784-unification-four-cities"
     :ordinance/title "Unification of the four historic cities of Prague (Hradčany, Malá Strana, Staré Město, Nové Město) under Joseph II"
     :ordinance/municipality "prague"
     :ordinance/country "CZE"
     :ordinance/kind :local-act
     :ordinance/number "1784"
     :ordinance/url "https://en.wikipedia.org/wiki/History_of_Prague"
     :ordinance/url-provenance :wikipedia-corroborated
     :ordinance/enacted-date "1784"
     :ordinance/retrieved-at "2026-07-17"
     :ordinance/topic #{:governance}}]})

(defn spec-basis [muni] (get catalog muni))

(defn coverage
  ([] (coverage (keys catalog)))
  ([munis]
   (let [have (filter catalog munis)
         missing (remove catalog munis)]
     {:requested (count munis)
      :covered (count have)
      :covered-municipalities (vec (sort have))
      :missing-municipalities (vec (sort missing))
      :note (str "cloud-itonami-municipality-cze-prague Wave 0 (ADR-2607141700): "
                 (count (get catalog "prague")) " Prague entries seeded "
                 "with praha.eu/Wikipedia citations. "
                 "Extend `ordinance.facts/catalog`, never fabricate an id/url.")})))

(defn by-topic [muni topic]
  (filterv #(contains? (:ordinance/topic %) topic) (spec-basis muni)))
