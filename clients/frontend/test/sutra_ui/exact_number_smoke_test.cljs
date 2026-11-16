(ns sutra-ui.exact-number-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [goog.math.Long]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]
            [sutra.capps.variables :as vars]
            [sutra-ui.apphost.interop :as interop]
            [sutra.types.decimal :as d]
            [sutra.wire.codec :as codec]))

(def ^:private de (cctx/of {:locale :de}))

(def ^:private long-decimal "123456789012345678901234.56789")

(def ^:private payload
  (str "[\"^ \",\"~:betrag\",\"~f" long-decimal "\""
       ",\"~:preis\",\"~f1234.50\""
       ",\"~:gross\",\"~i9007199254740993\""
       ",\"~:safe\",\"~i9007199254740991\""
       ",\"~:zaehler\",\"~n12345678901234567890\""
       ",\"~:klein\",5"
       ",\"~:auftrag-nr\",\"4711\""
       ",\"~:label\",\"Montage\"]"))

(def ^:private decoded (codec/decode payload))

(deftest exact-values-survive-the-wire
  (testing "a NUMERIC a double cannot carry arrives EXACT, not truncated"
    (is (d/exact? (:betrag decoded)))
    (is (= long-decimal (d/plain (:betrag decoded))))
    (is (not= 1.2345678901234568e23 (:betrag decoded))
        "the old parseFloat handler answered exactly this, in silence"))
  (testing "a value a double DOES carry stays a real number — arithmetic, React
            children and ReactFlow keep working for every ordinary payload"
    (is (number? (:preis decoded)))
    (is (= 1234.5 (:preis decoded)))
    (is (number? (:klein decoded)))
    (is (= 5 (:klein decoded))))
  (testing "a bigint keeps every digit"
    (is (d/exact? (:zaehler decoded)))
    (is (= "12345678901234567890" (d/plain (:zaehler decoded))))))

(deftest no-goog-long-reaches-app-code
  (testing "past ±(2^53−1) transit-js reaches for goog.math.Long — the `~i`
            handler must take that value before app code sees it"
    (is (not (instance? goog.math.Long (:gross decoded)))
        "a goog.math.Long is an OBJECT: `(+ x 1)` on it silently stringifies")
    (is (d/exact? (:gross decoded)))
    (is (= "9007199254740993" (d/plain (:gross decoded))))
    (is (= "9007199254740993" (str (:gross decoded)))))
  (testing "at the boundary itself a plain number is still right"
    (is (number? (:safe decoded)))
    (is (= 9007199254740991 (:safe decoded)))))

(deftest an-exact-value-reaches-the-display-unbent
  (testing "29 digits in, 29 digits out"
    (is (= "123.456.789.012.345.678.901.234,56789" (fmt/fmt (:betrag decoded) 5 de)))
    (is (= "123.456.789.012.345.678.901.234,57 €"  (fmt/eur (:betrag decoded) de)))
    (is (= "123.456.789.012.345.678.901.234,568"   (fmt/menge (:betrag decoded) de)))
    (is (= "123.456.789.012.345.678.901.234,568"   (fmt/auto (:betrag decoded) de))))
  (testing "an integer past 2^53 keeps its last digit"
    (is (= "9007199254740993" (fmt/auto (:gross decoded) de)))
    (is (= "9.007.199.254.740.993,00 €" (fmt/eur (:gross decoded) de))))
  (testing "`num` hands the carrier on rather than parsing it away"
    (is (d/exact? (fmt/num long-decimal)))
    (is (= "123.456.789.012.345.678.901.234,56789" (fmt/fmt long-decimal 5 de)))))

(deftest the-ordinary-numbers-are-unchanged
  (is (= "1.234,50 €" (fmt/eur 1234.5 de)))
  (is (= "1.235 €"    (fmt/eur0 1234.5 de)))
  (is (= "1.234,5 €"  (fmt/eur1 1234.5 de)))
  (is (= "12,5 %"     (fmt/prozent 12.5 de)))
  (is (= "1.234,57"   (fmt/fmt 1234.5678 2 de)))
  (is (= "–"          (fmt/fmt nil 2 de)))
  (is (= "0,00"       (fmt/fmt 8.881784197001252e-15 2 de)))
  (is (= "1.234,5"    (fmt/menge 1234.5 de)))
  (is (= "1.000"      (fmt/menge 1000 de)))
  (is (= "0,001"      (fmt/menge 0.0005 de)))
  (is (= "2024"       (fmt/auto 2024 de)))
  (is (= "2024"       (fmt/auto (d/exact "2024.00") de)))
  (is (= "1,5"        (fmt/auto 1.5 de)))
  (is (= "0"          (fmt/auto 1e-14 de)))
  (is (= "Ja"         (fmt/auto true de)))
  (is (nil?           (fmt/auto nil de))))

(deftest the-digits-round-the-same-way-on-both-hosts
  (is (= "1234.50" (d/round-half-up "1234.5" 2)))
  (is (= "0.01"    (d/round-half-up "0.005" 2)))
  (is (= "10.0"    (d/round-half-up "9.99" 1)))
  (is (= "1"       (d/round-half-up "0.9" 0)))
  (is (= "1.01"    (d/round-half-up "1.005" 2)) "a tie goes AWAY FROM ZERO")
  (is (= "2.68"    (d/round-half-up "2.675" 2)))
  (is (= "123456789012345678901234.56789" (d/round-half-up long-decimal 5)))
  (is (= "123456789012345678901234.5679"  (d/round-half-up long-decimal 4)))
  (testing "an exponent from either host's `str` expands to plain digits"
    (is (= "1000000000000000000000" (d/plain 1e21)))
    (is (= "0.00001" (d/plain 1.0e-5)))
    (is (= "1" (d/plain 1.0)))
    (is (= "0.000000000000008881784197001252" (d/plain 8.881784197001252e-15)))))

(deftest calc-is-the-only-door-to-arithmetic
  (testing "JS has no exact arithmetic, so `calc` is where the tail goes — named,
            local, and never at the wire"
    (is (number? (fmt/calc (:betrag decoded))))
    (is (= 1.2345678901234568e23 (fmt/calc (:betrag decoded))))
    (is (= 5 (fmt/calc 5)))
    (is (nil? (fmt/calc nil))))
  (testing "an `:expr` over exact values still folds instead of throwing"
    (is (= 2 (vars/eval-expr [:/ (d/exact "10") (d/exact "5")] identity)))
    (is (= 1.2345678901234568e23 (vars/eval-expr [:+ (:betrag decoded) 0] identity))))
  (testing "a `:when` reads an exact value as the VALUE, not as a bare `true`"
    (is (true? (vars/eval-pred [:> (:betrag decoded) 1])))
    (is (false? (vars/eval-pred [:> (:preis decoded) (:betrag decoded)])))))

(def ^:private property-types
  {:messung {:betrag :decimal :preis :decimal :gross :long :safe :long
             :zaehler :long :klein :long :auftrag-nr :string :label :string}})

(def ^:private json-payload
  (str "{\"application\":{\"id\":\"ri.app.1\",\"property-types\":{\"messung\":{"
       "\"betrag\":\"decimal\",\"preis\":\"decimal\",\"gross\":\"long\""
       ",\"safe\":\"long\",\"zaehler\":\"long\",\"klein\":\"long\""
       ",\"auftrag-nr\":\"string\",\"label\":\"string\"}}}"
       ",\"data\":{\"messung\":[{\"id\":\"m1\",\"values\":{"
       "\"betrag\":\"" long-decimal "\""
       ",\"preis\":\"1234.50\""
       ",\"gross\":\"9007199254740993\""
       ",\"safe\":\"9007199254740991\""
       ",\"zaehler\":\"12345678901234567890\""
       ",\"klein\":\"5\""
       ",\"auftrag-nr\":\"4711\""
       ",\"label\":\"Montage\"}}]}}"))

(def ^:private json-decoded
  (-> (interop/decode-json json-payload) interop/->clj
      (get-in [:data :messung 0 :values])))

(defn- json-value
  [t literal]
  (-> (interop/decode-json
       (str "{\"data\":{\"messung\":[{\"id\":\"x\",\"values\":{\"v\":" literal "}}]}}")
       {:messung {:v t}})
      interop/->clj
      (get-in [:data :messung 0 :values :v])))

(deftest both-wires-deliver-the-same-value-in-the-same-form
  (testing "THE acceptance condition: the transit payload and the JSON payload
            decode EQUAL, field for field"
    (is (= decoded json-decoded)))
  (testing "so every widget sees the same characters, whichever wire fed it"
    (doseq [k [:betrag :preis :gross :safe :zaehler :klein :auftrag-nr :label]]
      (is (= (fmt/auto (k decoded) de) (fmt/auto (k json-decoded) de)) (str k " · auto"))
      (is (= (fmt/eur (k decoded) de)  (fmt/eur (k json-decoded) de))  (str k " · eur"))
      (is (= (fmt/menge (k decoded) de) (fmt/menge (k json-decoded) de)) (str k " · menge"))))
  (testing "and a `:where` matches on both wires or on neither"
    (doseq [k [:betrag :preis :gross :safe :zaehler :klein :auftrag-nr :label]]
      (is (true? ((:= vars/where-ops) (k decoded) (k json-decoded))) (str k))))
  (testing "the exact ones stay exact on the JSON wire too — a string never
            reaches a view where the portal hands it a carrier"
    (is (d/exact? (:betrag json-decoded)))
    (is (= long-decimal (d/plain (:betrag json-decoded))))
    (is (d/exact? (:gross json-decoded)))
    (is (= "9007199254740993" (d/plain (:gross json-decoded))))
    (is (number? (:preis json-decoded)))
    (is (= 1234.5 (:preis json-decoded)))
    (is (= 5 (:klein json-decoded)))))

(deftest the-descriptor-carries-the-same-table-the-payload-ships
  (testing "an app-open GET ships the type table WITH the rows and types itself;
            `submit!`/`aggregate!`, which answer without a descriptor, get it
            from the one they were opened with. Both must decode identically, or
            a value would change form between a read and a write"
    (let [rows-only (str "{\"data\":{\"messung\":[{\"id\":\"m1\",\"values\":{"
                         "\"betrag\":\"" long-decimal "\""
                         ",\"preis\":\"1234.50\""
                         ",\"gross\":\"9007199254740993\""
                         ",\"safe\":\"9007199254740991\""
                         ",\"zaehler\":\"12345678901234567890\""
                         ",\"klein\":\"5\""
                         ",\"auftrag-nr\":\"4711\""
                         ",\"label\":\"Montage\"}}]}}")]
      (is (= json-decoded
             (-> (interop/decode-json rows-only property-types) interop/->clj
                 (get-in [:data :messung 0 :values])))))))

(deftest a-string-column-holding-digits-stays-a-string-on-both-wires
  (testing "the case a content test cannot decide, and the whole reason the door
            reads the schema: `:auftrag-nr` is declared `:string` and holds
            `4711`. Over transit the type rides along and it is a string; over
            JSON `json-by-type` writes it exactly as it writes a Long — and only
            the declared type tells them apart"
    (is (string? (:auftrag-nr decoded)))
    (is (string? (:auftrag-nr json-decoded))
        "shape-recognized, this became the number 4711 and the two wires split")
    (is (= "4711" (:auftrag-nr json-decoded)))
    (is (= (:auftrag-nr decoded) (:auftrag-nr json-decoded))))
  (testing "…while its `:long` neighbour holding the SAME kind of digits IS a
            number, from the same body, decided by nothing but the declaration"
    (is (= 5 (:klein json-decoded)))
    (is (= 9007199254740991 (:safe json-decoded))))
  (testing "a `:string` column takes no lift whatever its content spells"
    (doseq [s ["4711" "40.0000000000" "0" "-12.5" "58319870951433"
               "007" "1.2.3" "1e5" "+5" "12 345" "K-2024-0815" "2026-W32"]]
      (is (= s (json-value :string (js/JSON.stringify s))) (pr-str s)))))

(deftest the-four-shapes-that-reached-a-widget-raw
  (testing "a DECIMAL(38,10) displayed its stored scale instead of its value"
    (is (= "40" (fmt/auto (json-value :decimal "\"40.0000000000\"") de))
        "before the door: \"40.0000000000\", straight through `(str x)`")
    (is (= "40" (fmt/auto (d/carry "40.0000000000") de))
        "…which is what the transit wire has always shown"))
  (testing "an ordinary NUMERIC was shown unformatted"
    (is (= "1.234,56" (fmt/auto (json-value :decimal "\"1234.56\"") de))))
  (testing "a `:=` against a number matched NEVER — the table read empty"
    (is (true? ((:= vars/where-ops) (json-value :decimal "\"40.0000000000\"") 40)))
    (is (true? ((:= vars/where-ops) (json-value :long "\"58319870951433\"") 58319870951433))))
  (testing "`:falsy` read a zero as truthy"
    (is (true? (boolean ((:falsy vars/where-ops) (json-value :decimal "\"0.0000000000\"") nil))))
    (is (false? (boolean ((:truthy vars/where-ops) (json-value :long "\"0\"") nil))))))

(deftest what-carries-no-type-is-left-exactly-as-it-came
  (testing "a numeric column whose content spells no number is NOT forced —
            `carry` answers nil and the string stands"
    (doseq [s ["1.2.3" "12 345" "1,5" "0x10" "" "K-2024-0815" "2026-W32"
               "40.0000000000 €" "6f1c2e1a-1b2c-4d5e-8f90-0123456789ab"]]
      (is (= s (json-value :decimal (js/JSON.stringify s))) (pr-str s))))
  (testing "…but a number the OLD shape gate refused is now read, because the
            column says it is one. `1e5` and `+5` are shapes `.toPlainString`
            never writes, which is why a shape test had to reject them; a type
            test has no reason to — and `sutra.types.decimal/plain` reads exactly what
            the transit side reads"
    (is (= 100000 (json-value :decimal "\"1e5\"")))
    (is (= 5 (json-value :decimal "\"+5\""))))
  (testing "a property the table does not name at all keeps its form — no type,
            no change, which is the conservative half of the rule"
    (is
     (= "58319870951433"
        (-> (interop/decode-json
             "{\"data\":{\"messung\":[{\"id\":\"x\",\"values\":{\"fremd\":\"58319870951433\"}}]}}"
             {:messung {:v :long}})
            interop/->clj (get-in [:data :messung 0 :values :fremd])))))
  (testing "a body with NO type table is handed back untouched — an app opened
            against a server that ships none decodes nothing rather than guessing"
    (is (= {:label "Montage" :aktiv true :rest nil :tags ["a" "b"] :nr "4711"}
           (interop/->clj
            (interop/decode-json
             (str "{\"label\":\"Montage\",\"aktiv\":true,\"rest\":null"
                  ",\"tags\":[\"a\",\"b\"],\"nr\":\"4711\"}"))))))
  (testing "a bare JSON number is a Double/Integer by `json-by-type`'s own rule
            and needs nothing done to it; an empty body is nil, not a thrown parse"
    (is (= 3.14159265 (json-value :double "3.14159265")))
    (is (= 238940 (json-value :integer "238940")))
    (is (nil? (interop/decode-json "")))
    (is (nil? (interop/decode-json nil)))))

(def ^:private app-types
  {:zeitbuchung {:stunden :decimal :beleg-nr :string :kw :string}
   "ri.ot.zeitbuchung" {:stunden :decimal :beleg-nr :string}})

(deftest the-door-reaches-every-position-an-app-response-has
  (testing "`:data`, keyed by type, each row `{:id :values}`"
    (let [body (-> (interop/decode-json
                    (str "{\"data\":{\"zeitbuchung\":["
                         "{\"id\":\"a-1\",\"values\":{\"stunden\":\"7.5000000000\",\"beleg-nr\":\"4711\"}}"
                         ",{\"id\":\"a-2\",\"values\":{\"stunden\":\"" long-decimal "\"}}]}}")
                    app-types)
                   interop/->clj)]
      (is (= 7.5 (get-in body [:data :zeitbuchung 0 :values :stunden])))
      (is (= "a-1" (get-in body [:data :zeitbuchung 0 :id])))
      (is (= "4711" (get-in body [:data :zeitbuchung 0 :values :beleg-nr]))
          "a `:string` beside a `:decimal`, in the same row")
      (is (= long-decimal (d/plain (get-in body [:data :zeitbuchung 1 :values :stunden]))))))
  (testing "`:objects` — the singular bind, where only the ROW names its type,
            and it names it by RID"
    (let [body (-> (interop/decode-json
                    (str "{\"objects\":[{\"id\":\"a-1\",\"object-type-ref\":\"ri.ot.zeitbuchung\""
                         ",\"values\":{\"stunden\":\"7.5000000000\",\"beleg-nr\":\"4711\"}}]}")
                    app-types)
                   interop/->clj)]
      (is (= 7.5 (get-in body [:objects 0 :values :stunden])))
      (is (= "4711" (get-in body [:objects 0 :values :beleg-nr])))))
  (testing "an aggregate read: a DIMENSION is a value of the column it groups and
            is decoded like any property, a METRIC is a number by the aggregate
            vocabulary and the response says which keys those are"
    (let [body (-> (interop/decode-json
                    (str "{\"object-type\":\"zeitbuchung\""
                         ",\"keys\":{\"group\":[\"kw\"],\"metrics\":[\"total\"]}"
                         ",\"aggregations\":[{\"group\":{\"kw\":\"2026-W32\"}"
                         ",\"metrics\":{\"total\":\"9007199254740993\",\"count\":\"12\"}}]}")
                    app-types)
                   interop/->clj)]
      (is (= "2026-W32" (get-in body [:aggregations 0 :group :kw]))
          "a bucket label starts with a digit and must survive")
      (is (= "9007199254740993" (d/plain (get-in body [:aggregations 0 :metrics :total]))))
      (is (= 12 (get-in body [:aggregations 0 :metrics :count])))))
  (testing "a function read: `{:object-type :rows}`, the rows of a declared type"
    (let [body (-> (interop/decode-json
                    (str "{\"function\":\"scoring\",\"object-type\":\"zeitbuchung\""
                         ",\"rows\":[{\"id\":\"a-1\",\"values\":{\"stunden\":\"7.5000000000\""
                         ",\"beleg-nr\":\"4711\"}}]}")
                    app-types)
                   interop/->clj)]
      (is (= 7.5 (get-in body [:rows 0 :values :stunden])))
      (is (= "4711" (get-in body [:rows 0 :values :beleg-nr])))))
  (testing "a series read: `[label value]` pairs — the label is a bucket, the
            value a measurement (the property's own column holds a series ID)"
    (let [body (-> (interop/decode-json
                    (str "{\"object-type\":\"kostenstelle\",\"property\":\"produktivstunden\""
                         ",\"series\":[{\"series-id\":\"kst-4711\""
                         ",\"points\":[[\"2026-W32\",\"7.5000000000\"],[\"2026-W33\",\"9007199254740993\"]]}]}")
                    app-types)
                   interop/->clj)]
      (is (= "2026-W32" (get-in body [:series 0 :points 0 0])))
      (is (= 7.5 (get-in body [:series 0 :points 0 1])))
      (is (= "9007199254740993" (d/plain (get-in body [:series 0 :points 1 1])))))))

(deftest what-goes-back-up-is-what-the-server-parsed-before
  (testing "an exact carrier leaves as its DIGIT STRING — `JSON.stringify` writes
            the carrier's FIELDS otherwise (`{\"s\":\"…\"}`), and a `submit!`
            param declared scalar arrived as an object"
    (is (= (str "{\"betrag\":\"" long-decimal "\"}")
           (js/JSON.stringify (interop/->js {:betrag (:betrag json-decoded)}))))
    (is (= (str "{\"betrag\":\"" long-decimal "\"}")
           (js/JSON.stringify (interop/->js {:betrag (:betrag decoded)})))
        "the transit wire could already put a carrier here — same body either way")
    (is (= "{\"gross\":\"9007199254740993\"}"
           (js/JSON.stringify (interop/->js {:gross (:gross json-decoded)})))))
  (testing "a value a double carries goes up as that number, which is the shape
            the endpoint read before the JSON wire started stringing exact types"
    (is (= "{\"preis\":1234.5}"
           (js/JSON.stringify (interop/->js {:preis (:preis json-decoded)}))))
    (is (= (js/JSON.stringify (interop/->js {:preis (:preis decoded)}))
           (js/JSON.stringify (interop/->js {:preis (:preis json-decoded)})))))
  (testing "everything else the SDK sends is unchanged"
    (is (= "{\"parameters\":{\"jahr\":2026,\"art\":\"P\",\"ok\":true}}"
           (js/JSON.stringify (interop/->js {:parameters {:jahr 2026 :art "P" :ok true}}))))))
