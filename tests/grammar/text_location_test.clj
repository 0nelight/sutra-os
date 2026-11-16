(ns ^:unit grammar.text-location-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.projection :as projection]
            [sutra.cli.text-location :as loc]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-errors :as ent-errors]))

(def ^:private text
  (str "{:full\n"
       " {:items\n"
       "  [{:body {:kind :rows},\n"
       "    :kind :section,\n"
       "    :title \"Beleg\"}\n"
       "   {:items\n"
       "    [{:widget :badge,\n"
       "      :tippfehler 1}],\n"
       "    :kind :loop}]}}\n"))

(deftest a-path-becomes-a-line-and-words
  (is (= {:line 8 :trail ["full" "loop" "badge" "tippfehler"]}
         (loc/locate text [:full :items 1 :items 0 :tippfehler])))
  (is (= ["full" "Beleg" "rows"] (:trail (loc/locate text [:full :items 0 :body]))))
  (testing "a path the text does not hold ends at the deepest node it does"
    (is (= 6 (:line (loc/locate text [:full :items 1 :nix :tief]))))
    (is (= 2 (:line (loc/locate text [:full :items])))))
  (is (nil? (loc/locate "{:offen" [:a])) "an unreadable text answers nothing"))

(deftest a-server-line-becomes-problems
  (is (= [{:path [:full :items 1 :tippfehler] :reason "disallowed key"}]
         (loc/problems-of "full > items > 1 > tippfehler — disallowed key")))
  (is (= [{:path [:full :items 0] :reason "A"} {:path [:panel :text] :reason "B"}]
         (loc/problems-of "<root> — A — bei [:full :items 0] · B — bei [:panel :text]")))
  (is (= [{:path [] :reason "nur Worte"}] (loc/problems-of "<root> — nur Worte")))
  (is (= [{:path [] :reason "ganz frei"}] (loc/problems-of "ganz frei"))))

(deftest a-problem-reads-with-its-place
  (is (= "x.entv:8 full › loop › badge › tippfehler — disallowed key"
         (loc/line-for "x.entv" text {:path [:full :items 1 :items 0 :tippfehler]
                                      :reason "disallowed key"})))
  (is (= "x.entv — frei" (loc/line-for "x.entv" nil {:path [:a] :reason "frei"}))))

(deftest a-check-on-the-root-is-no-field-called-error
  (is (= ["<root> — A"] (ent-errors/humanize->lines {:malli/error ["A"]})))
  (is (= ["a > b — B" "a — A"]
         (ent-errors/humanize->lines {:a {:malli/error ["A"] :b ["B"]}}))))

(deftest an-unknown-widget-names-what-it-could-be
  (let [e (try (ent/parse-entv {:object-type :beleg :full {:widget :buton}} "x.entv")
               (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (re-find #"unknown :widget :buton — meant :?button" e))
    (is (re-find #"one of: .*action-button" e))
    (is (not (re-find #"invalid dispatch" e)))))

(deftest a-refusal-says-each-problem-once-with-its-place
  (let [out (with-out-str
              (binding [*err* *out*]
               (#'projection/failed!
                {:status 400
                 :body {:error (str "Invalid .entv (x.entv): 1 problem(s) — each is `path — "
                                    "reason`:\n  • full > items > 1 — disallowed key\n(hint)")
                        :data {:errors {:full {:items [nil ["disallowed key"]]}}}}}
                {"x.entv" text})))]
    (is (re-find #"✗ Invalid \.entv \(x\.entv\):" out))
    (is (re-find #"· x\.entv:6 full › loop — disallowed key" out))
    (is (= 1 (count (re-seq #"full" out))) "no second copy of the problem")))
