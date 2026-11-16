(ns grammar.ref-rename-test
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.impact :as impact]
            [sutra.parser.product :as product]
            [sutra.parser.project-export :as pe]))

(def ^:private golden-file "tests/grammar/ref_rename_golden.edn")

(def every-reference
  {:objects
   {:kunde    {:implements [:buchbar] :backing {:dataset :kunden-roh}
               :external-source {:datasource :erp :table "kunden"}
               :external-sources [{:datasource :crm :table "leads"}]
               :lifecycle {:property :status
                           :transitions [{:from :neu :to :aktiv :action :kunde-aktivieren}]}
               :attributes [[:name :string] [:mail :email-adresse]]}
    :auftrag  {:attributes [[:nr :string]] :project :vertrieb}
    :rechnung {:attributes [[:betrag :decimal]]}}
   :links
   [[:auftrag-kunde :auftrag :kunde :many-to-one
     {:satisfies [{:interface :buchbar :constraint :x}]
      :external-source {:datasource :erp :dataset :links-roh :shared-property :sp}
      :join {:on :kunde-id}}]
    [:rechnung-auftrag :rechnung :auftrag :many-to-one]]
   :interfaces
   {:buchbar {:extends [:basis] :link-constraints [{:link-name :zu :to-object-type :kunde}
                                                   {:link-name :an :to-interface :basis}]}}
   :actions
   {:kunde-aktivieren
    {:on       :kunde
     :function :pruefen
     :params   {:k   {:type :kunde :options-from {:object-type :auftrag}}
                :n   {:type :string}
                :s   {:type "kunde"}}
     :action-log {:object-type :rechnung :project :vertrieb}
     :rules    [{:op :update :object-type :kunde
                 :then [{:op :create-link :link-type :auftrag-kunde}
                        {:op :action :name :rechnung-stellen}]}
                {:op :apply-edit-function :function :pruefen}
                {:op :invoke-webhook :phase :writeback :source :erp :webhook :w}
                {:op :use-llm :model "m" :source :llm}
                {:op :create :object-type :auftrag :values {:type :kunde :on :x}}
                {:op :when :rules [{:op :delete-object :interface :buchbar}]}]
     :edits    [{:object-type :rechnung}]}
    :rechnung-stellen {:rules [{:op :create :object-type :rechnung}]}}
   :transforms
   {:t1 {:inputs  {:a {:kind :dataset :dataset :kunden-roh}
                   :b {:kind :datasource :datasource :erp}
                   :c {:kind :language-model :source :llm}
                   :d {:kind :media-set :media-set :belege}
                   :e {:kind :object-type :object-type :auftrag}
                   :f {:kind :transform :transform :t0}}
         :output  {:kind :object-type :object-type :kunde}
         :parameters {:p {:type :string}}}
    :t2 {:outputs [{:object-type :rechnung} {:link-type :auftrag-kunde} {:dataset :aus}]}}
   :functions
   {:pruefen {:target-type :kunde :language :clojure :source "erp"
              :inputs {:order {:object-type :auftrag}}}}
   :applications
   {:app {:object-type :kunde
          :layout {:export {:object-type :auftrag} :upload {:dataset :kunden-roh}}}}
   :agents
   {:ag {:source :llm :retrieval {:object-type :kunde}
         :tools [{:action :kunde-aktivieren} {:function :pruefen} {:object-type :auftrag}]}}
   :datasets
   {:kunden-roh {:object-type :kunde :virtual {:source :erp :table "k"}
                 :external-source {:datasource :erp :table "k"}}}
   :shared-properties {:sp {:type :string :value-type :email-adresse}}
   :value-types       {:email-adresse {:type :string}}
   :media-sets        {:belege {:source :erp}}
   :exports           {:x {:source :erp :dataset :aus}}
   :pipelines         {:p {:imports [:kunden-roh :kunde] :outputs [:aus]}}
   :object-sets       {:os {:root :kunde :include [{:link :auftrag-kunde}]}}
   :custom-applications {:ca {:scope {:object-types [:kunde] :actions [:kunde-aktivieren]
                                      :functions [:pruefen]}}}
   :templates         {:tp {:parameters {:p {:object-type :kunde :object-set :os}}}}
   :projects          {:vertrieb {:imports [:basis]} :basis {}}
   :workspaces        {:ws {:apps [:app] :home [{:items [{:app :app :object-type :kunde
                                                          :object-set :os}]}]}}
   :sources           {:erp {:type :jdbc} :crm {:type :jdbc} :llm {:type :llm}}})

(def ^:private corpus-roots ["resources" "plugins" "tests/frontend" "media" "tests/support"])

(defn- tracked-texts
  []
  (let [{:keys [exit out err]} (apply sh/sh "git" "ls-files" "-z" "--" corpus-roots)]
    (when-not (zero? exit) (throw (ex-info (str "git ls-files: " err) {:exit exit})))
    (sort (filter #(str/ends-with? % ".ent") (str/split out #"\u0000")))))

(defn corpus
  []
  (into [["every-reference" every-reference]]
        (for [p (tracked-texts)
              :let  [d (edn/read-string {:default (fn [_ v] v)} (slurp p))
                     b (if (map? (:schema d)) (:schema d) d)]
              :when (map? b)]
          [p b])))

(defn changes
  ([a b] (vec (sort-by pr-str (changes [] a b))))
  ([path a b]
   (cond
     (= a b) []
     (and (map? a) (map? b))
     (let [gone  (remove #(contains? b %) (keys a))
           new   (remove #(contains? a %) (keys b))
           kin?  (fn [k k'] (or (str/ends-with? (name k') (str "-" (name k)))
                                (str/starts-with? (name k') (str (name k) "-"))))
           to    (into {} (for [k gone
                                :let [k' (first (filter #(kin? k %) new))]
                                :when k']
                            [k k']))]
       (concat (for [k gone :when (not (to k))] [path :key-gone k])
               (for [k new :when (not ((set (vals to)) k))] [path :key-new k])
               (for [[k k'] to] [path :key k k'])
               (mapcat (fn [[k k']] (changes (conj path k') (get a k) (get b k'))) to)
               (mapcat #(changes (conj path %) (get a %) (get b %))
                       (filter #(contains? b %) (keys a)))))
     (and (sequential? a) (sequential? b) (= (count a) (count b)))
     (mapcat #(changes (conj path %) (nth a %) (nth b %)) (range (count a)))
     :else [[path a b]])))

(defn- object-type-names [body]
  (into (set (keys (:objects body)))
        (mapcat (fn [l] (when (sequential? l) [(nth l 1 nil) (nth l 2 nil)])) (:links body))))

(def ^:private bound-sections
  [:datasets :sources :link-types :actions :functions :interfaces :value-types
   :shared-properties :media-sets])

(defn- held-names
  [body section]
  (let [v (get body (if (= :link-types section) :links section))]
    (cond (map? v)        (keys v)
          (sequential? v) (keep #(when (sequential? %) (first %)) v))))

(defn- renamed [n] (keyword (str "zz-" (name n))))

(defn observe
  [body]
  {:rename-types   (changes body (pe/rename-types body (into {} (map (juxt identity renamed))
                                                                (object-type-names body))))
   :with-prefix    (changes body (product/with-prefix body "pfx"))
   :with-suffix    (changes body (product/with-suffix body "sfx"))
   :bind           (into (sorted-map)
                         (for [s     bound-sections
                               :let  [ns (map name (held-names body s))]
                               :when (seq ns)]
                           [s (changes body (product/bind
                                             body
                                             (into {} (map (fn [n] [n (str "zz-" n)])) ns)
                                             (for [n ns] {:section s :name n})))]))
   :action-targets (into (sorted-map)
                         (for [[k a] (:actions body) :when (map? a)]
                           [k (vec (sort-by pr-str (impact/action-targets a)))]))})

(defn observed [] (into (sorted-map) (for [[p b] (corpus)] [p (observe b)])))

(defn write-golden! []
  (spit golden-file (with-out-str (binding [*print-namespace-maps* false]
                                    (pprint/pprint (observed))))))

(deftest renames-reach-what-they-reached
  (let [want (edn/read-string (slurp golden-file))
        got  (observed)]
    (is (= (set (keys want)) (set (keys got))) "the corpus is the same texts")
    (doseq [[p o] got]
      (testing p
        (doseq [k (keys o)]
          (is (= (get-in want [p k]) (get o k)) (str k)))))))
