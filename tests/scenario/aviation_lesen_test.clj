(ns ^:api scenario.aviation-lesen-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.access.principals :as pr]
            [support.aviation :refer [kanten-aus ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.ref.objectset :as ros]
            [sutra.ref.tokens :as tokens]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(def ^:private leser
  (atom nil))

(def ^:private mit-zugriff
  (-> ontologie
      (update :objects merge {:user {:attributes [[:user-id :string] [:rolle :string]]
                                     :primary-key :user-id :plural "Benutzer"}
                              :marking {:attributes [[:name :string]] :match-keys [:name]
                                        :primary-key :name :plural "Marken"}})
      (update :links conj (kanten-aus [:cleared-for :user :marking :many-to-many]
                                       :verzeichnisdienst "SICHERHEITSFREIGABE"))
      (update :actions merge
              {:benutzer-anlegen {:params {:id {:name "Id" :type :string :required true}
                                           :rolle {:name "Rolle" :type :string :required true}}
                                  :rules [{:op :create :object-type :user :id :id
                                           :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                                  :edits [{:object-type :user}]}
               :marking-anlegen {:params {:name {:name "Name" :type :string :required true}}
                                 :rules [{:op :create :object-type :marking
                                          :values {:name [:get :name]}}]
                                 :edits [{:object-type :marking}]}})))

(defn- mit-ontologie [f]
  (assert (ok? (install-in-project {:schema mit-zugriff})))
  (assert (ok? (api/post! :staging)))
  (assert (ok? (api/mutate :marking-anlegen {:name "vs"} {:publish? true})))
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (api/mutate :benutzer-anlegen {:id id :rolle "ontology-viewer"} {:publish? true})))
    (assert (ok? (api/post! :register {:username "leser" :password "leser" :user-id id}))))
  (reset! leser (-> (api/post! :login {:username "leser" :password "leser"}) :body :token))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-ontologie)

(def ^:private kennzeichen ["D-AAAA" "D-AAAB" "D-AAAC"])

(def ^:private anfang
  [[:luftfahrzeug-registrieren {:kennzeichen "D-AAAA" :muster "A320"}]
   [:luftfahrzeug-registrieren {:kennzeichen "D-AAAB" :muster "B748"}]
   [:luftfahrzeug-registrieren {:kennzeichen "D-AAAC" :muster "a320"}]])

(def ^:private flotte
  (let [kz  (gen/elements kennzeichen)
        nr  (gen/elements ["M-1" "M-2" "M-3" "M-4" "M-5" "M-6"])
        txt (gen/elements ["Leck" "leck links" "LECK" "100% Druck" "a_b" "Öl" "Ölstand" "z"])
        tag (gen/elements [nil "2025-12-29" "2026-01-01" "2026-03-31" "2026-08-03" "2026-08-09"])
        h   (gen/elements [nil 0M 0.5M 99.999M 1200.10M 1000M])]
    (gen/vector
     (gen/frequency
      [[2 (gen/fmap (fn [[k m]] [:luftfahrzeug-registrieren {:kennzeichen k :muster m}])
                    (gen/tuple kz (gen/elements ["A320" "B748" "a320"])))]
       [5 (gen/fmap (fn [[n k b kat d]] [:mangel-melden (cond-> {:mangel-nr n :luftfahrzeug k
                                                                 :beschreibung b :kategorie kat}
                                                          d (assoc :gemeldet-am d))])
                    (gen/tuple nr kz txt (gen/elements ["A" "B" "C"]) tag))]
       [3
        (gen/fmap (fn [[s k stunden egt]]
                    [:triebwerk-einbauen (cond-> {:seriennummer s :luftfahrzeug k}
                                           stunden  (assoc :betriebsstunden stunden)
                                           egt (assoc :egt-marge egt))])
                  (gen/tuple (gen/elements ["E-1" "E-2" "E-3" "E-4"]) kz h
                             (gen/elements [nil -7M -5M -0.5M 0M 12.5M 99M])))]
       [1 (gen/fmap (fn [ns] [:maengel-zurueckstellen {:maengel ns}]) (gen/vector nr 0 2))]
       [1 (gen/fmap (fn [[s k l]] [:fahrwerk-einbauen (cond-> {:seriennummer s :luftfahrzeug k}
                                                        l (assoc :landungen l))])
                    (gen/tuple (gen/elements ["F-1" "F-2" "E-1"]) kz (gen/elements [nil 0 4000])))]
       [1 (gen/fmap (fn [[l m]] [:mechaniker-anlegen {:lizenz l :name l :muster m}])
                    (gen/tuple (gen/elements ["L-1" "L-2" "L-3"]) (gen/elements ["A320" "B748"])))]
       [3 (gen/fmap (fn [[l k]] [:berechtigen {:mechaniker l :luftfahrzeug k}])
                    (gen/tuple (gen/elements ["L-1" "L-2" "L-3"]) kz))]
       [1 (gen/fmap (fn [[l k]] [:berechtigung-entziehen {:mechaniker l :luftfahrzeug k}])
                    (gen/tuple (gen/elements ["L-1" "L-2" "L-3"]) kz))]])
     10 28)))

(def ^:private attrs
  {:luftfahrzeug {:kennzeichen :text :muster :text :status :text}
   :mechaniker   {:lizenz :text :name :text :muster :text}
   :wartbares-teil {:seriennummer :text :betriebsstunden :num}
   :mangel       {:mangel-nr :text :kennzeichen :text :beschreibung :text :kategorie :text
                  :zustand :text :gemeldet-am :date :behoben-von :text}
   :triebwerk    {:seriennummer :text :betriebsstunden :num :kennzeichen :text :egt-marge :num}})

(def ^:private spruenge
  {:luftfahrzeug   [[:luftfahrzeug :mangel] [:eingebaut-in :triebwerk]
                    [:berechtigt-fuer :mechaniker]]
   :mangel         [[:maengel :luftfahrzeug]]
   :triebwerk      [[:triebwerke :luftfahrzeug]]
   :mechaniker     [[:berechtigte :luftfahrzeug]]
   :wartbares-teil []})

(def ^:private abgehend
  {:luftfahrzeug   [[:maengel :mangel] [:triebwerke :triebwerk] [:berechtigte :mechaniker]]
   :mangel         [[:luftfahrzeug :luftfahrzeug]]
   :triebwerk      [[:eingebaut-in :luftfahrzeug]]
   :mechaniker     [[:berechtigt-fuer :luftfahrzeug]]
   :wartbares-teil []})

(def ^:private text-ops [:= :not= :< :> :<= :>= :starts-with :contains :contains-ci])
(def ^:private ord-ops [:= :not= :< :> :<= :>=])

(defn- praedikat
  [werte T]
  (let [vergleich
        (gen/bind (gen/elements (keys (attrs T)))
                  (fn [a]
                    (let [vs  (remove nil? (get-in werte [T a]))
                          txt (concat vs ["a" "A" "Leck" "leck" "%" "_" "Ö" ""])]
                      (gen/one-of
                       (cond-> [(gen/fmap (fn [b] [:nil? a b]) gen/boolean)]
                         (seq vs)
                         (conj (gen/tuple (gen/elements ord-ops) (gen/return a)
                                          (gen/elements vs)))
                         (= :text (get-in attrs [T a]))
                         (conj (gen/tuple (gen/elements text-ops) (gen/return a)
                                          (gen/elements txt))))))))]
    (gen/recursive-gen
     (fn [inner] (gen/one-of [(gen/fmap #(into [:and] %) (gen/vector inner 0 2))
                              (gen/fmap #(into [:or] %) (gen/vector inner 0 2))]))
     (gen/one-of [vergleich
                  (gen/fmap (fn [ids] [:id-in (vec ids)])
                            (gen/vector (gen/elements (concat (get-in werte [T :id]) ["nicht-da"]))
                                        0 3))]))))

(defn- menge
  [rids werte T tiefe]
  (let [blatt (gen/one-of
               [(gen/return [{:op :scan :root (rids T)} T])
                (gen/fmap (fn [ids] [{:op :static :ids (vec ids) :root (rids T)} T])
                          (gen/vector (gen/elements (concat (get-in werte [T :id]) ["nicht-da"])) 0
                                      3))])]
    (if (zero? tiefe)
      blatt
      (let [tiefer #(menge rids werte % (dec tiefe))]
        (gen/frequency
         [[2 blatt]
          [3 (gen/fmap (fn [[[in t] p]] [{:op :filter :where p :in in} t])
                       (gen/tuple (tiefer T) (praedikat werte T)))]
          [(if (seq (spruenge T)) 2 0)
           (gen/bind (gen/elements (or (seq (spruenge T)) [nil]))
                     (fn [[seite von]]
                       (gen/fmap (fn [[in _]] [{:op :search-around :link seite :in in} T])
                                 (tiefer von))))]
          [2 (gen/fmap (fn [[op a b]] [{:op op :in [(first a) (first b)]} T])
                       (gen/tuple (gen/elements [:union :intersect :subtract])
                                  (tiefer T) (tiefer T)))]
          [(if (seq (abgehend T)) 1 0)
           (gen/bind (gen/elements (or (seq (abgehend T)) [nil]))
                     (fn [[seite fern]]
                       (gen/fmap (fn [[[in t] C]]
                                   [{:op :filter :include [{:as :x :link seite}]
                                     :where [:having-all :x (vec C)] :in in} t])
                                 (gen/tuple (tiefer T)
                                            (gen/vector
                                             (gen/elements
                                              (concat (get-in werte [fern :id]) ["nicht-da"])) 0
                                             3)))))]])))))

(defn- kaputt
  [rids T]
  (let [scan {:op :scan :root (rids T)}
        anderer (first (remove #{T} (keys attrs)))]
    (gen/elements
     [[scan {:order-by :gibt-es-nicht}]
      [{:op :filter :where [:= :gibt-es-nicht "x"] :in scan} {}]
      [scan {:offset 1}]
      [scan {:limit -1}]
      [{:op :union :in [scan {:op :scan :root (rids anderer)}]} {}]
      [{:op :filter :where [:like :kennzeichen "x"] :in scan} {}]
      [scan {:aggs [{:fn :median :attr :kennzeichen :name :m0}]}]
      [scan {:aggs [{:fn :sum :name :m0}]}]
      [{:op :union :in [scan {:op :order :order-by (first (keys (attrs T))) :in scan}]} {}]
      [{:op :filter :where [:= (first (keys (attrs T)))] :in scan} {}]
      [scan
       {:aggs [{:fn :count :name :m0}] :group-by [[:bucket (first (keys (attrs T))) :fortnight]]}]
      [scan {:aggs [{:fn :count :name :m0}] :group-by [[:width (first (keys (attrs T))) 0]]}]
      [scan {:aggs [{:fn :count :name :x} {:fn :count :name :x}]}]
      [{:op :scan :root (rids T) :in scan} {}]
      [{:op :union :in []} {}]
      [{:op :search-around :in scan} {}]
      [{:op :scan :root (rids T) :limit 3} {}]
      [{:op :nope} {}]
      [{:op :template :param-slots [:x] :in scan} {}]
      [{:op :aggregate :aggs [{:fn :count}] :in {:op :page :limit 3 :in scan}} {}]
      [scan {:aggs [{:fn :count :name :m0}] :group-by [(first (keys (attrs T)))]
             :order-by (second (keys (attrs T)))}]
      [scan {:aggs [{:fn :count :name :m0}] :group-by [(name (first (keys (attrs T))))]}]])))

(defn- form
  [art T hop?]
  (let [as   (keys (attrs T))
        num? #(= :num (get-in attrs [T %]))
        date? #(= :date (get-in attrs [T %]))
        fenster (gen/one-of [(gen/return {})
                             (gen/fmap (fn [[l o]] (cond-> {:limit l} o (assoc :offset o)))
                                       (gen/tuple (gen/choose 0 4) (gen/elements [nil 0 1 3])))])
        ordnung (gen/one-of [(gen/return {})
                             (gen/fmap (fn [[a d]] {:order-by a :sort-dir d})
                                       (gen/tuple (gen/elements as) (gen/elements [:asc :desc])))])
        metrik  (gen/one-of
                 (cond-> [(gen/return {:fn :count})
                          (gen/fmap (fn [[f a]] {:fn f :attr a})
                                    (gen/tuple (gen/elements [:count :min :max :cardinality])
                                               (gen/elements as)))]
                   (some num? as) (conj (gen/fmap (fn [[f a]] {:fn f :attr a})
                                                  (gen/tuple (gen/elements [:sum :avg])
                                                             (gen/elements (filter num? as)))))))
        gruppe  (gen/one-of
                 (cond-> [(gen/elements as)]
                   (seq (abgehend T))
                   (conj (gen/fmap (fn [[seite _]] [:link seite]) (gen/elements (abgehend T))))
                   hop? (conj (gen/return [:origin]))
                   (some date? as) (conj (gen/fmap (fn [[a u]] [:bucket a u])
                                                   (gen/tuple (gen/elements (filter date? as))
                                                              (gen/elements
                                                               [:year :quarter :month :week
                                                                :day]))))
                   (some num? as) (conj (gen/fmap (fn [[a w]] [:width a w])
                                                  (gen/tuple (gen/elements (filter num? as))
                                                             (gen/elements [1 5 100 1000]))))))]
    (gen/frequency
     [[(if (= art :zeilen) 1 0) (gen/fmap (fn [[o f]] (merge o f)) (gen/tuple ordnung fenster))]
      [(if (= art :buckets) 3 0)
       (gen/fmap (fn [[ms gs benannt? o f]]
                   (cond-> {:aggs (if (or benannt? (next ms))
                                    (vec
                                     (map-indexed (fn [i m] (assoc m :name (keyword (str "m" i))))
                                                  ms))
                                    (vec ms))
                            :group-by (vec (distinct gs))}
                     (and (or benannt? (next ms)) o) (assoc :order-by :m0 :sort-dir o)
                     (and (or benannt? (next ms)) o) (merge f)))
                 (gen/tuple (gen/vector metrik 1 2) (gen/vector gruppe 0 2) gen/boolean
                            (gen/elements [nil :asc :desc]) fenster))]
      [(if (= art :buckets) 1 0)
       (gen/fmap (fn [gs] {:aggs :count :group-by (vec (distinct gs))})
                 (gen/vector gruppe 0 1))]])))

(defn- query [spec] (tx/send! :post (paths/url :ontology-query) spec))

(defn- alle [T] (:rows (:body (query {:find T}))))

(defn- name->rid [schema section field]
  (into {} (for [[rid e] (get schema section)] [(get e field) rid])))

(def ^:private konkret [:luftfahrzeug :mangel :triebwerk :fahrwerk :mechaniker])
(def ^:private implementierer {:wartbares-teil [:triebwerk :fahrwerk]})
(def ^:private link-typen
  [:mangel-luftfahrzeug :triebwerk-luftfahrzeug :fahrwerk-luftfahrzeug :berechtigung])

(defn- sigma-aus [rids ext kant]
  {:rids rids
   :ext ext
   :kant kant
   :cx {:sigma {:extension (fn [_ T] (get ext T []))
                :edges     (fn [_ L] (get kant L []))}}})

(defn- sigma [schema]
  (let [ot   (name->rid schema :object-types :object-type-name)
        it   (name->rid schema :interfaces :interface-name)
        lt   (name->rid schema :link-types :link-type-name)
        ext  (into {} (for [T konkret]
                        [(ot T) (mapv #(assoc % :object-type-ref (ot T)) (alle T))]))
        kant (into {} (for [l link-typen]
                        [(lt l)
                         (mapv (juxt #(or (:from-id %) (:from_id %)) #(or (:to-id %) (:to_id %)))
                               (:body (api/list-links {:type l})))]))]
    (sigma-aus (merge ot it lt) ext kant)))

(defn- schluessel
  [o]
  (some (:values o) [:kennzeichen :seriennummer :mangel-nr :lizenz]))

(defn- verbergen!
  [{:keys [rids ext kant]} saat]
  (let [name-of (into {} (map (fn [[k v]] [v k])) rids)
        weg (set (for [[T os] ext
                       o os
                       :when (zero? (mod (hash [(schluessel o) saat]) 3))]
                   (do (let [r (api/mutate :apply-marking
                                           {:id (:id o) :object-type (name (name-of T))
                                            :marking "vs"}
                                           {:publish? true})]
                         (assert (ok? r) (pr-str (:body r))))
                       (:id o))))]
    (sigma-aus rids
               (update-vals ext (fn [os] (vec (remove #(weg (:id %)) os))))
               (update-vals kant (fn [es] (vec (remove (fn [[a b]] (or (weg a) (weg b))) es)))))))

(defn- ext-of
  [{:keys [ext rids]} T]
  (if-let [ts (implementierer T)]
    (mapcat #(get ext (rids %)) ts)
    (get ext (rids T))))

(defn- plattform [T ir shape]
  (let [temp (api/post! :object-sets-temp {:ir ir})]
    (if-not (ok? temp)
      {:abgelehnt (:status temp) :fehler (get-in temp [:body :error])}
      (let [r (query (merge {:object-set (get-in temp [:body :rid]) :find T}
                            (select-keys shape
                                         [:order-by :sort-dir :limit :offset :aggs :group-by])))]
        (if (ok? r) {:antwort (:body r)}
            {:abgelehnt (:status r) :fehler (get-in r [:body :error])})))))

(defn- referenz [{:keys [cx]} schema ir shape]
  (let [ir (cond-> ir
             (:aggs shape) (as-> $ (cond-> {:op :aggregate :aggs (:aggs shape) :in $}
                                     (seq (:group-by shape)) (assoc :group-by (:group-by shape))))
             (:order-by shape) (as-> $ {:op :order :order-by (:order-by shape)
                                        :sort-dir (:sort-dir shape) :in $})
             (contains? shape :limit) (as-> $ (cond-> {:op :page :limit (:limit shape) :in $}
                                                (:offset shape) (assoc :offset (:offset shape)))))]
    (try {:antwort (ros/read cx schema ir)}
         (catch clojure.lang.ExceptionInfo e {:abgelehnt (ex-message e)}))))

(defn- zahl [x] (if (number? x) (.stripTrailingZeros (bigdec x)) x))

(defn- zahl6 [x] (if (number? x) (.setScale (bigdec x) 6 java.math.RoundingMode/HALF_EVEN) x))

(defn- normal-buckets [bs]
  (vec (for [{:keys [group metrics]} bs]
         [(update-vals (or group {}) zahl)
          (update-vals (or metrics {})
                       #(if (number? %) (.setScale (bigdec %) 6 java.math.RoundingMode/HALF_EVEN)
                            %))])))

(defn- vergleich [shape p r]
  (cond
    (and (:abgelehnt p) (:abgelehnt r))
    (when (>= (:abgelehnt p) 500) {:totalitaet p :referenz r})
    (or (:abgelehnt p) (:abgelehnt r)) {:plattform p :referenz r}
    (:aggs shape)
    (let [geordnet? (or (:order-by shape) (contains? shape :limit))
          a (normal-buckets (or (:aggregations (:antwort p)) (:rows (:antwort p))))
          b (normal-buckets (:antwort r))]
      (when (if geordnet? (not= a b) (not= (set a) (set b))) {:plattform a :referenz b}))
    :else
    (let [geordnet? (or (:order-by shape) (contains? shape :limit))
          a (mapv :id (:rows (:antwort p)))
          b (mapv :id (:antwort r))]
      (when (if geordnet? (not= a b) (not= (set a) (set b)))
        {:plattform a :referenz b}))))

(defn- werte-der-flotte [s]
  (into {} (for [T (keys attrs)
                 :let [os (sort-by schluessel (ext-of s T))]]
             [T (into {:id (mapv :id os)}
                      (for [a (keys (attrs T))]
                        [a (vec (distinct (map #(get-in % [:values a]) os)))]))])))

(def ^:private statistik (atom {}))
(def ^:private beantwortet (atom #{}))
(def ^:private gruende (atom []))

(defn- merkmale
  [ir shape]
  (let [knoten (tree-seq map? #(let [i (:in %)] (if (map? i) [i] i)) ir)
        gruppen (:group-by shape)]
    (cond-> #{}
      (some #(= :search-around (:op %)) knoten)                   (conj :sprung)
      (some #(#{:union :intersect :subtract} (:op %)) knoten)     (conj :mengenop)
      (some #(= :having-all (first (:where %))) knoten)           (conj :having-all)
      (some #(= :static (:op %)) knoten)                          (conj :static)
      (some #(#{:berechtigt-fuer :berechtigte} (:link %)) knoten)  (conj :nm-sprung)
      (some #(re-find #"\.interface\." (str (:root %))) knoten)    (conj :interface)
      (:order-by shape)                                           (conj :ordnung)
      (contains? shape :limit)                                    (conj :fenster)
      (:aggs shape)                                               (conj :aggregat)
      (some #(and (vector? %) (= :link (first %))) gruppen)       (conj :link-gruppe)
      (some #(= [:origin] %) gruppen)                             (conj :origin)
      (some #(and (vector? %) (= :bucket (first %))) gruppen)     (conj :bucket)
      (some #(and (vector? %) (= :width (first %))) gruppen)      (conj :width)
      (some #(and (vector? %) (= [:width :egt-marge] (take 2 %))) gruppen) (conj :width-negativ))))

(defn- ergebnis-art [shape p]
  (cond (:abgelehnt p)                     :abgelehnt
        (:aggs shape)                      :buckets
        (seq (:rows (:antwort p)))         :zeilen
        :else                              :leer))

(defn- mit-flotte
  [befehle f]
  (let [zweig (str "lesen-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (api/post! :branches {:parent "main" :name zweig})
    (try
      (tx/with-branch zweig
        (doseq [[verb args] (concat anfang befehle)] (api/mutate verb args {:publish? true}))
        (let [schema (:body (api/get! :schema))]
          (f schema (sigma schema))))
      (finally
        (api/delete! :branch {:name zweig})))))

(defn- als-sicht
  [sicht? saat f]
  (fn [schema s]
    (if-not sicht?
      (f schema s)
      (let [soll (verbergen! s saat)]
        (binding [tx/*auth-token* @leser]
          (let [ist (sigma schema)
                ids (fn [x] (update-vals (:ext x) #(set (map :id %))))]
            (if (not= (ids soll) (ids ist))
              {:liste-der-sicht {:soll (ids soll) :ist (ids ist)}}
              (f schema (sigma-aus (:rids soll) (:ext ist) (:kant soll))))))))))

(defn- lauf
  [art sicht? befehle saat]
  (mit-flotte
   befehle
   (als-sicht sicht? saat
              (fn [schema s]
                (let [werte  (werte-der-flotte s)
                      gut    (gen/bind (gen/elements (keys attrs))
                                       (fn [T]
                                         (gen/bind (menge (:rids s) werte T 2)
                                                   (fn [[ir t]]
                                                     (gen/fmap (fn [sh] [ir t sh])
                                                               (form art t
                                                                     (= :search-around
                                                                        (:op ir))))))))
                      frage  (gen/frequency
                              [[19 gut]
                               [1
                                (gen/bind (gen/elements (keys attrs))
                                          (fn [T]
                                            (gen/fmap (fn [[ir sh]] [ir T sh])
                                                      (kaputt (:rids s) T))))]])]
                  (some (fn [[ir T shape]]
                          (let [p (plattform T ir shape)]
                            (swap! statistik update (ergebnis-art shape p) (fnil inc 0))
                            (when (:abgelehnt p) (swap! gruende conj (:fehler p)))
                            (when-not (:abgelehnt p) (swap! beantwortet into (merkmale ir shape)))
                            (when-let [d (vergleich shape p (referenz s schema ir shape))]
                              {:typ T :ir ir :gestalt shape :abweichung d})))
                        (gen/generate (gen/vector frage 25) 30 saat)))))))

(defn- ueber-den-sprung
  [{:keys [cx rids]} schema T id seite where metrik]
  (let [hop {:op :search-around :link seite :in {:op :static :ids [id] :root (rids T)}}
        ir  {:op :aggregate :aggs [(assoc metrik :name :v)]
             :in (if where {:op :filter :where where :in hop} hop)}]
    (get-in (first (ros/read cx schema ir)) [:metrics :v])))

(defn- rollup-abweichung
  [schema {:keys [ext rids cx] :as s}]
  (or
   (some (fn [{:keys [id values]}]
           (let [soll {:offene-maengel    (ueber-den-sprung s schema :luftfahrzeug id :maengel
                                                            [:= :zustand "offen"] {:fn :count})
                       :triebwerksstunden (ueber-den-sprung s schema :luftfahrzeug id :triebwerke
                                                            nil {:fn :sum :attr :betriebsstunden})
                       :mittlere-stunden  (ueber-den-sprung s schema :luftfahrzeug id :triebwerke
                                                            nil {:fn :avg :attr :betriebsstunden})}
                 ist  (select-keys values (keys soll))]
             (when (not= (update-vals soll zahl6) (update-vals ist zahl6))
               {:objekt (:kennzeichen values) :soll soll :ist ist})))
         (get ext (rids :luftfahrzeug)))
   (some (fn [{:keys [id values]}]
           (let [fern (ros/read cx schema {:op :search-around :link :luftfahrzeug
                                           :in {:op :static :ids [id] :root (rids :mangel)}})
                 soll (get-in (first fern) [:values :muster])]
             (when (not= soll (:muster values))
               {:objekt (:mangel-nr values) :soll soll :ist (:muster values)})))
         (get ext (rids :mangel)))
   (let [soll (->> (get ext (rids :luftfahrzeug))
                   (filter #(some-> (get-in % [:values :offene-maengel]) pos?))
                   (sort-by (fn [o] [(zahl6 (get-in o [:values :triebwerksstunden])) (:id o)])
                            #(compare %2 %1))
                   (mapv :id))
         ist  (mapv :id (:rows (:body (query {:find :luftfahrzeug :where {:offene-maengel {:> 0}}
                                              :order-by :triebwerksstunden :sort-dir :desc}))))]
     (when (not= soll ist)
       {:filter-und-ordnung-ueber-rollup {:soll soll :ist ist}}))
   (let [soll (reduce + (keep #(get-in % [:values :offene-maengel]) (get ext (rids :luftfahrzeug))))
         ist  (get-in (:body (query {:find :luftfahrzeug
                                     :aggs [{:fn :sum :attr :offene-maengel :name :s}]}))
                      [:aggregations 0 :metrics :s])]
     (when-not (== soll (or ist -1))
       {:aggregat-ueber-rollup {:soll soll :ist ist}}))
   (let [offen (->> (get ext (rids :luftfahrzeug))
                    (filter #(some-> (get-in % [:values :offene-maengel]) pos?))
                    (mapv :id))
         soll  (set (map :id (ros/read cx schema
                                       {:op :search-around :link :maengel
                                        :in {:op :static :ids offen :root (rids :luftfahrzeug)}})))
         r     (query {:find :luftfahrzeug :where {:offene-maengel {:> 0}} :around [:maengel]})
         ist   (set (map :id (:rows (:body r))))]
     (when (or (not= 200 (:status r)) (not= soll ist))
       {:rollup-filter-vor-sprung {:status (:status r) :soll soll :ist ist}}))))

(def ^:private abdeckung
  {:zeilen  [:sprung :nm-sprung :interface :mengenop :having-all :static :ordnung :fenster]
   :buckets [:sprung :mengenop :aggregat :link-gruppe :origin :bucket :width :width-negativ]})

(defn- pruefe! [art & [sicht?]]
  (reset! statistik {})
  (reset! beantwortet #{})
  (reset! gruende [])
  (let [abweichung (atom nil)
        r (tc/quick-check 15
                          (prop/for-all [befehle (gen/no-shrink flotte) saat
                                         (gen/no-shrink gen/nat)]
                                        (let [a (lauf art sicht? befehle saat)]
                                          (when a (reset! abweichung a))
                                          (nil? a)))
                          :seed 20261009 :max-size 30)
        {:keys [abgelehnt] :or {abgelehnt 0} :as stunden} @statistik
        n (reduce + (vals stunden))]
    (is (:pass? r) (pr-str @abweichung))
    (testing "die Fragen waren echte Fragen, nicht lauter leere oder abgelehnte"
      (is (> (get stunden art 0) (* (if sicht? 0.25 0.4) n)) (pr-str stunden))
      (is (< abgelehnt (* 0.1 n)) (str (pr-str stunden) " " (pr-str (frequencies @gruende))))
      (is (empty? (remove @beantwortet (get abdeckung art)))
          (str "nie beantwortet: " (pr-str (remove @beantwortet (get abdeckung art))))))))

(deftest zeilen-lesen-wie-die-referenz (pruefe! :zeilen))

(deftest buckets-lesen-wie-die-referenz (pruefe! :buckets))

(deftest zeilen-der-sicht-lesen-wie-die-referenz-ohne-das-verborgene (pruefe! :zeilen true))

(deftest buckets-der-sicht-lesen-wie-die-referenz-ohne-das-verborgene (pruefe! :buckets true))

(def ^:private titel
  {:luftfahrzeug :kennzeichen :mangel :mangel-nr :triebwerk :seriennummer :mechaniker :lizenz})

(defn- text-attrs [T] (for [[a k] (attrs T) :when (= :text k)] a))

(defn- lesehilfen-abweichung
  [schema {:keys [rids cx] :as s}]
  (or
   (some (fn [[T seite fern]]
           (let [body
                 {:rows (concat (:rows (:body (query {:find T :show [{seite [(titel fern)]}]})))
                                (:rows (:body (query {:find T :show [{seite [(titel fern)]}]
                                                      :order-by (titel T) :limit 2
                                                      :offset 1}))))}]
             (some (fn [{:keys [id values]}]
                     (let [soll (set (map #(get-in % [:values (titel fern)])
                                          (ros/read cx schema
                                                    {:op :search-around :link seite
                                                     :in {:op :static :ids [id] :root (rids T)}})))
                           ist  (set (map (titel fern) (get values seite)))]
                       (when (not= soll ist) {:show [T seite] :objekt id :soll soll :ist ist})))
                   (:rows body))))
         (for [[T ss] abgehend [seite fern] ss] [T seite fern]))
   (some (fn [[T k]]
           (let [soll (set (for [{:keys [id values]} (ext-of s T)
                                 :when (tokens/match? (keep #(let [v (get values %)]
                                                               (when (string? v) v))
                                                            (text-attrs T))
                                                      k)]
                             id))
                 ist  (set (map :id (:rows (:body (query {:find T :keywords k})))))]
             (when (not= soll ist) {:keywords [T k] :soll soll :ist ist})))
         (for [T (keys attrs) k ["leck" "A3" "aaab" "%" "offen" "e-1"]] [T k]))
   (let [ex (fn [spec] (tx/send! :post (str (paths/url :ontology-query) "?explain=true") spec))
         r  (ex {:find :mangel :aggs [{:fn :count}] :accuracy :approximate})
         q  (query {:find :mangel :aggs [{:fn :count :name :n}] :accuracy :approximate})]
     (cond
       (not= [:exact :approximate]
             [(get-in r [:body :accuracy]) (get-in r [:body :accuracy-requested])])
       {:genauigkeit-ist-praeferenz (:body r)}
       (not= (count (ext-of s :mangel)) (get-in q [:body :aggregations 0 :metrics :n]))
       {:genauigkeit-exakt-bedient (:body q)}
       (not= 400 (:status (query {:find :mangel :aggs [{:fn :count}] :accuracy :schnell})))
       {:unbekannte-genauigkeit
        (:status (query {:find :mangel :aggs [{:fn :count}] :accuracy :schnell}))}
       (not= 400 (:status (query {:find :mangel :group-by [:kategorie]})))
       {:gruppen-ohne-metrik (:status (query {:find :mangel :group-by [:kategorie]}))}))
   (let [ist  (set (map :id
                        (:rows (:body (query {:find :fahrwerk :where {:landungen {:= "4000"}}})))))
         soll (set (map :id
                        (filter #(= 4000 (get-in % [:values :landungen])) (ext-of s :fahrwerk))))]
     (when (not= soll ist) {:wert-wird-umgewandelt {:soll soll :ist ist}}))
   (some (fn [T]
           (let [ids (map :id (ext-of s T))]
             (or (some (fn [id]
                         (let [ist (mapv :id
                                         (:rows (:body (query {:find T :where {:id {:= id}}}))))]
                           (when (not= [id] ist) {:id-gleich id :ist ist})))
                       (take 2 ids))
                 (let [r (query {:find T :where {:id {:starts-with "a"}}})]
                   (when (not= 400 (:status r)) {:id-nur-gleich (:status r)})))))
         (keys attrs))))

(deftest lesehilfen-folgen-ihrer-definition
  (let [abweichung (atom nil)
        r (tc/quick-check 6
                          (prop/for-all [befehle (gen/no-shrink flotte)]
                                        (let [a (mit-flotte befehle lesehilfen-abweichung)]
                                          (when a (reset! abweichung a))
                                          (nil? a)))
                          :seed 20261009 :max-size 30)]
    (is (:pass? r) (pr-str @abweichung))))

(deftest rollups-sind-aggregate-ueber-den-sprung
  (let [abweichung (atom nil)
        r (tc/quick-check 8
                          (prop/for-all [befehle (gen/no-shrink flotte)]
                                        (let [a (mit-flotte befehle rollup-abweichung)]
                                          (when a (reset! abweichung a))
                                          (nil? a)))
                          :seed 20261009 :max-size 30)]
    (is (:pass? r) (pr-str @abweichung))))

(deftest rollups-zaehlen-nur-was-der-leser-sieht
  (let [abweichung (atom nil)
        r (tc/quick-check 6
                          (prop/for-all [befehle (gen/no-shrink flotte) saat
                                         (gen/no-shrink gen/nat)]
                                        (let [a (mit-flotte befehle
                                                            (als-sicht true saat
                                                                       rollup-abweichung))]
                                          (when a (reset! abweichung a))
                                          (nil? a)))
                          :seed 20261009 :max-size 30)]
    (is (:pass? r) (pr-str @abweichung))))
