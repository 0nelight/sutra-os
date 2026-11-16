(ns ^:api scenario.aviation-gesetze-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.types.paths :as paths]))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-ontologie [f]
  (assert (ok? (install-in-project {:schema ontologie})))
  (assert (ok? (api/post! :staging)))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-ontologie)

(def ^:private leer {:lfz {} :maengel {} :mech {}})

(defn- offene [welt kz]
  (count (filter #(and (= kz (:kz %)) (= "offen" (:zustand %))) (vals (:maengel welt)))))

(defn- gueltiges-kennzeichen? [kz] (boolean (re-matches #"D-[A-Z]{4}" kz)))

(defmulti ^:private schritt
  (fn [_ [verb]] verb))

(defmethod schritt :registrieren [w [_ kz muster]]
  (if (and (gueltiges-kennzeichen? kz) (not (get-in w [:lfz kz])))
    [true (assoc-in w [:lfz kz] {:muster muster :status "in-betrieb"})]
    [false w]))

(defmethod schritt :mechaniker [w [_ lizenz muster]]
  (if (get-in w [:mech lizenz])
    [false w]
    [true (assoc-in w [:mech lizenz] muster)]))

(defmethod schritt :melden [w [_ nr kz kategorie]]
  (if (and (not (get-in w [:maengel nr]))
           (get-in w [:lfz kz])
           (#{"A" "B" "C"} kategorie))
    [true (assoc-in w [:maengel nr] {:kz kz :kategorie kategorie :zustand "offen"})]
    [false w]))

(defmethod schritt :beheben [w [_ nr lizenz]]
  (let [m (get-in w [:maengel nr])
        muster (get-in w [:mech lizenz])]
    (if (and m muster
             (= "offen" (:zustand m))
             (= muster (get-in w [:lfz (:kz m) :muster])))
      [true (assoc-in w [:maengel nr :zustand] "behoben")]
      [false w])))

(defmethod schritt :zurueckstellen [w [_ nrs]]
  (if (every? #(get-in w [:maengel %]) nrs)
    [true (reduce #(assoc-in %1 [:maengel %2 :zustand] "zurueckgestellt") w nrs)]
    [false w]))

(defmethod schritt :aog [w [_ kz]]
  (if (and (= "in-betrieb" (get-in w [:lfz kz :status])) (pos? (offene w kz)))
    [true (assoc-in w [:lfz kz :status] "aog")]
    [false w]))

(defmethod schritt :freigeben [w [_ kz]]
  (if (and (= "aog" (get-in w [:lfz kz :status])) (zero? (offene w kz)))
    [true (assoc-in w [:lfz kz :status] "in-betrieb")]
    [false w]))

(defn- aufruf [[verb & args]]
  (case verb
    :registrieren   (let [[kz m] args] [:luftfahrzeug-registrieren {:kennzeichen kz :muster m}])
    :mechaniker     (let [[l m] args] [:mechaniker-anlegen {:lizenz l :name l :muster m}])
    :melden         (let [[nr kz k] args] [:mangel-melden {:mangel-nr nr :luftfahrzeug kz
                                                           :beschreibung "x" :kategorie k}])
    :beheben        (let [[nr l] args] [:mangel-beheben {:mangel nr :mechaniker l}])
    :zurueckstellen [:maengel-zurueckstellen {:maengel (vec (first args))}]
    :aog            [:aog-erklaeren {:luftfahrzeug (first args)}]
    :freigeben      [:freigeben {:luftfahrzeug (first args)}]))

(def ^:private nummern ["G-1" "G-2" "G-3" "G-9"])

(defn- meist [haeufig & selten]
  (gen/frequency (into [[6 (gen/return haeufig)]] (map (fn [x] [1 (gen/return x)]) selten))))

(def ^:private befehl
  (let [kz  (meist "D-GAAA" "D-GAAB" "D-GAAC" "D-G1")
        nr  (gen/elements nummern)
        kat (meist "A" "C" "Z")]
    (gen/frequency
     [[1 (gen/tuple (gen/return :registrieren) kz (gen/elements ["A320" "B748"]))]
      [1
       (gen/tuple (gen/return :mechaniker) (gen/elements ["L-1" "L-3"])
                  (gen/elements ["A320" "B748"]))]
      [5 (gen/tuple (gen/return :melden) nr kz kat)]
      [3 (gen/tuple (gen/return :beheben) nr (meist "L-1" "L-2" "L-3"))]
      [2 (gen/tuple (gen/return :zurueckstellen) (gen/vector nr 0 2))]
      [4 (gen/tuple (gen/return :aog) kz)]
      [4 (gen/tuple (gen/return :freigeben) kz)]])))

(defn- lesen [typ]
  (:rows (:body (tx/send! :post (paths/url :ontology-query) {:find typ}))))

(defn- beobachtet []
  {:lfz     (into {} (for [{v :values} (lesen :luftfahrzeug)]
                       [(:kennzeichen v) {:muster (:muster v) :status (:status v)
                                          :offene (:offene-maengel v)}]))
   :maengel (into {} (for [{v :values} (lesen :mangel)]
                       [(:mangel-nr v) {:kz (:kennzeichen v) :kategorie (:kategorie v)
                                        :zustand (:zustand v)}]))
   :mech    (into {} (for [{v :values} (lesen :mechaniker)] [(:lizenz v) (:muster v)]))})

(defn- erwartet [w]
  (-> w
      (update :lfz #(into {} (for [[kz l] %] [kz (assoc l :offene (offene w kz))])))))

(defn- lauf
  [befehle]
  (let [zweig (str "gesetz-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (api/post! :branches {:parent "main" :name zweig})
    (try
      (tx/with-branch zweig
      (or (loop [w leer [b & mehr] befehle]
            (if-not b
              (let [ist (beobachtet) soll (erwartet w)]
                (when (not= ist soll) {:zustand {:soll soll :ist ist}}))
              (let [[angenommen? w'] (schritt w b)
                    r (apply api/mutate (conj (aufruf b) {:publish? true}))]
                (cond
                  (>= (:status r) 500)
                  {:befehl b :totalitaet (:status r) :fehler (get-in r [:body :error])}

                  (not= angenommen? (ok? r))
                  {:befehl b :modell angenommen? :plattform (:status r)
                   :fehler (get-in r [:body :error])}

                  :else (recur w' mehr)))))
          nil))
      (finally
        (api/delete! :branch {:name zweig})))))

(deftest die-plattform-folgt-dem-modell
  (let [abweichung (atom nil)
        r (tc/quick-check 100
                          (prop/for-all [befehle (gen/vector befehl 1 14)]
                                        (let [a (lauf befehle)]
                                          (when a (reset! abweichung a))
                                          (nil? a)))
                          :seed 20261009)]
    (is (:pass? r)
        (str "kleinste Folge: " (pr-str (first (:smallest (:shrunk r))))
             "\nAbweichung: " (pr-str @abweichung)))))
