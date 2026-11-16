(ns sutra.capps.bind
  (:refer-clojure :exclude [resolve])
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]))

(def anon-spec-keys
  #{:object-property :agg :expr :coalesce})

(def forms
  [{:id :literal :key nil :effect? true :label "fester Wert"
    :hint "Eine Zahl, ein Text, ein Schlüsselwort — oder nichts (Variable leeren)."}
   {:id :row :key :row :env :row :names :field :effect? true
    :label "Feld der geklickten Zeile"
    :hint "Nur in :on-row-click — das Feld des Objekts, auf das geklickt wurde."
    :where "in einem :on-row-click"}
   {:id :item :key :item :env :item :names :field :effect? true
    :label "Feld des Schleifen-Objekts"
    :hint "Nur im :body einer :loop — das Feld der Karte, die den Knopf trägt."
    :where "im :body einer :loop (oder einer verschachtelten :vars einer Objektansicht)"}
   {:id :item-not :key :item-not :env :item :names :field
    :label "Feld des Schleifen-Objekts, verneint"
    :hint "Nur im :body einer :loop — wahr, wenn das Feld leer oder falsch ist."
    :where "im :body einer :loop"}
   {:id :arg :key :arg :env :arg :names :field :effect? true
    :label "Feld der Overlay-Nutzlast"
    :hint "Nur im :body eines Overlays — das Feld der Nutzlast, die {:do :open} übergab."
    :where "im :body eines :overlay"}
   {:id :val :key :val :env :vars :names :variable :effect? true
    :label "Wert einer Variablen"
    :hint "Der aktuelle Wert einer anderen App-Variablen."}
   {:id :var :key :var :env :vars :names :variable :effect? true :handle? true
    :label "Variable selbst (schreibbar)"
    :hint "Die Variable selbst, nicht ihr Wert — nur selten gemeint."}
   {:id :from :key :from :env :vars :names :variable
    :label "Menge einer Variablen"
    :hint "Die Objektmenge einer anderen Variablen — nie ein Typ."}
   {:id :source :key :source :env :types :names :type
    :label "alle Objekte eines Typs"
    :hint "Jedes Objekt des Typs — eine Abfrage, die der Speicher beantwortet."}
   {:id :linked :key :linked :env :linked :names :type
    :label "verknüpfte Objekte"
    :hint "In einer Objektansicht: die mit ihrem Objekt verknüpften Objekte des Typs."
    :where "in einer Objektansicht"}
   {:id :spec :key nil :env :specs :names :spec
    :label "Definition vor Ort"
    :hint "Eine Variablendefinition ohne Namen, z. B. {:object-property p :of s}."}])

(def by-id (coll/index-by :id forms))

(def ^:private by-key (coll/index-by :key (filter :key forms)))

(def form-keys (set (keys by-key)))

(def variable-keys
  (into #{} (comp (filter :key) (filter (comp #{:variable :type} :names)) (map :key)) forms))

(def fill-keys
  (into #{} (comp (filter :key) (filter (comp #{:field} :names)) (map :key)) forms))

(def effect-forms (filterv :effect? forms))

(defn anon-spec?
  [x]
  (and (map? x)
       (not (contains? x :widget))
       (not (contains? x :kind))
       (not (contains? x :vars))
       (boolean (some anon-spec-keys (keys x)))))

(defn form-of
  [x]
  (when (map? x)
    (if (= 1 (count x))
      (get by-key (key (first x)))
      (when (anon-spec? x) (by-id :spec)))))

(defn form-id [x] (if-let [f (form-of x)] (:id f) :literal))

(defn reference?
  [x]
  (some? (form-of x)))

(defn variable-ref?
  [x]
  (and (map? x) (= 1 (count x)) (contains? variable-keys (key (first x)))))

(defn fill-ref?
  [x]
  (and (map? x) (= 1 (count x)) (contains? fill-keys (key (first x)))))

(defn named
  [x]
  (cond (keyword? x)                           x
        (and (string? x) (not (str/blank? x))) (keyword x)
        (symbol? x)                            (keyword x)))

(defn form-name
  [x]
  (when-let [f (form-of x)]
    (when (:key f) (named (get x (:key f))))))

(defn name-in
  [id x]
  (when (= id (form-id x)) (form-name x)))

(defn value-of
  [x]
  (if-let [k (:key (form-of x))] (get x k) x))

(defn problem
  [rule x & {:as more}]
  {:problem (merge {:rule rule :form x} more)})

(defn- lookup
  [env nm]
  (cond (map? env)  (if (contains? env nm) (get env nm) ::absent)
        (set? env)  (if (contains? env nm) nm ::absent)
        (true? env) nm
        (ifn? env)  (let [r (env nm)] (if (nil? r) ::absent r))
        :else       ::absent))

(defn resolve
  [form scope]
  (let [f (form-of form)]
    (if (nil? f)
      {:value form}
      (let [env (:env f)
            nm  (when (:key f) (named (get form (:key f))))]
        (cond
          (not (contains? scope env))
          (problem :not-filled form :env env :where (:where f))

          (and (:key f) (nil? nm))
          (problem :no-name form)

          (#{:field :spec} (:names f))
          ((:read scope) f form nm (get scope env) scope)

          :else
          (let [entry (lookup (get scope env) nm)]
            (cond
              (= ::absent entry)
              (problem (if (= :type (:names f)) :unknown-type :undeclared) form :name nm)

              (and (:handle? f) (:stateful? scope) (not ((:stateful? scope) entry)))
              (problem :no-state form :name nm)

              :else ((:read scope) f form nm entry scope))))))))

(defn value
  [form scope not-found]
  (let [r (resolve form scope)]
    (if (contains? r :value) (:value r) not-found)))

(defn with-fill
  [scope env x]
  (cond-> (assoc scope env x)
    (= :item env) (assoc :item-not x)))

(defn without-fill
  [scope env]
  (cond-> (dissoc scope env)
    (= :item env) (dissoc :item-not)))

(defn message
  [{:keys [rule form name where]}]
  (case rule
    :not-filled   (str (pr-str form) " ist hier nicht gefüllt"
                       (when where (str " — es gilt nur " where)))
    :no-name      (str (pr-str form) " nennt keinen Namen")
    :undeclared   (str "Variable " (pr-str name) " ist nicht deklariert")
    :unknown-type (str "Objekttyp " (pr-str name) " ist hier nicht lesbar")
    :no-state     (str (pr-str name) " ist berechnet und hat keinen Zustand"
                       " — {:val " (pr-str name) "} liest seinen Wert")
    (str (pr-str form) ": " (clojure.core/name (or rule :problem)))))
