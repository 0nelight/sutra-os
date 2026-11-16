(ns sutra.capps.variables
  (:refer-clojure :exclude [num])
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [clojure.string :as str]
            [sutra.capps.walk :as walk]
            [sutra.types.decimal :as d]
            [sutra.capps.format :refer [v num calc compare-operands temporal]]
            [sutra.capps.bind :as bind]
            [sutra.capps.object-filter :as ofilter]
            [sutra.ref.geo :as geo]))

(defn deref*
  [x]
  (if #?(:cljs (satisfies? IDeref x) :clj (instance? clojure.lang.IDeref x))
    @x
    x))

(defn fn-error [msg] {::fn-error msg})
(defn fn-error? [x] (and (map? x) (contains? x ::fn-error)))
(defn fn-error-msg [x] (get x ::fn-error))

(defn var-kind
  [spec]
  (cond (:aggregate spec)                           :aggregate
        (:function spec)                            :function
        (:compare spec)                             :compare
        (or (contains? spec :default)
            (contains? spec :default-agg)
            (:in-url spec)
            (:input? spec)
            (= ofilter/var-type (->kw (:type spec)))) :state
        :else                                       :derived))

(def url-coercions
  #{:string :keyword :int :bool :edn})

(def recompute-modes
  [{:id :automatic :label "Automatisch"}
   {:id :event :label "Nur durch ein Ereignis"}
   {:id :load-and-event :label "Beim Laden und durch ein Ereignis"}])

(def url-behaviors
  [{:id :never :label "nie in der URL"
    :writes? false :only-while-live? false
    :hint (str "Ein Startwert, den ein Aufrufer hereinreicht. Die Variable liest die"
               " Adresszeile EINMAL beim Laden und gehoert danach der App — was der"
               " Nutzer daran aendert, bleibt im Fenster.")}
   {:id :when-visible :label "solange sie sichtbar ist"
    :writes? true :only-while-live? true
    :hint (str "Im Link, aber nur solange ein Baustein auf der offenen Seite sie liest."
               " Der Filter einer Seite, die gerade niemand ansieht, verstopft den Link"
               " damit nicht.")}
   {:id :always :label "immer"
    :writes? true :only-while-live? false
    :hint (str "Im Link, sobald sie vom Standard abweicht — auch wenn die Seite, auf der"
               " sie bedient wird, gerade zu ist.")}])

(def in-url-behaviors
  (into #{} (map :id) url-behaviors))

(def ^:private by-behavior
  (coll/index-by :id url-behaviors))

(def default-in-url :never)

(defn interface?
  [spec]
  (some? (:external-id spec)))

(defn url-key
  [spec]
  (:external-id spec))

(defn in-url
  [spec]
  (let [b (->kw (:in-url spec))]
    (if (contains? in-url-behaviors b) b default-in-url)))

(defn url-behavior
  [b]
  (get by-behavior (->kw b) (get by-behavior default-in-url)))

(defn- lift-spec
  [nm spec]
  (if-not (contains? spec :url?)
    spec
    (cond-> (dissoc spec :url?)
      (:url? spec) (assoc :external-id (or (:external-id spec) nm)
                          :in-url      :always))))

(defn lift-url-flags
  [doc]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (map? (:vars x)))
       (update x :vars #(into {} (map (fn [[nm spec]]
                                        [nm (if (map? spec) (lift-spec nm spec) spec)]))
                              %))
       x))
   doc))

(defn lift-schema-applications
  [schema]
  (cond-> schema
    (map? (:applications schema)) (update :applications update-vals lift-url-flags)))

(defn interface-slots
  [vars]
  (into {} (keep (fn [[nm spec]] (when-let [ext (:external-id spec)] [ext nm]))) vars))

(defn url-slots
  [vars]
  (interface-slots vars))

(defn url-writers
  [vars]
  (into {} (keep (fn [[_ spec]]
                   (let [b (in-url spec)]
                     (when (and (interface? spec) (not= :never b))
                       [(url-key spec) b]))))
        vars))

(defn coercion-for
  [default]
  (cond
    (keyword? default) :keyword
    (boolean? default) :bool
    (integer? default) :int
    (string? default)  :string
    (coll? default)    :edn
    :else              :string))

(def string-ops
  #{:str :pad :upper :lower :trim})

(def logic-ops
  #{:= :not= :< :> :<= :>= :and :or :not :blank?})

(def date-ops
  #{:today :relative-date :relative-time :between-dates :between-times})

(def array-ops
  #{:length})

(def array-checks
  #{:contains :does-not-contain :is-subset-of})

(def raw-ops
  (into #{:if :coalesce}
        (concat string-ops logic-ops date-ops array-ops array-checks)))

(defn- literal-type
  [x]
  (cond (string? x)  :string
        (boolean? x) :boolean
        (number? x)  :number
        (d/exact? x) :number
        (keyword? x) :keyword
        :else        nil))

(defn expr-type
  [e]
  (if (vector? e)
    (let [[op & args] e
          op (->kw op)]
      (cond
        (contains? string-ops op) :string
        (contains? logic-ops op)  :boolean
        (contains? array-checks op) :boolean
        (#{:today :relative-date} op) :date
        (= :relative-time op)         :timestamp
        (= :if op)                (some expr-type (rest args))
        (= :coalesce op)          (some expr-type args)
        :else                     :number))
    (literal-type e)))

(def var-types
  #{:string :keyword :number :boolean :date :timestamp
    :array :object-set ofilter/var-type})

(def view-object  :object)
(def view-objects :objects)

(def reserved-names #{view-object view-objects})

(defn var-type
  [spec]
  (if-let [t (->kw (:type spec))]
    t
    (cond
      (:source spec)                    :object-set
      (:from spec)                      :object-set
      (:linked spec)                    :object-set
      (:function spec)                  :array
      (:union spec)                     :object-set
      (:pluck spec)                     :array
      (:expr spec)                      (or (expr-type (:expr spec)) :number)
      (:aggregate spec)                 (if (or (:group-by spec)
                                                (:group-by (:aggregate spec)))
                                          :array :number)
      (:agg spec)                       (when (#{:count :distinct-count :sum :avg}
                                               (->kw (first (:agg spec))))
                                          :number)
      (:default-agg spec)               (when (#{:count :distinct-count :sum :avg}
                                               (->kw (first (:default-agg spec))))
                                          :number)
      (contains? spec :default)         (let [d (:default spec)]
                                          (cond (keyword? d) :keyword
                                                (boolean? d) :boolean
                                                (number? d)  :number
                                                (string? d)  :string
                                                (coll? d)    :array
                                                :else        nil))
      :else                             nil)))

(defn input-vars
  [vars]
  (into {} (for [[k spec] vars :when (:input? spec)] [k (var-type spec)])))

(def ^:private type->coercion
  {:string :string, :keyword :keyword, :number :int, :boolean :bool,
   :date :string, :timestamp :string, :array :edn, ofilter/var-type :edn})

(defn coercion-of
  [spec]
  (or (->kw (:as spec))
      (get type->coercion (var-type spec))
      (coercion-for (:default spec))))

(defn definition-kind
  [spec]
  (cond
    (:object-property spec)      :object-property
    (:function spec)             :function
    (:aggregate spec)            :aggregation
    (:default-agg spec)          :aggregation
    (:compare spec)              :comparison
    (or (:source spec)
        (:from spec)
        (:linked spec)
        (:union spec))           :object-set-definition
    (or (:expr spec) (:agg spec)
        (:coalesce spec) (:pluck spec)
        (:with spec))            :transformation
    (contains? spec :default)    :static
    :else                        nil))

(def type->definitions
  (let [skalar #{:static :aggregation :transformation :object-property}]
    {:object-set #{:object-set-definition}
     :array      #{:static :aggregation :transformation :comparison :function}
     :string     skalar
     :keyword    skalar
     :number     skalar
     :boolean    skalar
     :date       skalar
     :timestamp  skalar
     ofilter/var-type #{:static}}))

(defn definition-problem
  [spec]
  (when-let [t (->kw (:type spec))]
    (when-let [erlaubt (get type->definitions t)]
      (when-let [d (definition-kind spec)]
        (when-not (contains? erlaubt d)
          (str "ein " (pr-str t) " entsteht nicht aus " (pr-str d)
               " — zulaessig sind " (str/join " " (sort erlaubt))))))))

(defn object-inputs
  [vars]
  (vec (for [[_ spec] (sort-by (comp str key) vars)
             :let  [t (->kw (:object-type spec)) ext (:external-id spec)]
             :when (and t ext)]
         {:external-id ext :object-type t :set? (= :array (->kw (:type spec)))})))

(defn interface-problem
  [nm spec]
  (let [ext (:external-id spec)
        b   (->kw (:in-url spec))
        im? (and (some? b) (not= :never b))
        nm  (if (some? nm) (pr-str nm) "diese Variable")]
    (cond
      (contains? spec :url?)
      (str "app/vars: " nm " nennt :url? — das gibt es nicht mehr. In die URL"
           " geschrieben wird nach :in-url (" (str/join " " (sort in-url-behaviors))
           "), und der Schluessel dort ist die :external-id. Aus {:url? true} wird"
           " {:external-id <name> :in-url :always}")

      (and (some? b) (not (contains? in-url-behaviors b)))
      (str "app/vars: " nm " nennt :in-url " (pr-str b)
           " — erlaubt sind " (str/join " " (sort in-url-behaviors)))

      (and im? (nil? ext))
      (str "app/vars: " nm " soll in die URL, nennt aber keine :external-id."
           " Der Schluessel in der Adresszeile ist die external id, nie der"
           " Variablenname: sonst bricht ein Link, den jemand anders aufbewahrt,"
           " sobald die App innen umbenennt")

      (and im? (nil? (->kw (:type spec))))
      (str "app/vars: " nm " soll in die URL und braucht dafuer :type — die"
           " URL traegt Text, und ob " (pr-str (:default spec)) " als String oder"
           " Keyword zurueckkommt, sagt nur der Typ ("
           (str/join " " (sort var-types)) "). Er bestimmt auch die Uebersetzung;"
           " :as ist nur noch der Sonderfall, der davon abweicht")

      (and (some? (:object-type spec)) (nil? ext))
      (str "app/vars: " nm " nennt :object-type und ist damit ein Eingang der App fuer"
           " ein Objekt — ein Eingang ist Teil der Schnittstelle und braucht :external-id")

      (and (some? (:object-type spec)) (not (#{:string :array} (->kw (:type spec)))))
      (str "app/vars: " nm " nennt :object-type — die Variable traegt die Id eines"
           " Objekts (:type :string) oder die Ids einer Menge (:type :array)")

      (and im? (= :object-set-definition (definition-kind spec)))
      (str "app/vars: " nm " ist eine gefilterte Objektmenge und kann nicht in"
           " der URL stehen — eine Menge, die aus Bedingungen entsteht, hat keine"
           " geschriebene Form, die das Aendern der Daten unter ihr ueberlebt. In die"
           " URL gehoert der SKALAR, den die Bedingung liest; die Menge leitet sich"
           " daraus wieder ab")

      :else nil)))

(defn type-contradiction
  [spec]
  (when-let [erklaert (->kw (:type spec))]
    (cond
      (not (contains? var-types erklaert))
      (str (pr-str erklaert) " ist kein Variablen-Typ ("
           (str/join " " (sort var-types)) ")")

      :else
      (let [abgeleitet (var-type (dissoc spec :type))]
        (when (and abgeleitet (not= abgeleitet erklaert))
          (str ":type " (pr-str erklaert) " widerspricht der Form, die "
               (pr-str abgeleitet) " ergibt"))))))

(defn- vals* [m] (when (map? m) (vals m)))

(defn prop-get
  [vals prop]
  (let [k (->kw prop)]
    (cond
      (nil? k)            nil
      (contains? vals k)  (get vals k)
      :else               (get vals (name k)))))

(defn row-prop
  [row prop]
  (let [k    (->kw prop)
        vals (v row)]
    (cond
      (nil? k)                    nil
      (contains? vals k)          (get vals k)
      (contains? vals (name k))   (get vals (name k))
      :else                       (prop-get row prop))))

(def where-ops
  {:=        (fn [a b] (= a b))
   :not=     (fn [a b] (not= a b))
   :in       (fn [a s]
               (cond
                 (nil? s)                          true
                 (coll? s)                         (if (empty? s) true (contains? (set s) a))
                 (and (string? s) (str/blank? s))  true
                 :else                             (= a s)))
   :contains (fn [a b] (if (if (coll? b) (empty? b) (str/blank? (str b)))
                         true
                         (str/includes? (str/lower-case (str a)) (str/lower-case (str b)))))
   :>        (fn [a b] (boolean (when-let [[a b] (compare-operands a b)] (> a b))))
   :<        (fn [a b] (boolean (when-let [[a b] (compare-operands a b)] (< a b))))
   :>=       (fn [a b] (boolean (when-let [[a b] (compare-operands a b)] (>= a b))))
   :<=       (fn [a b] (boolean (when-let [[a b] (compare-operands a b)] (<= a b))))
   :truthy   (fn [a _] (not (or (false? a) (= 0 a) (= 0.0 a) (= "0" a))))
   :falsy    (fn [a _] (or (false? a) (= 0 a) (= 0.0 a) (= "0" a)))})

(def row-ops
  #{:filter})

(def ^:private store-ops
  {:=    :=
   :not= :not=
   :<    :<
   :>    :>
   :<=   :<=
   :>=   :>=})

(defn- open-needle?
  [b]
  (if (coll? b) (empty? b) (str/blank? (str b))))

(defn- lower-clause
  [[prop op _] target]
  (let [op   (->kw op)
        prop (when (or (keyword? prop) (string? prop)) (->kw prop))]
    (cond
      (or (nil? prop) (= :* prop))         nil
      (= :contains op)                     (if (open-needle? target)
                                             ::open
                                             {prop {:contains-ci (str target)}})
      (and (#{:= :not=} op) (nil? target)) {prop {:nil? (= := op)}}
      (nil? target)                        nil
      (store-ops op)                       {prop {(store-ops op) target}}
      :else                                nil)))

(defn lowerable-op?
  [op]
  (let [op (->kw op)]
    (boolean (or (store-ops op) (= :contains op)))))

(defn store-where
  [clauses targets]
  (when (seq clauses)
    (reduce (fn [acc [clause target]]
              (let [p (lower-clause clause target)]
                (cond
                  (nil? p)     (reduced nil)
                  (= ::open p) acc
                  :else        (let [[prop m] (first p)
                                     [op _]   (first m)]
                                 (if (get-in acc [prop op])
                                   (reduced nil)
                                   (update acc prop merge m))))))
            {}
            (map vector clauses targets))))

(def ^:private wire-ops
  {:= "eq" :< "lt" :> "gt" :<= "lte" :>= "gte" :contains-ci "containsAllTermsInOrder"})

(defn- wire-clause
  [[prop m]]
  (let [[op v] (first m)
        field  (name prop)]
    (case op
      :not= {:type "not" :value {:type "eq" :field field :value v}}
      :nil? {:type "isNull" :field field :value v}
      {:type (wire-ops op) :field field :value v})))

(defn watched-set
  [type-name {:keys [where]} target-of]
  (let [base  {:type "base" :objectType (name type-name)}
        bound (mapv (fn [c] [c (target-of (nth c 2 nil))]) where)
        in?   (fn [[[prop op] t]]
                (and (= :in (->kw op)) (coll? t) (seq t)
                     (or (keyword? prop) (string? prop)) (not= :* (->kw prop))))
        ins   (filterv in? bound)
        rest* (filterv (complement in?) bound)
        w     (when (seq rest*) (store-where (mapv first rest*) (mapv second rest*)))
        cs    (into (mapv wire-clause w)
                    (map (fn [[[prop] t]]
                           {:type "in" :field (name (->kw prop)) :value (vec (distinct t))}))
                    ins)]
    (if (and (seq cs) (or (empty? rest*) (some? w)))
      {:type "filter" :objectSet base
       :where (if (next cs) {:type "and" :value cs} (first cs))}
      base)))

(defn- temporal-extreme
  [better? prop rows]
  (some->> (keep #(let [x (prop-get (v %) prop)] (when-let [t (temporal x)] [t x])) rows)
           seq
           (reduce (fn [a b] (if (better? (compare (first b) (first a))) b a)))
           second))

(def agg-fns
  {:count          (fn [_ rows] (count rows))
   :distinct-count (fn [prop rows] (count (distinct (keep #(prop-get (v %) prop) rows))))
   :sum            (fn [prop rows] (reduce + 0 (keep #(calc (prop-get (v %) prop)) rows)))
   :avg            (fn [prop rows] (let [ns (keep #(calc (prop-get (v %) prop)) rows)]
                                     (if (seq ns) (/ (reduce + 0 ns) (count ns)) 0)))
   :min            (fn [prop rows]
                     (let [ns (keep #(calc (prop-get (v %) prop)) rows)]
                       (if (seq ns) (reduce min ns) (temporal-extreme neg? prop rows))))
   :max            (fn [prop rows]
                     (let [ns (keep #(calc (prop-get (v %) prop)) rows)]
                       (if (seq ns) (reduce max ns) (temporal-extreme pos? prop rows))))})

(defn text
  [x]
  (cond (nil? x)     ""
        (string? x)  x
        (keyword? x) (name x)
        (d/exact? x) (or (d/plain x) (str x))
        :else        (str x)))

(defn- blank* [x] (or (nil? x) (and (string? x) (str/blank? x))))

(defn- truthy*
  [x]
  (cond (nil? x)     false
        (boolean? x) x
        (string? x)  (not (str/blank? x))
        (d/exact? x) (not (zero? (or (calc x) 0)))
        (number? x)  (not (zero? x))
        :else        true))

(defn- same?
  [a b]
  (or (= a b)
      (if-let [[x y] (compare-operands a b)] (== x y) (= (text a) (text b)))))

(defn- cmp*
  [a b]
  (if-let [[x y] (compare-operands a b)] (compare x y) (compare (text a) (text b))))

(defn- against
  [f]
  (fn [[a & bs]] (boolean (and (seq bs) (every? #(f a %) bs)))))

(defn- members [a] (cond (nil? a) [] (coll? a) (seq a) :else [a]))

(defn- has? [arr x] (boolean (some #(same? % x) (members arr))))

(def ^:private day-ms 86400000)

(def ^:private unit-ms
  {:seconds 1000 :minutes 60000 :hours 3600000 :days day-ms :weeks (* 7 day-ms)})

(defn- unit [u] (or (->kw u) :days))

(defn- pad2 [n] (if (< n 10) (str "0" n) (str n)))

(defn- ms->ymd
  [ms]
  #?(:cljs (let [d (js/Date. ms)] [(.getUTCFullYear d) (inc (.getUTCMonth d)) (.getUTCDate d)])
     :clj  (let [d (.toLocalDate (.atZone (java.time.Instant/ofEpochMilli (long ms))
                                          java.time.ZoneOffset/UTC))]
             [(.getYear d) (.getMonthValue d) (.getDayOfMonth d)])))

(defn- ymd->ms
  [y m d]
  (let [t  (+ (* 12 y) (dec m))
        y' (quot (- t (mod t 12)) 12)
        m' (inc (mod t 12))]
    #?(:cljs (js/Date.UTC y' (dec m') d)
       :clj  (-> (java.time.LocalDate/of (int y') (int m') 1)
                 (.plusDays (dec (long d)))
                 (.atStartOfDay java.time.ZoneOffset/UTC) .toInstant .toEpochMilli))))

(defn- ymd->date [[y m d]] (str y "-" (pad2 m) "-" (pad2 d)))

(defn- ms->time
  [ms]
  #?(:cljs (.toISOString (js/Date. ms))
     :clj  (str (java.time.Instant/ofEpochMilli (long ms)))))

(def today-key :_today)

(defn with-today
  [data d]
  (if (contains? data today-key) data (assoc data today-key d)))

(defn- plus-months
  [ms n]
  (let [[y m d] (ms->ymd ms)
        days    (quot (- (ymd->ms y (+ m n 1) 1) (ymd->ms y (+ m n) 1)) day-ms)]
    (+ (ymd->ms y (+ m n) (min d days)) (mod ms day-ms))))

(defn- shift
  [ms n u]
  (let [n (long (or (calc n) 0)) u (unit u)]
    (case u
      :months (plus-months ms n)
      :years  (plus-months ms (* 12 n))
      (when-let [f (unit-ms u)] (+ ms (* n f))))))

(defn- months-between
  [a b]
  (let [[ya ma da] (ms->ymd a)
        [yb mb db] (ms->ymd b)
        raw        (- (+ (* 12 yb) mb) (+ (* 12 ya) ma))
        rest-a     (+ (* (dec da) day-ms) (mod a day-ms))
        rest-b     (+ (* (dec db) day-ms) (mod b day-ms))]
    (cond (and (pos? raw) (< rest-b rest-a)) (dec raw)
          (and (neg? raw) (> rest-b rest-a)) (inc raw)
          :else                              raw)))

(defn- between
  [ta tb u]
  (when (and ta tb)
    (case (unit u)
      :months (months-between ta tb)
      :years  (quot (months-between ta tb) 12)
      (when-let [f (unit-ms (unit u))] (quot (- tb ta) f)))))

(defn- day-of [x] (when-let [t (temporal x)] (- t (mod t day-ms))))

(def expr-ops
  {:+     (fn [xs]      (reduce + 0 (keep calc xs)))
   :-     (fn [[a & r]]
            (let [a (calc a)] (if (seq r) (reduce - (or a 0) (keep calc r)) (when a (- a)))))
   :*     (fn [xs]      (reduce * 1 (keep calc xs)))
   :/     (fn [[a & r]]
            (reduce (fn [acc b] (let [b (calc b)] (when (and acc b (not (zero? b))) (/ acc b))))
                    (calc a) r))
   :min   (fn [xs]      (when-let [ns (seq (keep calc xs))] (apply min ns)))
   :max   (fn [xs]      (when-let [ns (seq (keep calc xs))] (apply max ns)))
   :abs   (fn [[a]]
            (when-let [a (calc a)] #?(:cljs (js/Math.abs a) :clj (Math/abs (double a)))))
   :round (fn [[a p]]   (when-let [a (calc a)]
                          (let [f #?(:cljs (js/Math.pow 10 (or (calc p) 0))
                                     :clj  (Math/pow 10 (double (or (calc p) 0))))]
                            (/ #?(:cljs (js/Math.round (* a f))
                                  :clj  (double (Math/round (* (double a) f))))
                               f))))
   :str   (fn [xs]      (str/join (map text xs)))
   :pad   (fn [[x w]]   (let [t (text x)
                              w (long (or (calc w) 0))]
                          (if (< (count t) w)
                            (str (str/join (repeat (- w (count t)) "0")) t)
                            t)))
   :upper (fn [[a]]     (str/upper-case (text a)))
   :lower (fn [[a]]     (str/lower-case (text a)))
   :trim  (fn [[a]]     (str/trim (text a)))
   :if    (fn [[c a b]] (if (truthy* c) a b))
   :coalesce (fn [xs]   (some (fn [x] (when-not (blank* x) x)) xs))
   :=     (against same?)
   :not=  (against (complement same?))
   :>     (against #(pos? (cmp* %1 %2)))
   :<     (against #(neg? (cmp* %1 %2)))
   :>=    (against #(not (neg? (cmp* %1 %2))))
   :<=    (against #(not (pos? (cmp* %1 %2))))
   :and   (fn [xs]      (every? truthy* xs))
   :or    (fn [xs]      (boolean (some truthy* xs)))
   :not   (fn [[a]]     (not (truthy* a)))
   :blank? (fn [[a]]    (blank* a))
   :length (fn [[a]]    (cond (nil? a) 0 (sequential? a) (count a) :else 1))
   :contains         (fn [[arr & xs]] (every? #(has? arr %) xs))
   :does-not-contain (fn [[arr & xs]] (not-any? #(has? arr %) xs))
   :is-subset-of     (fn [[a b]] (every? #(has? b %) (members a)))
   :relative-date (fn [[x n u]] (some-> (day-of x) (shift n u) ms->ymd ymd->date))
   :relative-time (fn [[x n u]] (some-> (temporal x) (shift n u) ms->time))
   :between-dates (fn [[a b u]] (between (day-of a) (day-of b) u))
   :between-times (fn [[a b u]] (between (temporal a) (temporal b) u))})

(def pred-ops
  {:=         (fn [[a b]]  (= a b))
   :not=      (fn [[a b]]  (not= a b))
   :>         (fn [[a b]]  ((:> where-ops) a b))
   :<         (fn [[a b]]  ((:< where-ops) a b))
   :>=        (fn [[a b]]  ((:>= where-ops) a b))
   :<=        (fn [[a b]]  ((:<= where-ops) a b))
   :in        (fn [[a s]]  ((:in where-ops) a s))
   :contains? (fn [[a b]]  ((:contains where-ops) a b))
   :and       (fn [args]   (every? identity args))
   :or        (fn [args]   (boolean (some identity args)))
   :not       (fn [[a]]    (not a))
   :blank?    (fn [[a]]    (or (nil? a) (and (string? a) (str/blank? a))))
   :empty?    (fn [[a]]    (or (nil? a) (and (coll? a) (empty? a))
                               (and (string? a) (str/blank? a))))
   :truthy?   (fn [[a]]    ((:truthy where-ops) a nil))})

(defn- pred-form?
  [e]
  (and (vector? e) (let [h (first e)] (or (keyword? h) (string? h) (symbol? h)))))

(defn eval-pred
  [e]
  (if (pred-form? e)
    (let [[op & args] e]
      (if-let [f (get pred-ops (->kw op))]
        (boolean (f (mapv eval-pred args)))
        nil))
    (let [x (deref* e)]
      (if (or (nil? x) (number? x) (d/exact? x) (string? x) (keyword? x) (coll? x))
        x
        (boolean x)))))

(declare resolve-ref)

(defn- eval-node
  [e rv raw?]
  (cond
    (number? e) e
    (vector? e) (let [[op & args] e
                      op   (->kw op)
                      raw? (contains? raw-ops op)]
                  (if (= :today op)
                    (rv {:from today-key})
                    ((get expr-ops op (constantly nil))
                     (mapv #(eval-node % rv raw?) args))))
    (map? e)    (let [x (rv e)] (if raw? x (num x)))
    :else       (if raw? e (num e))))

(defn eval-expr
  [e rv]
  (eval-node e rv false))

(defn- clause-pred
  [prop f target]
  (fn [row]
    (cond
      (= :* prop)    (boolean (some #(f % target) (vals* (v row))))
      (vector? prop) (boolean (some #(f (row-prop row %) target) prop))
      :else          (f (row-prop row prop) target))))

(defn- geo-point
  [row prop]
  (if (vector? prop)
    (geo/point (mapv #(calc (row-prop row %)) prop))
    (geo/point (row-prop row prop))))

(defn- geo-target
  [ref rv]
  (if (map? ref)
    (update-vals ref #(if (vector? %) (mapv rv %) (rv %)))
    (rv ref)))

(defn- apply-where
  [rows clauses rv]
  (reduce (fn [rs [prop op ref]]
            (let [op (->kw op)]
              (cond
                (contains? row-ops op)
                (let [f (rv ref)] (filterv #(ofilter/matches? f %) rs))

                (geo/ops op)
                (let [target (geo-target ref rv)]
                  (if (geo/explain op prop target)
                    rs
                    (filterv #(geo/match? op (geo-point % prop) target) rs)))

                :else
                (let [f      (get where-ops op (:= where-ops))
                      target (rv ref)]
                  (filterv (clause-pred prop f target) rs)))))
          (vec rows) clauses))

(defn- shape-agg
  [h agg]
  (let [rows (:aggregations h)]
    (if (:group-by agg)
      (vec rows)
      (get (v (first rows)) (:name (first (:aggs agg)))))))

(defn- join-rows
  [rows {:keys [index on key as absent]} vv]
  (let [ns         (name index)
        by-k       (into {} (map (fn [r] [(str (get (v r) key)) (v r)])) (vv index))
        own        (set (some-> (first rows) v keys))
        qualify    (fn [m] (into {} (map (fn [[k val]] [(keyword ns (name k)) val])) m))
        plain-safe (fn [m] (into {} (remove (fn [[k _]] (contains? own k))) m))]
    (mapv (fn [row]
            (let [rec  (get by-k (str (get (v row) on)))
                  vals (if rec (if as (select-keys rec as) rec) (or absent {}))]
              (update row :values merge (qualify vals) (plain-safe vals))))
          rows)))

(declare var-value source-key)

(def ^:dynamic *pending*
  nil)

(def ^:dynamic *data-pending?* false)

(defn- note-pending! [h]
  (when (and *pending* (:loading? h)) (vreset! *pending* true)))

(def ^:dynamic *read-unanswered* nil)

(defn note-unanswered!
  []
  (when (and *read-unanswered* *data-pending?*) (vreset! *read-unanswered* true)))

(defn- data-get
  [data k]
  (if (contains? data k)
    (let [x (get data k)]
      (if-let [total (and (vector? x) (get-in data [:_truncated k]))]
        (vary-meta x assoc ::total total ::cut [k])
        x))
    (do (note-unanswered!) nil)))

(defn linked-key
  [of t]
  (keyword (str (name (->kw of)) ">" (name (->kw t)))))

(defn total-of [rows] (::total (meta rows)))

(defn cut-of [rows] (::cut (meta rows)))

(defn- source-set
  [data t]
  (data-get data (source-key (->kw t))))

(defn- spec-value
  [spec handles derived data]
  (let [rv (fn [r] (deref* (resolve-ref r handles derived data)))
        vv (fn [n] (var-value n handles derived data))
        of (fn [n] (or (if (or (get handles n) (get derived n)) (get data n) (data-get data n))
                       (vv n)))]
    (cond
      (nil? spec)      nil
      (:expr spec)     (eval-expr (:expr spec) rv)
      (:object-property spec)
      (let [rows (of (:of spec))]
        (row-prop (first rows) (:object-property spec)))
      (:coalesce spec) (some (fn [r] (let [x (rv r)] (when (some? x) x))) (:coalesce spec))
      (:pluck spec)    (into [] (comp (keep #(prop-get (v %) (:pluck spec)))
                                      (remove #(and (string? %) (str/blank? %)))
                                      (distinct))
                             (of (:of spec)))
      (:union spec)    (let [sets (into [] (keep #(let [x (rv %)] (when (sequential? x) x)))
                                        (:union spec))
                             rows (first (reduce (fn [[acc seen] r]
                                                   (let [k (or (:id r) r)]
                                                     (if (seen k)
                                                       [acc seen]
                                                       [(conj acc r) (conj seen k)])))
                                                 [[] #{}]
                                                 (apply concat sets)))
                             cut  (into [] (mapcat cut-of) sets)]
                         (cond-> rows (seq cut) (vary-meta assoc ::cut cut)))
      (:agg spec)      (let [[f prop] (:agg spec)
                             base (apply-where (of (:of spec))
                                               (:where spec) rv)]
                         ((get agg-fns (->kw f) (get agg-fns :sum)) prop base))
      :else            (let [f   (:from spec)
                             src (cond (:source spec) (source-set data (:source spec))
                                       (:linked spec) (data-get data
                                                                (if (:of spec)
                                                                  (linked-key (:of spec)
                                                                              (:linked spec))
                                                                  (->kw (:linked spec))))
                                       f (if (or (get handles f) (get derived f))
                                           (vv f) (data-get data f))
                                       :else ::keine-menge)]
                         (when-not (= ::keine-menge src)
                           (let [base (apply-where src (:where spec) rv)]
                             (if-let [j (:join spec)] (join-rows base j vv) base)))))))

(defn- var-value
  [nm handles derived data]
  (let [spec (get derived nm)]
    (if-let [h (get handles nm)]
      (cond
        (:aggregate spec) (let [h (deref* h)]
                            (note-pending! h)
                            (if-let [e (:error h)] (fn-error e) (shape-agg h (:aggregate spec))))
        (:function spec)  (let [h (deref* h)]
                            (note-pending! h)
                            (if-let [e (:error h)] (fn-error e) (vec (:rows h))))
        (:compare spec)   (deref* h)
        :else             (deref* h))
      (cond
        (contains? (:_refused data) (->kw nm))
        (fn-error (get-in data [:_refused (->kw nm)]))

        (and (or (:from spec) (:source spec) (:agg spec)) (contains? data (->kw nm)))
        (data-get data (->kw nm))

        (and (nil? spec) (contains? reserved-names (->kw nm)))
        (data-get data (->kw nm))

        :else (spec-value spec handles derived data)))))

(defn item-field
  [item k]
  (let [vs (v item)]
    (cond
      (contains? item k) (get item k)
      (contains? vs k)   (get vs k)
      :else              (do (note-unanswered!) nil))))

(defn- caught
  [f]
  (try (f)
       #?(:cljs (catch :default e (fn-error (or (some-> e .-message) (str e))))
          :clj  (catch Throwable e
                  (when (instance? InterruptedException e) (throw e))
                  (fn-error (or (.getMessage e) (str e)))))))

(defn- read-value
  [f form nm entry {::keys [handles derived data]}]
  {:value
   (case (:id f)
     :from            (if (or (get handles nm) (get derived nm))
                        (var-value nm handles derived data)
                        (data-get data nm))
     (:source :linked) (data-get data nm)
     :var             (get handles nm)
     :val             (caught #(var-value nm handles derived data))
     :spec            (caught #(spec-value form handles derived data))
     :item-not        (let [x (item-field entry nm)]
                        (not (and (some? x) ((:truthy where-ops) x nil))))
     (:item :arg :row) (item-field entry nm))})

(defn value-scope
  [handles derived data]
  {:vars      true
   :types     true
   :linked    true
   :specs     true
   :stateful? #(some? (get handles %))
   :read      read-value
   ::handles  handles
   ::derived  derived
   ::data     data})

(defn fill-scope
  [env x]
  (bind/with-fill {:read read-value} env x))

(defn resolved
  [form r]
  (cond (contains? r :value)                       (:value r)
        (= :not-filled (get-in r [:problem :rule])) form
        :else                                      nil))

(defn call-scope
  [scope]
  (dissoc scope :specs))

(defn resolve-call-args
  [args handles derived data]
  (let [scope (call-scope (value-scope handles derived data))]
    (walk/postwalk (fn [x]
                     (if (bind/reference? x)
                       (let [r (resolved x (bind/resolve x scope))]
                         (if (identical? r x) x (deref* r)))
                       x))
                   args)))

(defn resolve-ref
  [ref handles derived data]
  (resolved ref (bind/resolve ref (value-scope handles derived data))))

(defn state-default
  ([spec data] (state-default spec data nil))
  ([spec data nm]
   (if-let [[f prop of] (:default-agg spec)]
     (if (and nm (contains? data (->kw nm)))
       (get data (->kw nm))
       ((get agg-fns (->kw f) (get agg-fns :max)) prop (vec (get data of))))
     (:default spec))))

(defn var-refs
  [form]
  (let [acc (volatile! #{})]
    (walk/postwalk
     (fn [x]
       (when (and (map? x) (= 1 (count x)))
         (when-let [v (or (:val x) (:var x) (:from x) (:source x) (:linked x))]
           (when (or (keyword? v) (string? v)) (vswap! acc conj (->kw v)))))
       (when (and (map? x) (:widget x) (keyword? (:set x)))
         (vswap! acc conj (:set x)))
       (when (and (map? x) (some #{:object-property :agg :pluck} (keys x)))
         (let [v (:of x)]
           (when (or (keyword? v) (string? v)) (vswap! acc conj (->kw v)))))
       x)
     form)
    @acc))

(defn var-deps
  [spec]
  (into (into #{} (keep (fn [v] (when (or (keyword? v) (string? v)) (->kw v))))
              [(:of spec) (:from spec) (:source spec) (:linked spec)])
        (var-refs (dissoc spec :of :from :source))))

(defn- shown-tree
  [tree read-var]
  (letfn [(pick [x k]
            (let [panes (get x k)
                  sel   (:select x)
                  nm    (or (bind/name-in :var sel) (bind/name-in :val sel))
                  v     (when nm (read-var nm))]
              (if-let [one (when (and (some? v) (every? :id panes))
                             (or (some #(when (= (->kw (:id %)) (->kw v)) %) panes)
                                 (first panes)))]
                (assoc x k [one])
                x)))
          (narrow [x]
            (let [k (when (and (map? x) (not (walk/leaf-ref? x)))
                      (cond (vector? (:pages x)) :pages
                            (vector? (:tabs x))  :tabs))]
              (if k (pick x k) x)))]
    (walk/postwalk narrow tree)))

(defn all-var-specs
  [tree]
  (reduce (fn [m node] (if (map? (:vars node)) (merge m (:vars node)) m))
          {}
          (tree/nodes tree)))

(defn live-vars
  [tree specs read-var]
  (loop [todo (seq (var-refs (shown-tree tree read-var))) seen #{}]
    (if-let [nm (first todo)]
      (if (seen nm)
        (recur (next todo) seen)
        (recur (concat (next todo) (var-deps (get specs nm))) (conj seen nm)))
      seen)))

(defn- mirror-interface
  [x handles derived data]
  (cond-> x
    (map? (:interface x))
    (update :interface update-vals
            (fn [r] (if (and (bind/variable-ref? r) (contains? r :val))
                      {::mirror (resolve-ref r handles derived data)}
                      r)))))

(def ^:private opaque-params {:vega-chart #{:spec :theme}})

(defn- opaque?
  [x k]
  (and (contains? (get opaque-params (->kw (:widget x))) k)
       (not (bind/variable-ref? (get x k)))))

(defn resolve-scoped
  [tree handles derived data]
  (walk/walk-tree (fn [x]
               (cond (and (map? x) (:vars x)) [x]
                     (or (bind/variable-ref? x) (bind/anon-spec? x))
                     [(resolve-ref x handles derived data)]
                     (and (map? x) (or (contains? x :widget) (= :loop (->kw (:kind x))))
                          (not (contains? x ::pending?)))
                     [(let [p (volatile! false)
                            q (volatile! false)
                            x (mirror-interface x handles derived data)
                            r (binding [*pending* p *read-unanswered* q]
                                (resolve-scoped (assoc x ::pending? false) handles derived data))]
                        (cond-> (if @p (assoc r ::pending? true) (dissoc r ::pending?))
                          @q (assoc ::data-pending? true)))]))
             opaque?
             tree))

(defn handle?
  [x]
  #?(:cljs (satisfies? IDeref x) :clj (instance? clojure.lang.IDeref x)))

(defn interface-bindings
  [vars interface]
  (let [slots (interface-slots vars)]
    (reduce-kv
     (fn [acc ext v]
       (let [nm (get slots ext)]
         (cond
           (nil? nm)
           (update acc :problems conj
                   (str "app/object-view: :interface names " (pr-str ext)
                        ", which is no :external-id of this view's variables"
                        (when (seq slots)
                          (str " — it offers " (str/join ", " (map pr-str (sort (keys slots))))))))

           (and (map? v) (contains? v ::mirror))
           (assoc-in acc [:mirror nm] (::mirror v))

           (not (handle? v))
           (update acc :problems conj
                   (str "app/object-view: :interface " (pr-str ext)
                        " must be bound to a module variable (`{:var …}`, or `{:val …}`"
                        " for a derived one) — a mapped"
                        " variable is BACKED by the host's definition, so a literal"
                        " cannot stand in for it"))

           :else (assoc-in acc [:bind nm] v))))
     {:bind {} :problems []}
     interface)))

(defn folds-payload-at-build?
  [tree]
  (boolean
   (some (fn [x] (and (:vars x) (some :default-agg (vals (:vars x)))))
         (tree/nodes tree))))

(defn link-key
  [api-name]
  (keyword "link" (name api-name)))

(defn source-key
  [type-name]
  (keyword "source" (name type-name)))

(defn object-view-problems
  [around]
  (for [[nm why] (:_refused around)]
    (str "app/object-view: " (name nm) " was not answered — " why)))

(defn object-view-payload
  [around row]
  (let [t   (->kw (:object-type around))
        row (or (:object around) row)]
    (cond-> (-> (or (:objects around) {})
                (into (map (fn [[k rows]] [(link-key k) (vec rows)]))
                      (:links around))
                (into (:answers around))
                (as-> m (into m (map (fn [[nm holder]] [(->kw nm) (get m (->kw holder))]))
                              (:answers-same around))))
      (and t row)          (assoc t [row] view-object [row])
      (:_truncated around) (assoc :_truncated (:_truncated around))
      (:_refused around)   (assoc :_refused (:_refused around)))))
