(ns sutra.types.feedback-szenario
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.kit.decisions :as decisions]))

(defn- art? [k e] (= (name k) (some-> (:art e) name)))

(defn fehler-und-klick
  [verlauf ereignis-id]
  (let [v (vec (sort-by :t verlauf))
        i (last (keep-indexed (fn [i e]
                                (when (and (art? :fehler e)
                                           (or (nil? ereignis-id)
                                               (= (str ereignis-id) (str (:id e)))))
                                  i))
                              v))]
    (when i
      {:fehler (v i)
       :klick  (last (filter #(art? :klick %) (subvec v 0 i)))})))

(defn letzter-klick
  [verlauf ereignis-id]
  (last (filter #(and (art? :klick %)
                      (or (nil? ereignis-id) (= (str ereignis-id) (str (:id %)))))
                (sort-by :t verlauf))))

(defn entscheidungen-von
  [klick]
  (or (try (some-> (:entscheidungen klick) decisions/read-text vec)
           (catch #?(:clj Exception :cljs :default) _ nil))
      []))

(defn klick-szenario
  [{:keys [sitzung verlauf ereignis]}]
  (let [klick (or (letzter-klick verlauf ereignis)
                  (refusal/refuse! :refusal.types/no-click-in-session {:sitzung sitzung}))
        ents  (entscheidungen-von klick)]
    (when (empty? ents)
      (refusal/refuse! :refusal.types/click-without-view-decision
                       {:sitzung sitzung :ereignis (:id klick)}))
    {:id             (str sitzung "/" (:id klick))
     :sitzung        sitzung
     :ereignis       (:id klick)
     :aufgezeichnet  {:t (:t klick)
                      :klick (select-keys klick [:id :t :ort :ziel :knoten :selektor])}
     :entscheidungen ents
     :annahmen       ["nur die Ansicht: welche Seite der Schirm zeigte, nicht, was der Klick tat"]}))

(defn anzeigename
  [text]
  (second (re-find #"^„([^“]+)“ nicht ausgeführt" (str text))))

(defn server-text
  [text]
  (or (second (re-find #" nicht ausgeführt — (.+)$" (str text))) (str text)))

(defn aktion-von
  [fehler actions]
  (if-let [nm (or (get-in fehler [:absage :aktion])
                  (get-in fehler [:absage :ausloeser :knoten :aktion])
                  (get-in fehler [:knoten :aktion]))]
    (keyword nm)
    (let [anz      (anzeigename (:text fehler))
          treffer  (for [[k a] actions :when (and anz (= anz (:display-name a)))] k)]
      (when (= 1 (count treffer)) (first treffer)))))

(defn- typ-kw [t] (some-> t name keyword))

(defn- objekt-param? [typen p] (contains? typen (typ-kw (:type p))))

(defn- gets [form]
  (filter #(and (vector? %) (= :get (first %))) (tree-seq coll? seq form)))

(def ^:private regel-ops #{:create :update :delete})

(defn bezuege
  [aktion typen]
  (let [params (:params aktion)
        typ-von (fn [p] (when (objekt-param? typen (get params p)) (typ-kw (:type (get params p)))))
        aus-params (for [[_ p] params :when (objekt-param? typen p)] [(typ-kw (:type p)) nil])
        aus-gets   (for [[_ p a] (gets aktion) :let [t (typ-von p)] :when (and t (keyword? a))]
                     [t a])
        aus-regeln (for [{:keys [op object-type values set]} (:rules aktion)
                         :when (and (regel-ops op) (contains? typen (typ-kw object-type)))
                         a (into (vec (keys values)) set)]
                     [(typ-kw object-type) a])
        regel-typen (for [{:keys [op object-type]} (:rules aktion)
                          :when (and (regel-ops op) (contains? typen (typ-kw object-type)))]
                      [(typ-kw object-type) nil])]
    (reduce (fn [m [t a]] (update m t (fnil #(cond-> % a (conj a)) #{})))
            {}
            (concat aus-params regel-typen aus-gets aus-regeln))))

(defn- feld-typ [s]
  (let [s (str s)]
    (if (str/starts-with? s "[") (edn/read-string s) (keyword s))))

(defn ersatz-typ
  [t {:keys [primary-key title fields]} attribute]
  (let [pk     (keyword (or primary-key "id"))
        titel  (some-> title keyword)
        typ-an (into {} (map (fn [{n :name ty :type}] [(keyword n) (feld-typ ty)])) fields)
        namen  (cond-> (conj (set attribute) pk) (typ-an titel) (conj titel))]
    {:name        (name t)
     :plural      (str (name t) "-ersatz")
     :primary-key pk
     :match-keys  [pk]
     :title       (if (typ-an titel) titel pk)
     :attributes  (vec (for [a (sort-by name namen)] [a (get typ-an a :string)]))}))

(defn- anlegen [t] (keyword (str "ersatz-" (name t) "-anlegen")))

(defn- anlege-aktion [t {:keys [primary-key attributes]}]
  {:display-name (str "Ersatz " (name t))
   :params {primary-key {:type (or (some (fn [[a ty]] (when (= a primary-key) ty)) attributes)
                                   :string)
                         :name (name primary-key)
                         :required true}}
   :rules [{:op :create :object-type t :values {primary-key [:get primary-key]}}]})

(defn- ersatz-wert [typ laenge]
  (case (typ-kw typ)
    (:decimal :integer :long) 1
    :double                   1.0
    :boolean                  true
    :date                     "2026-01-01"
    :timestamp                "2026-01-01T00:00:00Z"
    (apply str (repeat (max 1 (or laenge 1)) "x"))))

(def ^:private behaltene-felder
  [:display-name :description :params :rules :submission-criteria :form :status])

(defn- aktion-fuer-welt
  [aktion typen]
  (let [weltlich? (fn [{:keys [op object-type]}]
                    (and (regel-ops op) (contains? typen (typ-kw object-type))))]
    {:aktion (-> (select-keys aktion behaltene-felder)
                 (assoc :rules (vec (filter weltlich? (:rules aktion)))))
     :ausgelassen (vec (concat
                        (for [r (:rules aktion) :when (not (weltlich? r))]
                          (str "Regel " (pr-str (select-keys r [:op :object-type :link-type
                                                                :function]))))
                        (for [k (sort (remove (set behaltene-felder) (keys aktion)))]
                          (str "Feld " k))))}))

(defn- param-am-ziel
  [params klick]
  (or (some-> (get-in klick [:knoten :param]) keyword)
      (let [ziel (some-> (get-in klick [:ziel :name]) (str/replace #"…$" "") str/trim)]
        (when (seq ziel)
          (some (fn [[k p]] (when (= ziel (:name p)) k)) params)))))

(defn- aus-form
  [spec form]
  (if (map? form)
    (cond (:id form)     (str (:id form))
          (:laenge form) (ersatz-wert (:type spec) (:laenge form))
          (:anzahl form) (vec (repeat (:anzahl form) (ersatz-wert (:type spec) nil))))
    (case (some-> form name keyword)
      :zahl    1
      :ja-nein true
      nil)))

(defn szenario
  [{:keys [sitzung verlauf ereignis actions typen]}]
  (let [{:keys [fehler klick]} (fehler-und-klick verlauf ereignis)
        _ (when-not fehler
            (refusal/refuse! :refusal.types/no-error-in-session
                             {:sitzung sitzung :ereignis ereignis}))
        aname  (or (aktion-von fehler actions)
                   (refusal/refuse! :refusal.types/no-unique-action {:text (:text fehler)}))
        aktion (or (get actions aname)
                   (refusal/refuse! :refusal.types/action-in-no-text {:aktion aname}))
        typen  (update-keys typen typ-kw)
        params (:params aktion)
        bez    (bezuege aktion typen)
        ersatz (into {} (for [[t as] bez] [t (ersatz-typ t (get typen t) as)]))
        absage (:absage fehler)
        fehlt  (set (map keyword (:fehlt absage)))
        objekt (get-in klick [:ziel :objekt])
        aktuell (for [[k p] params :when (= :current-object (typ-kw (:prefill p)))] [k p])
        am-ziel (param-am-ziel params klick)
        gesendet (:gesendet absage)
        bindung (if gesendet
                  (for [[p form] gesendet
                        :let [k (keyword (name p)) v (aus-form (get params k) form)]
                        :when (some? v)]
                    [k v])
                  (concat
                   (for [[k p] aktuell
                         :when (and objekt (= (typ-kw (:typ objekt)) (typ-kw (:type p))))]
                     [k (str (:id objekt))])
                   (when-let [p (and am-ziel (get params am-ziel))]
                     [[am-ziel (if (objekt-param? typen p)
                                 (str "ersatz-" (name am-ziel))
                                 (ersatz-wert (:type p) nil))]])))
        args   (into (sorted-map) (remove (comp fehlt first)) bindung)
        saat   (vec (for [[k v] args :let [t (typ-kw (:type (get params k)))]
                          :when (contains? ersatz t)]
                      [(anlegen t) {(:primary-key (get ersatz t)) v}]))
        {welt-aktion :aktion ausgelassen :ausgelassen} (aktion-fuer-welt aktion typen)
        stelle (first (:server absage))]
    {:id          (str sitzung "/" (:id fehler))
     :sitzung     sitzung
     :ereignis    (:id fehler)
     :revision    (:revision fehler)
     :aufgezeichnet (cond-> {:t (:t fehler) :text (:text fehler)}
                      klick (assoc :klick (select-keys klick [:id :t :ziel :knoten :selektor]))
                      absage (assoc :absage (dissoc absage :spur)))
     :aktion      aname
     :welt        {:schema {:objects ersatz
                            :actions (into {aname welt-aktion}
                                           (for [[t e] ersatz] [(anlegen t) (anlege-aktion t e)]))}
                   :saat saat}
     :schritte    [{:aktion aname
                    :args args
                    :erwartet (cond-> {:ok? false :text (server-text (:text fehler))}
                                (:status absage) (assoc :status (:status absage))
                                stelle (assoc :wirft-in (select-keys stelle [:datei :fn])))}]
     :annahmen    (vec (concat
                        (when gesendet
                          [(str "Argumente laut Absage (:gesendet): Ids echt, Texte Ersatz in"
                                " ihrer Länge")])
                        (for [[k p] aktuell
                              :when (and (not gesendet) (not (contains? args k)))]
                          (str (name k) " (:prefill :current-object) blieb leer: geklickt "
                               (if objekt (str "in " (:typ objekt)) "in keinem Objekt")
                               ", die Aktion erwartet " (name (typ-kw (:type p)))))
                        (when (and am-ziel (not gesendet))
                          [(str (name am-ziel) " ist das Feld des letzten Klicks; Wert: Ersatz")])
                        (when (seq fehlt)
                          [(str "fehlt laut Absage: " (str/join ", " (map name (sort fehlt))))])
                        (when-not absage
                          ["ältere Sitzung ohne Absage: Status und Serverstelle unbekannt"])
                        ["nachgestellt über die Aktions-Schnittstelle (/api/actions), nicht über die Route der App"]))
     :entscheidungen (entscheidungen-von klick)
     :ausgelassen ausgelassen}))
