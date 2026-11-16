(ns sutra.capps.editing
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.capps.catalog :as cat]
            [sutra.capps.tree-move :as tm]))

(def blank
  {:tree nil :file nil :cursor [] :clip nil :past [] :future []
   :mode :idle :pending nil :coalescing nil})

(defn draft
  [file]
  (assoc blank :file file :tree (:layout file)))

(def history-limit 60)

(def ^:private step-keys [:tree :file :cursor])

(defn node
  ([editor] (node editor (:cursor editor)))
  ([editor path] (cat/node-at (:tree editor) (vec path))))

(def node? tm/node?)

(defn- push
  [editor label tag]
  (-> editor
      (update :past (fn [p] (vec (take-last history-limit
                                            (conj (vec p)
                                                  (assoc (select-keys editor step-keys)
                                                         :label label :tag tag))))))
      (assoc :future [])))

(defn can-undo? [editor] (boolean (seq (:past editor))))
(defn can-redo? [editor] (boolean (seq (:future editor))))
(defn undo-label [editor] (:label (peek (vec (:past editor)))))
(defn redo-label [editor] (:label (first (:future editor))))

(defn- undo*
  [editor]
  (if-let [prev (peek (vec (:past editor)))]
    (-> editor
        (assoc :coalescing nil)
        (merge (select-keys prev step-keys))
        (update :past (comp vec butlast))
        (update :future (fn [f] (vec (cons (assoc (select-keys editor step-keys)
                                                  :label (:label prev) :tag (:tag prev))
                                           f)))))
    editor))

(defn- redo*
  [editor]
  (if-let [nxt (first (:future editor))]
    (-> editor
        (assoc :coalescing nil)
        (merge (select-keys nxt step-keys))
        (update :future (comp vec rest))
        (update :past (fn [p] (vec (take-last history-limit
                                              (conj (vec p)
                                                    (assoc (select-keys editor step-keys)
                                                           :label (:label nxt)
                                                           :tag (:tag nxt))))))))
    editor))

(def modes
  {:idle     {:cancel :idle :add-start :adding :drag-start :dragging
              :menu-open :menu :type-start :typing}
   :adding   {:cancel :idle :commit :idle}
   :dragging {:cancel :idle :commit :idle}
   :menu     {:cancel :idle :commit :idle :menu-open :menu}
   :typing   {:cancel :idle :commit :idle :type-start :typing}})

(defn transition
  ([editor event] (transition editor event nil))
  ([editor event pending]
   (if-let [m (get-in modes [(:mode editor :idle) event])]
     (assoc editor :mode m :pending (when-not (= :idle m) pending) :coalescing nil)
     editor)))

(def needs
  {:any       {:label "immer" :ok (fn [_] true)}
   :tree      {:label "eine geladene App" :ok (fn [editor] (some? (:tree editor)))}
   :node      {:label "einen gewählten Baustein"
               :ok (fn [editor] (node? (node editor)))}
   :child     {:label "einen gewählten Baustein unter der Wurzel"
               :ok (fn [editor] (and (seq (:cursor editor)) (node? (node editor))))}
   :widget    {:label "einen gewählten Baustein (kein Behälter)"
               :ok (fn [editor] (= :widget (cat/node-type (node editor))))}
   :container {:label "einen gewählten Behälter"
               :ok (fn [editor] (= :container (cat/node-type (node editor))))}
   :indexed   {:label "einen Baustein in einer Reihe"
               :ok (fn [editor] (and (integer? (peek (vec (:cursor editor)))) (node? (node editor))))}
   :clip      {:label "etwas in der Ablage" :ok (fn [editor] (some? (:clip editor)))}
   :undoable  {:label "einen Schritt zum Zurücknehmen" :ok can-undo?}
   :redoable  {:label "einen zurückgenommenen Schritt" :ok can-redo?}})

(defn- append
  [tree parent slot n]
  (let [t (cat/add-child tree parent slot n)]
    (when-not (= t tree)
      (let [spec (some #(when (= slot (:key %)) %)
                       (cat/child-slots (cat/node-at t parent)))
            kids (cat/slot-children (cat/node-at t parent) spec)]
        {:tree t :cursor (if-let [k (last kids)]
                           (into (vec parent) (:path k))
                           (vec parent))}))))

(defn- entries [editor] (if (:tree editor) (cat/flatten-tree (:tree editor)) []))

(defn- cursor-index
  [editor]
  (let [c (vec (:cursor editor))]
    (first (keep-indexed (fn [i e] (when (= c (:path e)) i)) (entries editor)))))

(defn- goto
  [editor i]
  (let [es (entries editor)]
    (when-let [e (get es i)]
      (assoc editor :cursor (vec (:path e))))))

(defn target
  [editor]
  (tm/add-target (:tree editor) (:cursor editor)))

(defn- drop-at
  [editor path zone]
  (if zone
    (tm/target (:tree editor) path zone)
    (tm/add-target (:tree editor) path)))

(defn retype
  [n widget]
  (let [fresh (cat/blank-node widget)
        keeps (into #{:vars} (map :key) (cat/node-params fresh))]
    (merge fresh (select-keys n (disj keeps :widget :kind)))))

(def intents
  [{:id :select :label "Wählen" :group :auswahl :needs :tree
    :hint "Den Baustein zum Bearbeiten bestimmen — ändert den Entwurf nicht."
    :do (fn [editor {:keys [path]}] (assoc editor :cursor (vec path)))}

   {:id :cursor-next :label "Nächster Baustein" :group :auswahl :needs :tree
    :keys ["ArrowDown"]
    :do (fn [editor _] (goto editor (inc (or (cursor-index editor) -1))))}

   {:id :cursor-prev :label "Voriger Baustein" :group :auswahl :needs :tree
    :keys ["ArrowUp"]
    :do (fn [editor _] (when-let [i (cursor-index editor)] (goto editor (dec i))))}

   {:id :cursor-out :label "Zum Behälter darüber" :group :auswahl :needs :child
    :keys ["ArrowLeft"]
    :do (fn [editor _] (assoc editor :cursor (tm/node-path-up (:tree editor) (:cursor editor))))}

   {:id :cursor-in :label "In den Behälter hinein" :group :auswahl :needs :container
    :keys ["ArrowRight"]
    :do (fn [editor _]
          (when-let [c (first (cat/children-of (node editor)))]
            (assoc editor :cursor (into (vec (:cursor editor)) (:path c)))))}

   {:id :add :label "Baustein einfügen" :group :struktur :needs :tree :step true
    :hint "Legt die gewählte Art ab: in einen genannten Slot, in eine Zone des
           genannten Pfads, oder dort, wohin `target` zeigt."
    :do (fn [editor {:keys [kind path slot zone]}]
          (when kind
            (let [node (cat/blank-node kind)
                  p    (vec (or path (:cursor editor)))]
              (if slot
                (when-let [{:keys [tree cursor]} (append (:tree editor) p slot node)]
                  (assoc editor :tree tree :cursor cursor))
                (when-let [res (tm/put (:tree editor) (drop-at editor p zone) node)]
                  (assoc editor :tree (:tree res) :cursor (:path res)))))))}

   {:id :remove :label "Entfernen" :group :struktur :needs :child :step true :menu true
    :keys ["Delete" "Backspace"]
    :do (fn [editor _]
          (when-let [h (tm/handle (:tree editor) (:cursor editor))]
            (assoc editor
                   :tree (cat/remove-at (:tree editor) h)
                   :cursor (tm/node-path-up (:tree editor) (:cursor editor)))))}

   {:id :duplicate :label "Verdoppeln" :group :struktur :needs :child :step true :menu true
    :keys ["ctrl+d"]
    :do (fn [editor _]
          (when-let [[parent slot] (tm/parent-slot (:cursor editor))]
            (when-let [{:keys [tree cursor]} (append (:tree editor) parent slot (node editor))]
              (assoc editor :tree tree :cursor cursor))))}

   {:id :move-up :label "Nach oben" :group :struktur :needs :indexed :step true :menu true
    :keys ["alt+ArrowUp"]
    :do (fn [editor _]
          (let [p (vec (:cursor editor))
                t (cat/move-at (:tree editor) p -1)]
            (when-not (= t (:tree editor))
              (assoc editor :tree t :cursor (conj (vec (butlast p)) (dec (peek p)))))))}

   {:id :move-down :label "Nach unten" :group :struktur :needs :indexed :step true :menu true
    :keys ["alt+ArrowDown"]
    :do (fn [editor _]
          (let [p (vec (:cursor editor))
                t (cat/move-at (:tree editor) p 1)]
            (when-not (= t (:tree editor))
              (assoc editor :tree t :cursor (conj (vec (butlast p)) (inc (peek p)))))))}

   {:id :move-to :label "Verschieben" :group :struktur :needs :tree :step true
    :hint "Die Zone entscheidet, ob davor, dahinter oder hinein — `tm/plan`
           sagt es vorher, ohne zu verschieben."
    :do (fn [editor {:keys [src tgt zone]}]
          (when-let [res (tm/move (:tree editor) (vec src) (vec tgt) (or zone :into))]
            (assoc editor :tree (:tree res) :cursor (:path res))))}

   {:id :retype :label "Art wechseln" :group :struktur :needs :widget :step true
    :do (fn [editor {:keys [widget]}]
          (when widget
            (assoc editor :tree (cat/set-node (:tree editor) (:cursor editor)
                                          (retype (node editor) widget)))))}

   {:id :copy :label "Kopieren" :group :ablage :needs :child :menu true
    :keys ["ctrl+c"]
    :do (fn [editor _]
          (let [n (node editor)]
            (assoc editor :clip {:node n :label (cat/node-label n)})))}

   {:id :cut :label "Ausschneiden" :group :ablage :needs :child :step true :menu true
    :keys ["ctrl+x"]
    :do (fn [editor _]
          (when-let [h (tm/handle (:tree editor) (:cursor editor))]
            (let [n (node editor)]
              (assoc editor
                     :clip {:node n :label (cat/node-label n)}
                     :tree (cat/remove-at (:tree editor) h)
                     :cursor (tm/node-path-up (:tree editor) (:cursor editor))))))}

   {:id :paste :label "Einfügen" :group :ablage :needs :clip :step true :menu true
    :keys ["ctrl+v"]
    :do (fn [editor {:keys [path zone]}]
          (when-let [res (tm/put (:tree editor)
                                 (drop-at editor (vec (or path (:cursor editor))) zone)
                                 (:node (:clip editor)))]
            (assoc editor :tree (:tree res) :cursor (:path res))))}

   {:id :put-param :label "Parameter setzen" :group :parameter :needs :node :step true
    :tag (fn [editor {:keys [key path]}] [:param (vec (or path (:cursor editor))) key])
    :hint "Tippen verschmilzt zu EINEM Schritt — sonst frisst ein Titel den Verlauf."
    :do (fn [editor {:keys [key value path]}]
          (when key
            (assoc editor :tree (cat/update-node (:tree editor) (vec (or path (:cursor editor)))
                                             #(if (nil? value) (dissoc % key)
                                                (assoc % key value))))))}

   {:id :replace-node :label "Baustein ersetzen" :group :parameter :needs :node :step true
    :do (fn [editor {:keys [node path]}]
          (assoc editor :tree (cat/set-node (:tree editor) (vec (or path (:cursor editor))) node)))}

   {:id :put-var :label "Variable setzen" :group :variablen :needs :tree :step true
    :tag (fn [editor {:keys [name path]}] [:var (vec (or path (:cursor editor))) name])
    :do (fn [editor {:keys [name spec path]}]
          (when name
            (assoc editor :tree (cat/update-node (:tree editor) (vec (or path (:cursor editor)))
                                             #(assoc-in % [:vars name] spec)))))}

   {:id :bind-new-var :label "Parameter an neue Variable binden"
    :group :variablen :needs :node :step true
    :hint "Variable anlegen und Parameter darauf zeigen lassen — EIN Schritt,
           sonst nimmt ein Undo nur die Hälfte zurück."
    :do (fn [editor {:keys [key ref name spec path]}]
          (when (and key ref name)
            (assoc editor :tree (-> (:tree editor)
                                (assoc-in [:vars name] spec)
                                (cat/update-node (vec (or path (:cursor editor)))
                                                 #(assoc % key {ref name}))))))}

   {:id :put-file-key :label "App-Angabe setzen" :group :app :needs :any :step true
    :tag (fn [_ {:keys [key]}] [:file key])
    :do (fn [editor {:keys [key value]}]
          (when key
            (assoc editor :file (if (or (nil? value) (and (coll? value) (empty? value)))
                              (dissoc (:file editor) key)
                              (assoc (:file editor) key value)))))}

   {:id :rename-var :label "Variable umbenennen" :group :variablen :needs :tree :step true
    :hint "Bei Kollision wird eine Nummer angehängt, verglichen ohne Gross- und
           Kleinschreibung — die andere Deklaration wird NIE überschrieben, sonst
           verschwindet sie beim Tippen."
    :do (fn [editor {:keys [from to path]}]
          (when (and from to (not= from to))
            (let [frei (cat/var-slug (name to)
                                     (remove #{from} (cat/declared-vars (:tree editor))))]
              (assoc editor :tree
                     (cat/update-node (:tree editor) (vec (or path (:cursor editor)))
                                      (fn [n] (update n :vars
                                                      #(into {} (map (fn [[k v]]
                                                                       [(if (= k from) frei k) v]))
                                                             %))))))))}

   {:id :drop-var :label "Variable löschen" :group :variablen :needs :tree :step true
    :refuse (fn [editor {:keys [name]}]
              (when (and name (contains? (cat/referenced-vars (:tree editor)) name))
                (str "Die Variable " name " wird noch gelesen — erst die Leser"
                     " umstellen, sonst zeigt ein Baustein ins Leere.")))
    :do (fn [editor {:keys [name path]}]
          (when name
            (assoc editor :tree (cat/update-node (:tree editor) (vec (or path (:cursor editor)))
                                             #(update % :vars dissoc name)))))}

   {:id :undo :label "Zurücknehmen" :group :verlauf :needs :undoable
    :keys ["ctrl+z"]
    :do (fn [editor _] (undo* editor))}

   {:id :redo :label "Wiederholen" :group :verlauf :needs :redoable
    :keys ["ctrl+shift+z" "ctrl+y"]
    :do (fn [editor _] (redo* editor))}])

(def intent-index (coll/index-by :id intents))

(def group-labels
  {:auswahl "Auswahl" :struktur "Struktur" :ablage "Ablage"
   :parameter "Parameter" :variablen "Variablen" :app "App" :verlauf "Verlauf"})

(def by-key
  (into {} (for [in intents, k (:keys in)] [k (:id in)])))

(defn chord
  [{:keys [key ctrl alt shift meta]}]
  (let [k (str key)]
    (str (when (or ctrl meta) "ctrl+") (when alt "alt+") (when shift "shift+")
         (if (= 1 (count k)) (str/lower-case k) k))))

(defn possible?
  [editor id]
  (boolean (when-let [in (get intent-index id)]
             ((get-in needs [(:needs in) :ok] (constantly false)) editor))))

(defn available
  [editor]
  (filterv #(possible? editor (:id %)) intents))

(defn blocked-because
  [editor id]
  (when-not (possible? editor id)
    (get-in needs [(:needs (get intent-index id)) :label])))

(defn refusal
  ([editor id] (refusal editor id nil))
  ([editor id args]
   (or (blocked-because editor id)
       (when-let [f (:refuse (get intent-index id))] (f editor args)))))

(defn run
  ([editor id] (run editor id nil))
  ([editor id args]
   (if (some? (refusal editor id args))
     editor
     (let [in  (get intent-index id)
           nxt ((:do in) editor args)]
       (cond
         (nil? nxt) editor
         (not (:step in)) nxt
         (= (select-keys nxt [:tree :file]) (select-keys editor [:tree :file])) nxt
         :else (let [tag   (when-let [f (:tag in)] (f editor args))
                     weiter? (and (some? tag) (= tag (:coalescing editor)))
                     based (if weiter? editor (push editor (:label in) tag))]
                 (merge nxt (select-keys based [:past :future]) {:coalescing tag})))))))
