(ns ent-lsp.diff
  (:require [babashka.process :as p]
            [clojure.string :as str]
            [ent-lsp.analysis :as an]
            [ent-lsp.workspace :as ws]))

(defn- git [root & args]
  (let [r (apply p/shell {:dir root :out :string :err :string :continue true} "git" args)]
    (when (zero? (:exit r)) (:out r))))

(defn index-at
  [ws rev]
  (let [root (:root ws)]
    (when-let [out (git root "ls-tree" "-r" "--name-only" rev "--" ".")]
      (an/build-index (:spec ws)
                      (into {} (for [rel (str/split-lines out)
                                     :when (ws/in-scope? ws rel)
                                     :let [text (git root "show" (str rev ":./" rel))]
                                     :when text]
                                 [rel (ws/facts-of ws rel text)]))))))

(def ^:private watched
  #{:object :link :action :interface :shared-property :value-type :property :dataset :source})

(def ^:private top {:row 1 :col 1 :end-row 1 :end-col 2})

(defn findings [spec old new base]
  (let [k      (juxt :kind :name :object)
        live   (fn [idx] (into {} (for [d (:defs idx) :when (watched (:kind d))] [(k d) d])))
        o      (live old)
        n      (live new)
        object (fn [nm] (n [:object nm nil]))]
    (concat
     (for [[key d] o
           :when (not (n key))
           :when (not (and (:object d) (not (object (:object d)))))
           :let [at (if (:object d) (object (:object d)) d)
                 st (when-not (:object d) (an/status-of spec d))
                 protected? (contains? (get-in spec [:status :protected]) st)]]
       {:rule (if protected? :entfernt-geschuetzt :entfernt)
        :file (:file at) :loc (if (= at d) top (:name-loc at))
        :vars {:kind (name (:kind d))
               :name (str (when (:object d) (str (an/show (:object d)) " "))
                          (an/show (:name d)))
               :status (an/show st)
               :base base}})
     (for [[[kind nm] d] n
           :when (and (= :object kind) (o [:object nm nil]))
           :let [was (an/primary-key spec old nm) now (an/primary-key spec new nm)]
           :when (and was now (not= was now))]
       {:rule :pk-geaendert :file (:file d) :loc (:name-loc d)
        :vars {:name (an/show nm) :old (str/join ", " (map an/show was))
               :new (str/join ", " (map an/show now))}})
     (for [[key d] n
           :when (= :property (:kind d))
           :let [prev (o key)
                 was (get-in prev [:data :type]) now (get-in d [:data :type])]
           :when (and was now (not= was now))]
       {:rule :typ-geaendert :file (:file d) :loc (:name-loc d)
        :vars {:name (an/show (:name d)) :object (an/show (:object d))
               :old (an/show was) :new (an/show now)}}))))
