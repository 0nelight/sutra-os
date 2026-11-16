(ns sutra.parser.checkout
  (:require [sutra.kit.lifecycle :as lc]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree])
  (:import (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(def lifecycle
  (let [pulled  (fn [_ {:keys [rehome?]}] (if rehome? :edited :clean))
        any     #{:absent :clean :edited :conflicted :staged :orphaned}
        usable  #{:clean :edited :staged}]
    (lc/table
     {:name   :checkout
      :states any
      :final  #{}
      :edges  {:pull          {:from #{:absent :clean :staged} :to pulled
                               :guard (fn [_ {:keys [stop]}]
                                        (when (seq stop)
                                          (str "would overwrite "
                                               (apply str (interpose ", " stop))
                                               " — changed since its pull, or written by no pull; "
                                               "push it first, or pull --force")))}
               :pull-force    {:from any :to pulled}
               :edit          {:from usable :to :edited}
               :preview       {:from (conj usable :conflicted) :to (fn [from _] from)}
               :push          {:from usable :to :staged}
               :move          {:from usable :to (fn [from _] (if (= :staged from) :staged :clean))}
               :push-conflict {:from (conj usable :conflicted) :to :conflicted}
               :resolve       {:from #{:conflicted} :to :staged
                               :guard (fn [_ {:keys [resolve]}]
                                        (when-not resolve
                                          "an element conflicts — decide: push --mine or --theirs, or pull --force"))}
               :commit        {:from #{:staged} :to :clean}
               :discard       {:from #{:staged} :to :orphaned}}})))

(defn state
  [sidecar edited?]
  (let [stored (some-> (:state sidecar) keyword)]
    (cond (nil? sidecar)                       :absent
          (#{:conflicted :orphaned} stored)    stored
          edited?                              :edited
          :else                                (or stored :clean))))

(defn- canon
  [x]
  (cond
    (map? x)        (into [::map]
                          (sort-by #(pr-str (first %)) (map (fn [[k v]] [(canon k) (canon v)]) x)))
    (set? x)        (into [::set] (sort-by pr-str (map canon x)))
    (sequential? x) (mapv canon x)
    :else           x))

(def ^:private ^String hex-digits "0123456789abcdef")

(defn fingerprint
  [x]
  (let [^bytes bs (.digest (MessageDigest/getInstance "SHA-256")
                           (.getBytes ^String (pr-str (canon x)) "UTF-8"))
        sb (StringBuilder. (* 2 (alength bs)))]
    (doseq [b bs :let [v (bit-and (long b) 0xff)]]
      (.append sb (.charAt hex-digits (bit-shift-right v 4)))
      (.append sb (.charAt hex-digits (bit-and v 0xf))))
    (.toString sb)))

(def whole
  tree/whole)

(def ^:private recent-bases (atom []))

(defn base-of
  ([body] (base-of body nil))
  ([body elements]
   (or (some (fn [[[b e] v]] (when (and (identical? b body) (= e elements)) v)) @recent-bases)
       (let [v (tree/base body (tree/declared elements) fingerprint tshape/fold)]
         (swap! recent-bases #(vec (take 4 (cons [[body elements] v] %))))
         v))))

(defn merge3
  ([base mine-body theirs-body] (merge3 base mine-body theirs-body nil))
  ([base mine-body theirs-body {:keys [resolve elements]}]
   (tree/merge3-over base mine-body theirs-body
                     {:cut     (tree/declared elements) :token fingerprint :fold tshape/fold
                      :resolve resolve})))

(defn memory-merge
  [a b]
  (into {} (for [p (distinct (concat (keys a) (keys b)))
                 :let [m (merge (get a p) (get b p))]
                 :when (seq m)]
             [p m])))

(defn moved
  [before after]
  (sort-by pr-str
           (for [[node to] (:placement after)
                 :let [from (get-in before [:placement node])]
                 :when (and from (not= from to))]
             [node from to])))

(defn by-path
  [memory]
  (reduce (fn [idx [part el path]] (update idx path (fnil conj #{}) [part el]))
          {}
          (for [part [:placement :fields :attributes]
                [el path] (get memory part)]
            [part el path])))

(defn source
  [files of-node in-text]
  (cond-> {::of-node of-node ::in-text in-text}
    (seq files) (assoc :files files)))

(defn texts
  [memory]
  (or (::in-text memory)
      (let [idx (delay (by-path memory))] #(get @idx %))))

(defn in-text
  [memory path]
  ((texts memory) path))

(defn memory-for
  [memory nodes]
  (if-let [f (::of-node memory)]
    (reduce (fn [m n] (reduce (fn [m [[part el] path]] (assoc-in m [part el] path)) m (f n)))
            (select-keys memory [:files ::of-node ::in-text])
            nodes)
    memory))

(defn source?
  [memory]
  (contains? memory ::of-node))
