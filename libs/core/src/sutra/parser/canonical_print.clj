(ns sutra.parser.canonical-print
  (:require [clojure.string :as str])
  (:import (java.util ArrayList)))

(set! *warn-on-reflection* true)

(def ^:const right-margin 80)

(def ^:const t-text 0)
(def ^:const t-ws 1)
(def ^:const t-nl 2)
(def ^:const t-start 3)
(def ^:const t-end 4)

(defn- flat-str ^String [v]
  (binding [*print-length* nil
            *print-level* nil
            *print-namespace-maps* false
            *print-readably* true]
    (pr-str v)))

(defn- push! [^ArrayList tags ^ArrayList strs ^ArrayList lbs tag ^String s lb]
  (.add tags (int tag))
  (.add strs s)
  (.add lbs (int lb)))

(defn- block-open!
  [{:keys [^ArrayList tags ^ArrayList strs ^ArrayList lbs
           ^ArrayList parent ^ArrayList depth stack]}
   ^String prefix]
  (let [b   (.size parent)
        top (peek @stack)]
    (.add parent (int (or top -1)))
    (.add depth (int (if top (inc (int (.get ^ArrayList depth (int top)))) 0)))
    (swap! stack conj b)
    (push! tags strs lbs t-start prefix b)
    b))

(defn- block-close!
  [{:keys [^ArrayList tags ^ArrayList strs ^ArrayList lbs stack]} ^String suffix]
  (let [b (peek @stack)]
    (swap! stack pop)
    (push! tags strs lbs t-end suffix b)))

(defn- text! [{:keys [^ArrayList tags ^ArrayList strs ^ArrayList lbs]} ^String s]
  (push! tags strs lbs t-text s -1))

(defn- ws! [{:keys [^ArrayList tags ^ArrayList strs ^ArrayList lbs]} ^String s]
  (push! tags strs lbs t-ws s -1))

(defn- nl! [{:keys [^ArrayList tags ^ArrayList strs ^ArrayList lbs stack]}]
  (push! tags strs lbs t-nl "" (peek @stack)))

(declare walk!)

(defn- walk-seq! [printer items ^String prefix ^String suffix]
  (block-open! printer prefix)
  (loop [xs (seq items)]
    (when xs
      (walk! printer (first xs))
      (when (next xs)
        (ws! printer " ")
        (nl! printer)
        (recur (next xs)))))
  (block-close! printer suffix))

(defn- walk-map! [printer m]
  (block-open! printer "{")
  (loop [es (seq m)]
    (when es
      (let [[k v] (first es)]
        (block-open! printer nil)
        (walk! printer k)
        (ws! printer " ")
        (nl! printer)
        (walk! printer v)
        (block-close! printer nil))
      (when (next es)
        (text! printer ",")
        (ws! printer " ")
        (nl! printer)
        (recur (next es)))))
  (block-close! printer "}"))

(defn- walk! [printer v]
  (cond
    (map? v)    (walk-map! printer v)
    (vector? v) (walk-seq! printer v "[" "]")
    (set? v)    (walk-seq! printer v "#{" "}")
    (seq? v)    (walk-seq! printer v "(" ")")
    :else       (text! printer (flat-str v))))

(defn- token-widths ^ints [^ArrayList tags ^ArrayList strs]
  (let [n (.size tags)
        w (int-array n)]
    (dotimes [i n]
      (let [tag (int (.get tags i))]
        (aset w i (int (if (= tag t-nl) 0 (count ^String (or (.get strs i) "")))))))
    w))

(defn- section-ends
  ^ints [^ArrayList tags ^ArrayList lbs ^ArrayList depth n-blocks]
  (let [n     (.size tags)
        ends  (int-array n-blocks n)
        maxd  (inc (reduce max 0 (map int depth)))
        near  (int-array (max 1 maxd) n)]
    (loop [i (dec n)]
      (when (>= i 0)
        (let [tag (int (.get tags i))]
          (when (= tag t-nl)
            (let [b  (int (.get lbs i))
                  db (int (.get ^ArrayList depth b))
                  e  (loop [d 0 best n]
                       (if (< d db)
                         (recur (inc d) (min best (aget near d)))
                         best))]
              (aset ends b (int e))
              (aset near db i))))
        (recur (dec i))))
    ends))

(defn- render
  ^String [{:keys [^ArrayList tags ^ArrayList strs ^ArrayList lbs
                   ^ArrayList parent ^ArrayList depth]}]
  (let [n        (.size tags)
        nb       (.size parent)
        w        (token-widths tags strs)
        prefix   (long-array (inc n))
        _        (dotimes [i n]
                   (aset prefix (inc i) (+ (aget prefix i) (long (aget w i)))))
        ends     (section-ends tags lbs depth nb)
        start-col (int-array nb)
        done-nl  (boolean-array nb)
        sb       (StringBuilder.)
        col      (int-array 1)
        pending  (object-array 1)
        write!   (fn [^String s]
                   (when (seq s)
                     (.append sb s)
                     (aset col 0 (int (+ (aget col 0) (count s))))))
        flush-ws! (fn []
                    (when-let [^String p (aget pending 0)]
                      (write! p)
                      (aset pending 0 nil)))]
    (dotimes [i n]
      (let [tag (int (.get tags i))
            ^String s (.get strs i)]
        (cond
          (= tag t-text) (do (flush-ws!) (write! s))

          (= tag t-ws) (aset pending 0 s)

          (= tag t-start)
          (do (flush-ws!)
              (write! s)
              (let [b (int (.get lbs i))]
                (aset start-col b (aget col 0))
                (aset done-nl b false)))

          (= tag t-end) (do (flush-ws!) (write! s))

          :else
          (let [b    (int (.get lbs i))
                e    (aget ends b)
                span (- (aget prefix e) (aget prefix (inc i)))
                brk? (or (aget done-nl b)
                         (not (< (+ (aget col 0) span) right-margin)))]
            (if brk?
              (do (aset pending 0 nil)
                  (.append sb \newline)
                  (let [ind (aget start-col b)]
                    (dotimes [_ ind] (.append sb \space))
                    (aset col 0 ind))
                  (aset done-nl b true)
                  (loop [p (int (.get parent b))]
                    (when (>= p 0)
                      (aset done-nl p true)
                      (recur (int (.get ^ArrayList parent p))))))
              (flush-ws!))))))
    (.toString sb)))

(defn canonical-str
  ^String [v]
  (let [printer {:tags (ArrayList.) :strs (ArrayList.) :lbs (ArrayList.)
            :parent (ArrayList.) :depth (ArrayList.) :stack (atom [])}]
    (walk! printer v)
    (str (render printer) "\n")))

(def ^:private key-rank
  (zipmap [:widget :kind :do :op :when] (range)))

(defn- key-order
  [a b]
  (compare [(key-rank a 5) (pr-str a)] [(key-rank b 5) (pr-str b)]))

(defn- ->canonical-value
  [v]
  (cond
    (map? v)
    (let [entries (mapv (fn [[k x]] [k (->canonical-value x)]) v)
          sorted  (into (sorted-map-by key-order) entries)]
      (when (not= (count sorted) (count entries))
        (throw (ex-info "canonical writer: two keys share one printed form"
                        {:kind :sutra.parser.canonical-print/key-collision
                         :keys (->> (map first entries)
                                    (group-by pr-str)
                                    (filter (fn [[_ ks]] (< 1 (count ks))))
                                    (mapv (fn [[s ks]] {:printed s :keys (vec ks)})))})))
      sorted)

    (set? v)
    (into (sorted-set-by #(compare (pr-str %1) (pr-str %2)))
          (map ->canonical-value) v)

    (vector? v) (mapv ->canonical-value v)
    (seq? v)    (mapv ->canonical-value v)
    :else v))

(defn canonical-ent-str
  ^String [ent]
  (let [txt (canonical-str (->canonical-value ent))]
    (str (str/replace (str/trimr (str/replace txt "\r\n" "\n")) #"[ \t]+\n" "\n")
         "\n")))
