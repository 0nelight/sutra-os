(ns sutra.capps.rtf
  (:require [clojure.string :as str]))

(def ^:private skip-gruppen
  #{"fonttbl" "colortbl" "stylesheet" "info" "generator" "pict" "listtable"
    "rsidtbl" "themedata" "colorschememapping" "latentstyles" "datastore"})

(defn rtf?
  [s]
  (boolean (and (string? s) (str/includes? (subs s 0 (min 40 (count s))) "\\rtf"))))

(defn- code
  [c]
  #?(:clj (int ^char c) :cljs (.charCodeAt (str c) 0)))

(defn- zeichen [n] #?(:clj (str (char n)) :cljs (js/String.fromCharCode n)))

(defn- buchstabe? [c] (let [n (code c)] (or (<= 65 n 90) (<= 97 n 122))))
(defn- ziffer?    [c] (let [n (code c)] (<= 48 n 57)))

(defn- hex->zahl [h] #?(:clj (Long/parseLong h 16) :cljs (js/parseInt h 16)))
(defn- zahl->hex [n] #?(:clj (Integer/toHexString n) :cljs (.toString n 16)))

(defn- steuerwort
  [s i]
  (let [n (count s)]
    (loop [j (inc i) nm ""]
      (if (and (< j n) (buchstabe? (.charAt s j)))
        (recur (inc j) (str nm (.charAt s j)))
        (when (seq nm)
          (let [[k arg] (loop [k j neg false num ""]
                          (cond
                            (and (< k n) (= \- (.charAt s k)) (empty? num))
                            (recur (inc k) true num)
                            (and (< k n) (ziffer? (.charAt s k)))
                            (recur (inc k) neg (str num (.charAt s k)))
                            :else
                            [k (when (seq num)
                                 (cond-> (parse-long num) neg -))]))
                k (if (and (< k n) (= \space (.charAt s k))) (inc k) k)]
            [(- k i) nm arg]))))))

(def ^:private umbruch #{"par" "line"})

(defn segments
  [s]
  (if-not (rtf? s)
    [{:kind :text :s (or s "") :v (or s "")}]
    (let [n (count s)]
      (loop [i 0, tiefe 0, skip-ab nil, aus [], mk "", tx "", tv ""]
        (let [flush-mk #(if (seq mk) (conj % {:kind :markup :s mk}) %)
              flush-tx #(if (seq tx) (conj % {:kind :text :s tx :v tv}) %)]
          (if (>= i n)
            (-> aus flush-tx flush-mk)
            (let [c (.charAt ^String s i)
                  mark (fn [i' tiefe' skip' roh brk?]
                         (if brk?
                           [i' tiefe' skip'
                            (conj (-> aus flush-tx flush-mk) {:kind :markup :s roh :break? true})
                            "" "" ""]
                           [i' tiefe' skip' (flush-tx aus) (str mk roh) "" ""]))
                  txt  (fn [i' roh v]
                         [i' tiefe skip-ab (flush-mk aus) "" (str tx roh) (str tv v)])
                  [i' tiefe' skip' aus' mk' tx' tv']
                  (cond
                    (= c \{)
                    (let [neue-tiefe (inc tiefe)
                          [_ nm]     (steuerwort s (inc i))
                          stern?     (and (< (inc i) n) (= \* (.charAt ^String s (inc i))))
                          skip''     (if (and (nil? skip-ab)
                                              (or stern? (contains? skip-gruppen (str nm))))
                                       neue-tiefe skip-ab)]
                      (mark (inc i) neue-tiefe skip'' "{" false))

                    (= c \})
                    (mark (inc i) (dec tiefe) (when-not (= skip-ab tiefe) skip-ab) "}" false)

                    skip-ab
                    (mark (inc i) tiefe skip-ab (subs s i (inc i)) false)

                    (= c \\)
                    (let [nx (when (< (inc i) n) (.charAt ^String s (inc i)))]
                      (cond
                        (and (= nx \') (< (+ i 3) n))
                        (let [hex (subs s (+ i 2) (+ i 4))
                              ch  (try (zeichen (hex->zahl hex))
                                       (catch #?(:clj Exception :cljs :default) _ "?"))]
                          (txt (+ i 4) (subs s i (+ i 4)) ch))

                        (contains? #{\\ \{ \}} nx)
                        (txt (+ i 2) (subs s i (+ i 2)) (str nx))

                        :else
                        (if-let [[len nm arg] (steuerwort s i)]
                          (if (and (= "u" nm) arg)
                            (let [ersatz (if (< (+ i len) n) 1 0)]
                              (txt (+ i len ersatz) (subs s i (+ i len ersatz))
                                   (zeichen arg)))
                            (mark (+ i len) tiefe skip-ab (subs s i (+ i len))
                                  (boolean (umbruch nm))))
                          (mark (inc i) tiefe skip-ab (subs s i (inc i)) false))))

                    :else
                    (txt (inc i) (subs s i (inc i))
                         (if (contains? #{\newline \return} c) "" (str c))))]
              (recur i' tiefe' skip' aus' mk' tx' tv'))))))))

(declare absaetze)

(defn edit-text
  [s]
  (if-not (rtf? s)
    (or s "")
    (str/join "\n" (map :v (butlast (absaetze (segments s)))))))

(defn text
  [s]
  (-> (edit-text s)
      (str/replace #"[ \t]+\n" "\n")
      str/trim))

(defn- kodiere
  [t]
  (apply str
         (for [c (or t "")]
           (let [n (code c)]
             (cond
               (contains? #{\\ \{ \}} c) (str "\\" c)
               (< 31 n 127)              (str c)
               (< n 256)                 (str "\\'" (let [h (zahl->hex n)]
                                                      (if (= 1 (count h)) (str "0" h) h)))
               :else                     (str "\\u" n "?"))))))

(defn- absaetze
  [segs]
  (loop [[s & r] segs, cur [], aus []]
    (cond
      (nil? s) (conj aus {:segs cur :v (apply str (keep :v cur)) :break nil})
      (:break? s) (recur r [] (conj aus {:segs cur :v (apply str (keep :v cur)) :break (:s s)}))
      :else (recur r (conj cur s) aus))))

(defn- absatz-roh [{:keys [segs break]}] (str (apply str (map :s segs)) break))

(defn- absatz-nur-marken
  [{:keys [segs]}]
  (apply str (keep #(when (= :markup (:kind %)) (:s %)) segs)))

(defn- absatz-mit-text
  [{:keys [segs break]} t]
  (let [traegt? (fn [s] (and (= :text (:kind s)) (not (str/blank? (:s s)))))
        erster (first (keep-indexed (fn [i s] (when (traegt? s) i)) segs))
        segs   (if erster
                 (map-indexed (fn [i s]
                                (cond (= i erster) (assoc s :s (kodiere t))
                                      (traegt? s)  (assoc s :s "")
                                      :else s))
                              segs)
                 (concat segs [{:kind :text :s (kodiere t)}]))]
    (str (apply str (map :s segs)) break)))

(defn- verbinde
  [links rechts]
  (if (and (seq links) (seq rechts)
           (re-find #"\\[a-zA-Z]+$" links)
           (re-find #"^[a-zA-Z0-9]" rechts))
    (str links " " rechts)
    (str links rechts)))

(defn patch
  [original neuer-text]
  (if-not (rtf? original)
    (or neuer-text "")
    (let [teile   (absaetze (segments original))
          korpus  (vec (butlast teile))
          nach    (last teile)
          alt     (mapv :v korpus)
          neu     (str/split (or neuer-text "") #"\n" -1)
          muster  (or (last korpus) {:segs [] :break "\\par "})]
      (reduce verbinde ""
              (concat
               (map-indexed
                (fn [i t]
                  (cond
                    (>= i (count korpus)) (absatz-mit-text muster t)
                    (= t (nth alt i))     (absatz-roh (nth korpus i))
                    :else                 (absatz-mit-text (nth korpus i) t)))
                neu)
               (map absatz-nur-marken (drop (count neu) korpus))
               [(absatz-roh nach)])))))
