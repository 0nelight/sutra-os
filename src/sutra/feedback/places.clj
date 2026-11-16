(ns sutra.feedback.places
  (:require [sutra.http.endpoint :as ep]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]
            [sutra.foundation.env :as env]
            [sutra.http.provenance :as provenance]
            [sutra.store.control :as ctl]
            [sutra.types.feedback :as b]))

(set! *warn-on-reflection* true)

(defn session-in-header
  [req]
  (some-> (get-in req [:headers "x-sutra-feedback"]) str str/trim not-empty))

(defn own-session?
  [store u id]
  (boolean (and store u id (= u (:benutzer (ctl/get store :begleitungen {:id id}))))))

(defn admitted?
  [req]
  (boolean (when-let [id (session-in-header req)]
             (try (own-session? (:sutra/store req) (ep/signed-in-user req) id)
                  (catch clojure.lang.ExceptionInfo e
                    (println "[feedback] session" id "not read:" (ex-message e))
                    false)))))

(def ^:private base64
  (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" (range)))

(defn- vlq [^String s]
  (loop [i 0, shift 0, acc 0, out []]
    (if (= i (count s))
      out
      (let [d   (long (base64 (.charAt s i)))
            acc (bit-or acc (bit-shift-left (bit-and d 31) shift))]
        (if (bit-test d 5)
          (recur (inc i) (+ shift 5) acc out)
          (let [v (bit-shift-right acc 1)]
            (recur (inc i) 0 0 (conj out (if (bit-test acc 0) (- v) v)))))))))

(defn- line-segments
  [text src line]
  (reduce (fn [[segs col src line] seg]
            (let [[dc ds dl] (vlq seg)
                  col (+ col dc)]
              (if ds
                (let [src (+ src ds), line (+ line dl)]
                  [(conj segs [col src line]) col src line])
                [segs col src line])))
          [[] 0 src line]
          (remove str/blank? (str/split text #","))))

(defn- search-section
  [mappings sought]
  (let [per-line (group-by first sought)
        last-ln  (reduce max -1 (keys per-line))]
    (loop [lines (str/split (str mappings) #";" -1), i 0, src 0, line 0, acc {}]
      (if (or (empty? lines) (> i last-ln))
        acc
        (let [[segs _ src line] (line-segments (first lines) src line)
              acc (reduce (fn [a [_ column :as p]]
                            (if-let [[_ q l] (last (take-while #(<= (first %) column) segs))]
                              (assoc a p [q l])
                              a))
                          acc
                          (per-line i))]
          (recur (rest lines) (inc i) (long src) (long line) acc))))))

(defn- section-at
  [sections pos]
  (last (take-while (fn [{{:keys [line column]} :offset}]
                      (not (pos? (compare [line column] pos))))
                    sections)))

(defn original
  [{:keys [sections mappings sources]} sought]
  (if sections
    (into {}
          (for [[s ps] (group-by #(section-at sections %) sought)
                :when s
                :let [{:keys [line column]} (:offset s)
                      inner (fn [[l c]] [(- l line) (if (= l line) (- c column) c)])
                      hits  (original (:map s) (map inner ps))]
                p ps
                :let [t (hits (inner p))]
                :when t]
            [p t]))
    (update-vals (search-section mappings sought)
                 (fn [[q l]] {:quelle (get sources q) :zeile l}))))

(defn- v8->map-pos
  [^String text v8-line column]
  (let [m      (re-matcher #"\r\n|\n|\r| | " text)
        starts (loop [v [0]] (if (.find m) (recur (conj v (.end m))) v))
        offset (+ (get starts v8-line 0) column)
        before (subs text 0 (min offset (count text)))]
    [(count (filter #{\newline} before))
     (- (count before) (inc (.lastIndexOf before "\n")))]))

(def ^:private script-name #"^[A-Za-z0-9_.-]+\.js$")

(defn- public-url
  [path]
  (or (when-let [d (env/getenv "SUTRA_PUBLIC_DIR")]
        (let [f (io/file d path)] (when (.exists f) (io/as-url f))))
      (io/resource (str "public/" path))))

(defn- bundle-path
  [script]
  (when (re-matches script-name (str script))
    (some #(when (public-url %) %) [(str "js/" script) (str "js/cljs-runtime/" script)])))

(defn- source->repo
  [q]
  (let [q (str/replace (str q) #"^(\.\./|\./)+" "")]
    (cond
      (str/starts-with? q "sutra_ui/") (str "clients/frontend/src/" q)
      (re-find #"^(sutra|platform_sdk)/" q) (provenance/repo-path q)
      :else nil)))

(def ^:private own-frames
  #"feedback_aufzeichnung\.cljs$|sutra/wire/transport\.cljc$")

(defn- frame-hits
  [script frames]
  (when-let [path (bundle-path script)]
    (when-let [source-map (public-url (str path ".map"))]
      (let [text (slurp (public-url path))
            pos  (into {}
                       (map (fn [{:keys [zeile spalte] :as r}]
                              [r (v8->map-pos text (dec zeile) (dec spalte))]))
                       frames)
            hits (original (json/read-str (slurp source-map) :key-fn keyword)
                           (distinct (vals pos)))]
        (update-vals pos hits)))))

(defn browser-frames
  [trace]
  (let [found (into {} (mapcat (fn [[s rs]] (frame-hits s rs))) (group-by :skript trace))]
    (->> trace
         (keep (fn [r]
                 (when-let [{:keys [quelle zeile]} (found r)]
                   (when-let [d (source->repo quelle)]
                     (when-not (re-find own-frames d)
                       {:datei d :zeile (inc zeile)})))))
         dedupe
         (take provenance/frames-max)
         vec)))

(def ^:private index
  (delay (or (some-> (public-url "js/quellen.edn") slurp edn/read-string) {})))

(def ^:private alone-max
  5)

(defn- frames-of-classes
  [idx ks]
  (let [known (keep #(some-> (get idx %) set) ks)]
    (when (seq known)
      (let [all    (apply clojure.set/intersection known)
            rarest (apply min-key count known)]
        (cond (seq all)                      (sort all)
              (<= (count rarest) alone-max) (sort rarest))))))

(defn renderer
  ([m] (renderer @index m))
  ([idx {:keys [selektor klassen]}]
   (some->> (or (when (seq klassen) (frames-of-classes idx klassen))
                (->> (b/klassen selektor)
                     (keep #(let [ss (get idx %)] (when (<= (count ss) alone-max) ss)))
                     first))
            (take alone-max)
            (mapv (fn [s] (let [[_ d z] (re-matches #"(.+):(\d+)" s)]
                            {:datei d :zeile (parse-long z)}))))))

(defn- revision [] (not-empty (env/getenv "SUTRA_REVISION")))

(defn- with-renderer [m]
  (let [r (when (or (:selektor m) (:klassen m)) (renderer m))]
    (cond-> m (seq r) (assoc :renderer r))))

(defn- resolve-stack [{:keys [spur] :as a}]
  (let [b (when (seq spur)
            (try (browser-frames spur)
                 (catch Exception e
                   (println "[feedback] browser stack not resolved:" (ex-message e))
                   nil)))]
    (cond-> a
      (seq b) (-> (assoc :browser b) (dissoc :spur)))))

(defn resolve-event
  [e]
  (let [e (cond-> (with-renderer e)
            (:absage e) (update :absage resolve-stack)
            (:spur e) resolve-stack
            (seq (:referenzen e)) (update :referenzen (partial mapv with-renderer)))
        placed? (or (:renderer e) (some :renderer (:referenzen e)) (:browser e)
                    (get-in e [:absage :browser]) (get-in e [:absage :server]))]
    (cond-> e
      (and placed? (revision)) (assoc :revision (revision)))))
