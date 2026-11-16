(ns sutra.cli.slice
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.out :as out]
            [sutra.cli.projection :as projection]
            [sutra.wire.api :as api])
  (:import (java.io File)))

(defn- ->name [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- sidecar ^File [dir] (io/file dir ".slice.sutra"))

(defn- read-sidecar [dir]
  (let [f (sidecar dir)]
    (when (.exists f) (edn/read-string {:default (fn [_ v] v)} (slurp f)))))

(defn- body! [r]
  (if (<= 200 (:status r) 299)
    (:body r)
    (throw (ex-info (str (or (get-in r [:body :error]) (str "HTTP " (:status r))))
                    {:status (:status r) :body (:body r)}))))

(defn- written!
  [dir rel text]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f text)))

(defn- text-rel [nm path] (str (->name nm) "/" (->name path)))

(defn- code-rel [repo path] (str "repos/" (->name repo) "/" (->name path)))

(defn- recorded
  [branch nodes {:keys [projections code around]}]
  {:branch branch :nodes nodes :around around
   :projections (into {} (for [[nm {:keys [base shape files]}] projections]
                           [(->name nm)
                            {:base   (update-keys base ->name)
                             :shape  shape
                             :hashes (into {} (for [[p t] files]
                                                [(->name p) (projection/sha t)]))}]))
   :code (into {} (for [[repo fs] code]
                    [(->name repo)
                     (into {} (for [[p {:keys [text blob]}] fs]
                                [(->name p) {:blob blob :hash (projection/sha text)}]))]))})

(defn- rel-of [^File dir ^File f]
  (str/replace (str (.relativize (.toPath dir) (.toPath f))) "\\" "/"))

(defn- disk-texts
  [^File dir]
  (into {} (for [^File f (file-seq dir)
                 :when (and (.isFile f) (not= ".slice.sutra" (.getName f)))]
             [(rel-of dir f) (slurp f)])))

(defn- edited
  [c on-disk]
  (let [recorded (merge
                  (into {} (for [[nm {:keys [hashes]}] (:projections c) [p h] hashes]
                             [(text-rel nm p) h]))
                  (into {} (for [[repo fs] (:code c) [p {:keys [hash]}] fs]
                             [(code-rel repo p) hash])))]
    (vec (sort (for [rel (distinct (concat (keys recorded) (keys on-disk)))
                     :let [text (get on-disk rel)]
                     :when (or (nil? text) (not= (get recorded rel) (projection/sha text)))]
                 rel)))))

(defn- answer-files
  [{:keys [projections code]}]
  (into {}
        (concat
         (for [[nm {:keys [files]}] projections [p t] files] [(text-rel nm p) t])
         (for [[repo fs] code [p {:keys [text]}] fs] [(code-rel repo p) text]))))

(defn- stale-files
  [prior files]
  (let [wrote (concat
               (for [[nm {:keys [hashes]}] (:projections prior) [p _] hashes] (text-rel nm p))
               (for [[repo fs] (:code prior) [p _] fs] (code-rel repo p)))]
    (remove (set (keys files)) wrote)))

(defn- pull-lines
  [dir branch {:keys [nodes projections code not-held inputs feeds]}]
  (concat
   [(str "  ✓ slice → " dir "  (" (count nodes) " node(s), "
         (count (mapcat (comp :files val) projections)) " text(s), "
         (count (mapcat val code)) " code file(s)"
         (when branch (str ", branch " branch)) ")")]
   (when (seq not-held) [(str "    no text shows: " (str/join ", " not-held))])
   (for [[word ids] [["reads" inputs] ["feeds" feeds]] :when (seq ids)]
     (str "    " word " " (count ids) ": " (str/join ", " (take 12 ids))
          (when (< 12 (count ids)) " …")))))

(defn pull!
  [opts nodes {:keys [to force?]}]
  (let [dir   (.getCanonicalFile (io/file (or to ".")))
        prior (read-sidecar dir)
        stop  (when (and prior (not force?)) (seq (edited prior (disk-texts dir))))]
    (if stop
      (out/fail! (str "  ✗ would overwrite " (str/join ", " stop)
                      " — changed since the pull; push it first, or pull --force"))
      (projection/in-branch
       opts
       (fn []
         (let [r (body! (api/get! :slices {:nodes (str/join "," nodes)}))]
           (let [files (answer-files r)]
             (doseq [rel (stale-files prior files)] (io/delete-file (io/file dir rel) true))
             (doseq [[rel text] files] (written! dir rel text)))
           (spit (sidecar dir) (pr-str (recorded (:branch opts) nodes r)))
           (run! println (pull-lines dir (:branch opts) r))
           0))))))

(defn- text-part
  [nm {:keys [base shape]} changed on-disk]
  (let [prefix (str nm "/")
        files  (into {} (for [^String rel changed
                              :when (str/starts-with? rel prefix)]
                          [(subs rel (count prefix)) (get on-disk rel)]))]
    (when (seq files) {:files files :base base :shape shape})))

(defn- code-part
  [c changed on-disk]
  (reduce (fn [acc ^String rel]
            (if-let [repo (second (re-find #"^repos/([^/]+)/" rel))]
              (let [path (subs rel (+ 7 (count repo)))]
                (assoc-in acc [repo path] {:text (get on-disk rel)
                                           :blob (get-in c [:code repo path :blob])}))
              acc))
          {} changed))

(defn request
  [c on-disk {:keys [preview? resolve message]}]
  (let [changed (edited c on-disk)]
    (cond-> {:projections (into {}
                                (keep (fn [[nm p]]
                                        (when-let [x (text-part nm p changed on-disk)] [nm x])))
                                (:projections c))
             :code     (code-part c changed on-disk)
             :around   (:around c)
             :preview? (boolean preview?)}
      resolve (assoc :resolve resolve)
      message (assoc :message message))))

(defn unheld-files
  [c on-disk]
  (let [held (map #(str (->name %) "/") (keys (:projections c)))]
    (vec (sort (for [^String rel (keys on-disk)
                     :when (not (or (re-find #"^repos/[^/]+/" rel)
                                    (some #(str/starts-with? rel %) held)))]
                 rel)))))

(defn- push-lines
  [{:keys [code code-refused proposal commits inputs-changed]} preview?]
  (concat
   (projection/reach-lines {:inputs-changed inputs-changed})
   (when (seq code)
     [(str "  code" (when preview? " (would commit)") ": "
           (str/join ", " (map #(str (->name (:repo %)) "/" (->name (:path %))) code)))])
   (map (fn [{:keys [repo why]}] (str "  ✗ code of " (->name repo) " refused: " why)) code-refused)
   (when (:id proposal) [(str "  ✓ proposal opened: " (:id proposal))])
   (mapcat (fn [{:keys [repo commit lines]}]
             (cons (str "  ✓ " (->name repo) " " (out/short-id commit))
                   (map #(str "    " %) lines)))
           commits)))

(defn push!
  [opts dir {:keys [preview?] :as flags}]
  (let [root (.getCanonicalFile (io/file dir))
        c    (read-sidecar root)
        disk (when c (disk-texts root))]
    (cond
      (nil? c)
      (out/fail! (str "  ✗ no slice in " root " — pull one first"))

      (and (:branch c) (:branch opts) (not= (:branch c) (:branch opts)))
      (out/fail! (str "  ✗ this slice was pulled on " (:branch c) ", not " (:branch opts)))

      (seq (unheld-files c disk))
      (apply out/fail!
             (concat ["  ✗ this slice's pull wrote no such file, and a push would not send it:"]
                     (map #(str "    " %) (unheld-files c disk))
                     [(str "    A text is edited through a pull that holds it: `slice pull <node>`"
                           " of a node in it, or for code alone push that first — a decorated"
                           " file declares its transform, then the text exists to pull.")]))

      (empty? (edited c disk))
      (do (println "  ◦ nothing changed") 0)

      :else
      (let [opts (assoc opts :branch (or (:branch opts) (:branch c)))]
        (projection/in-branch
         opts
         (fn []
           (let [r (body! (api/post! :slices nil (request c disk flags)))]
             (doseq [[nm answer] (:projections r)]
               (println (str "  " (->name nm) ":"))
               (projection/report! (dissoc opts :json?) (:branch opts) answer))
             (run! println (push-lines r preview?))
             (if preview?
               (if (or (seq (:code-refused r)) (some :refused? (vals (:projections r)))) 1 0)
               (pull! opts (:nodes c) {:to (str root) :force? true})))))))))
