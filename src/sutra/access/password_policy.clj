(ns sutra.access.password-policy
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(def defaults {:min-length 12 :max-length 1024})

(defn settings
  []
  (let [m (some-> (env/getenv "SUTRA_PASSWORD_MIN_LENGTH") str/trim parse-long)]
    (cond-> defaults m (assoc :min-length (max 8 m)))))

(def common
  (delay
    (with-open [r (io/reader (io/resource "sutra/auth/common-passwords.txt"))]
      (into #{} (comp (map str/trim) (remove #(or (str/blank? %) (str/starts-with? % "#")))
                      (map str/lower-case))
            (line-seq r)))))

(defn- core-word
  [s]
  (-> s (str/replace #"^[\d\W_]+" "") (str/replace #"[\d\W_]+$" "")))

(defn- run?
  [s]
  (let [ds (map (fn [a b] ({-9 1, 9 -1} (- (int b) (int a)) (- (int b) (int a)))) s (rest s))]
    (boolean (and (seq ds) (or (every? #{1} ds) (every? #{-1} ds))))))

(defn- repeated? [s] (boolean (re-matches #"(?s)(.{1,4})\1+" s)))

(defn common?
  ([password] (common? @common password))
  ([words password]
   (let [s (str/lower-case (str password))
         w (core-word s)]
     (boolean (or (contains? words s)
                  (contains? words w)
                  (repeated? s)
                  (run? s))))))

(defn context-words
  ([] (context-words (env/getenv "SUTRA_DOMAIN") (env/getenv "SUTRA_PUBLIC_URL")
                     (env/getenv "SUTRA_PASSWORD_CONTEXT_WORDS")))
  ([domain public-url extra]
   (let [host (or (some->> public-url str (re-find #"^[a-z]+://([^/:]+)") second) domain)]
     (into #{"sutra"}
           (comp (map (comp str/lower-case str/trim)) (filter #(>= (count %) 4)))
           (concat (some-> host str (str/split #"\."))
                   (some-> extra str (str/split #",")))))))

(defn- context-built?
  [lc words]
  (boolean (some #(and (str/includes? lc %)
                       (str/blank? (core-word (str/replace lc % ""))))
                 words)))

(defn problems
  ([m] (problems (cond-> (settings) (env/auth-hardening?) (assoc :context (context-words)))
                 @common m))
  ([{:keys [min-length max-length context]} words {:keys [username password]}]
   (let [pw (str password)
         n  (.codePointCount pw 0 (count pw))
         lc (str/lower-case pw)
         u  (some-> username str str/trim str/lower-case not-empty)]
     (cond-> []
       (< n min-length)                         (conj :too-short)
       (> n max-length)                         (conj :too-long)
       (and u (or (= lc u) (= (core-word lc) u))) (conj :is-username)
       (common? words pw)                       (conj :common)
       (context-built? lc context)              (conj :context)))))

(def ^:private said
  {:too-short   "is shorter than {min} characters"
   :too-long    "is longer than {max} characters"
   :is-username "is the username"
   :common      "is a common password, a common word with digits around it, or a pattern"
   :context     "is built from the name of this service or its host"})

(defn explain
  ([ps] (explain (settings) ps))
  ([{:keys [min-length max-length]} ps]
   (str "the password "
        (str/join ", " (for [p ps]
                         (-> (said p)
                             (str/replace "{min}" (str min-length))
                             (str/replace "{max}" (str max-length)))))
        " — a long passphrase passes, no character classes are required")))
