(ns sutra.foundation.compat
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.foundation.boundaries :as bnd])
  (:import (java.io PushbackReader StringReader)
           (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(def lock-file "boundaries/compat.lock.edn")

(defn- ref-name
  [ref]
  (peek (str/split (str ref) #"/")))

(defn- hashed?
  [n]
  (str/starts-with? (str n) "h-"))

(defn- schema-facts
  ([node path] (schema-facts node path {}))
  ([node path defs]
   (if-not (map? node)
     []
     (let [ref (get node "$ref")
           n   (some-> ref ref-name)]
       (cond
         (and n (hashed? n) (contains? defs n)) (schema-facts (get defs n) path defs)
         ref [[:ref path n]]
         :else
         (let [types (let [t (get node "type")] (if (sequential? t) t (some-> t vector)))]
           (concat
            (for [t types] [:type path t])
            (for [v (get node "enum")] [:enum path v])
            (when (contains? node "const") [[:enum path (get node "const")]])
            (for [r (get node "required")] [:required path r])
            (when (false? (get node "additionalProperties")) [[:closed path]])
            (mapcat (fn [[k v]] (cons [:prop path k] (schema-facts v (conj path k) defs)))
                    (get node "properties"))
            (when (map? (get node "items"))
              (schema-facts (get node "items") (conj path "[]") defs))
            (when (map? (get node "additionalProperties"))
              (schema-facts (get node "additionalProperties") (conj path "*") defs))
            (apply concat
                   (map-indexed #(schema-facts %2 (conj path (str "[" %1 "]")) defs)
                                (get node "prefixItems")))
            (apply concat
                   (for [k ["anyOf" "oneOf" "allOf"]
                         [i s] (map-indexed vector (get node k))]
                     (schema-facts s (conj path (str k i)) defs))))))))))

(defn- json-body [node]
  (some #(get-in node ["content" %]) ["application/json" "application/transit+json"]))

(defn openapi-facts
  [doc]
  (for [[p ops] (get doc "paths")
        [m op] ops
        :when (map? op)
        f (concat
           [[:op p m]]
           (mapcat (fn [{:strs [in name required schema]}]
                     (cond-> [[:param p m in name]]
                       required (conj [:required-param p m in name])
                       schema   (into (map #(into [:param-schema p m name] %)
                                           (schema-facts schema [])))))
                   (get op "parameters"))
           (when-let [b (json-body (get op "requestBody"))]
             (map #(into [:request p m] %) (schema-facts (get b "schema") [])))
           (mapcat (fn [[code r]]
                     (cons [:response p m code]
                           (when-let [b (json-body r)]
                             (map #(into [:response-body p m code] %)
                                  (schema-facts (get b "schema") [])))))
                   (get op "responses")))]
    f))

(defn component-facts
  [doc facts]
  (let [refs  (fn [fs] (keep #(when (= :ref (nth % (- (count %) 3) nil)) (peek %)) fs))
        local (fn [n] (when (contains? (get-in doc ["components" "schemas"]) n) n))]
    (loop [todo (keep local (refs facts)) seen #{} out []]
      (if-let [[n & more] (seq todo)]
        (if (seen n)
          (recur more seen out)
          (let [fs (schema-facts (get-in doc ["components" "schemas" n]) [])]
            (recur (concat more (keep local (refs fs))) (conj seen n)
                   (into out (map #(into [:component n] %)) fs))))
        out))))

(defn json-schema-facts
  [doc]
  (let [defs (merge (get doc "definitions") (get doc "$defs"))]
    (concat (schema-facts doc [] defs)
            (for [[n d] defs
                  :when (not (hashed? n))
                  f (schema-facts d ["#" n] defs)]
              f))))

(defn- read-protocol
  [^String s n]
  (let [m (re-matcher (re-pattern (str "\\(defprotocol\\s+" n "(?![\\w\\-])")) s)]
    (when (.find m)
      (binding [*read-eval* false]
        (read (PushbackReader. (StringReader. (subs s (.start m)))))))))

(defn protocol-facts
  [s n]
  (let [form (read-protocol s n)]
    (for [sig (drop 2 form)
          :when (seq? sig)
          a (filter vector? sig)]
      [:method n (str (first sig)) (count a)])))

(defn- json-artifacts [row]
  (filter #(str/ends-with? % ".json") (get-in row [:spec :generated])))

(defn surface
  ([reg id] (surface reg id #(let [f (io/file %)] (when (.exists f) (slurp f)))))
  ([reg id text-of]
   (let [row (get-in reg [:boundaries id])]
     (set
      (concat
       (for [f (json-artifacts row)
             :let [text (text-of f)]
             :when text
             :let [doc (json/read-str text)]
             fact (if (get doc "openapi")
                    (let [own (filter #(= id (bnd/owner-of-url reg (second %)))
                                      (openapi-facts doc))]
                      (concat own (component-facts doc own)))
                    (map #(into [:schema f] %) (json-schema-facts doc)))]
         fact)
       (for [[f n] (get-in row [:covers :protocols])
             :let [text (text-of f)]
             :when text
             fact (protocol-facts text n)]
         fact))))))

(defn digest
  [facts]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (.update md (.getBytes (pr-str (sort-by pr-str facts)) "UTF-8"))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(defn- fact-path
  [[kind & more]]
  (case kind
    (:required :enum :type :closed :ref) (vec (butlast more))
    (vec more)))

(defn- tightening? [[kind]] (#{:required :required-param :closed} kind))

(defn change
  [old new]
  (let [removed (sort-by pr-str (remove new old))
        added   (sort-by pr-str (remove old new))
        known   (set (map fact-path old))
        tight   (filter #(and (tightening? %) (known (fact-path %))) added)]
    {:kind      (cond (seq removed) :breaking
                      (seq tight)   :breaking
                      (seq added)   :minor
                      :else         :none)
     :removed   removed
     :added     added
     :tightened tight}))

(defn- semver [v] (mapv parse-long (str/split v #"\.")))

(defn admits?
  [a b k]
  (let [[ma mi pa] (semver a) [mb ib pb] (semver b)]
    (case k
      :breaking (> mb ma)
      :minor    (or (> mb ma) (and (= mb ma) (> ib mi)))
      :patch    (or (> mb ma) (and (= mb ma) (or (> ib mi) (and (= ib mi) (> pb pa)))))
      :none     true)))

(defn lock
  []
  (let [f (io/file lock-file)]
    (if (.exists f) (edn/read-string (slurp f)) {})))

(defn versioned
  [reg]
  (into (sorted-set)
        (keep (fn [[id row]]
                (when (or (seq (json-artifacts row)) (seq (get-in row [:covers :protocols])))
                  id)))
        (:boundaries reg)))
