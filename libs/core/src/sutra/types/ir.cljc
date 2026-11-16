(ns sutra.types.ir
  (:require [clojure.string :as str]
            [sutra.types.paths :as paths]))

(defn endpoint [id server-facts]
  (let [tmpl   (paths/paths id)
        params (paths/params-of tmpl)]
    (cond-> {:id id :path tmpl}
      (seq params)              (assoc :params params)
      (contains? paths/query id)    (assoc :query (vec (sort (keys (paths/query id)))))
      (contains? paths/encoding id) (assoc :encoding (paths/encoding id))
      (seq server-facts)        (merge server-facts))))

(defn ir
  ([] (ir nil))
  ([server-facts]
   {:version 1
    :endpoints (into (sorted-map)
                     (map (fn [id] [id (endpoint id (get server-facts id))]))
                     (keys paths/paths))}))

(defn- py-str [x] (str "\"" (-> (str x) (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) "\""))

(defn- kw-name [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- py-list [xs] (str "[" (str/join ", " (map py-str xs)) "]"))

(defn- py-entry
  [{:keys [path params query encoding methods auth service request response]}]
  (let [kv (fn [k v] (str (py-str k) ": " v))]
    (str "{"
         (str/join ", "
                   (cond-> [(kv "path" (py-str (str/replace path #":([^/]+)" "{$1}")))]
                     (seq params) (conj (kv "params" (py-list (map name params))))
                     (seq query)  (conj (kv "query" (py-list (map name query))))
                     encoding     (conj (kv "encoding" (py-str (name encoding))))
                     true         (conj (kv "methods" (py-list (map name methods)))
                                        (kv "auth" (py-str (name auth)))
                                        (kv "service" (py-str (name service))))
                     request      (conj (kv "request" (py-str (kw-name request))))
                     response     (conj (kv "response" (py-str (kw-name response))))))
         "}")))

(defn python-endpoints
  [{:keys [endpoints]}]
  (str "ENDPOINTS = {\n"
       (apply str (for [[id e] endpoints]
                    (str "    " (py-str (name id)) ": " (py-entry e) ",\n")))
       "}\n"))
