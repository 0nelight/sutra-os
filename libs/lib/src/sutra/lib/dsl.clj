(ns sutra.lib.dsl
  (:require [clojure.pprint :as pp]))

(set! *warn-on-reflection* true)

(defn decl [kind nm spec] {::kind kind ::name nm ::spec spec})

(defn decl? [x] (and (map? x) (contains? x ::kind)))

(defn ref-name
  [x]
  (if (decl? x) (::name x) x))

(defn- split-doc+opts [body]
  (let [[doc body]  (if (string? (first body)) [(first body) (rest body)] [nil body])
        [opts body] (if (map? (first body)) [(first body) (rest body)] [{} body])]
    [doc opts body]))

(defn object-spec [doc opts attributes]
  (cond-> (assoc opts :attributes (vec attributes))
    doc (assoc :description doc)))

(defmacro defobject
  [sym & body]
  (let [[doc opts attrs] (split-doc+opts body)]
    `(def ~sym (decl :objects ~(keyword sym) (object-spec ~doc ~opts ~(vec attrs))))))

(defn link-spec [from card to opts]
  [(ref-name from) (ref-name to) card opts])

(defmacro deflink
  [sym from card to & [opts]]
  `(def ~sym (decl :links ~(keyword sym) (link-spec ~from ~card ~to ~(or opts {})))))

(defn action-spec [doc opts params clauses]
  (cond-> (-> opts
              (merge clauses)
              (update :on ref-name)
              (assoc :params (into (array-map)
                                   (map (fn [[p spec]] [p (update spec :type ref-name)]))
                                   params)))
    (nil? (:on opts)) (dissoc :on)
    doc (assoc :description doc)))

(defmacro defaction
  [sym & body]
  (let [[doc opts [params & clauses]] (split-doc+opts body)
        params (mapv (fn [[p spec]] [(keyword p) spec]) (partition 2 params))]
    `(def ~sym (decl :actions ~(keyword sym)
                     (action-spec ~doc ~opts ~params ~(apply hash-map clauses))))))

(defn object-set-spec [doc opts]
  (cond-> (update opts :root ref-name)
    doc (assoc :description doc)))

(defmacro defobjectset
  [sym & body]
  (let [[doc opts] (split-doc+opts body)]
    `(def ~sym (decl :object-sets ~(keyword sym) (object-set-spec ~doc ~opts)))))

(defn ontology
  [& decls]
  {:schema
   (reduce (fn [s {::keys [kind name spec]}]
             (if (= :links kind)
               (update s :links (fnil conj []) (into [name] spec))
               (assoc-in s [kind name] spec)))
           {}
           (filter decl? (flatten decls)))})

(defn ns-decls
  [ns]
  (->> (vals (ns-interns ns))
       (sort-by (comp :line meta))
       (map deref)
       (filter decl?)))

(defn ns-ontology
  [ns]
  (ontology (ns-decls ns)))

(defn write-ent
  [path ent]
  (spit path (with-out-str (pp/pprint ent))))
