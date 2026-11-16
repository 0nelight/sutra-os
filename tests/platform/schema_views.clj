(ns ^:unit platform.schema-views
  (:require [sutra.objects.views :as views]
            [sutra.types.schema-view :as sv]
            [sutra.types.object-set :as os]
            [support.helpers :refer [check section]]))

(defn- base
  []
  {:object-types
   {:switch {:object-type-name :switch
             :attribute-list   [{:attribute-name :name :attribute-type :string}]
             :implements       [:network-device]
             :symbol           "🔲"
             :plural "Switches"}
    :router {:object-type-name :router
             :attribute-list   [{:attribute-name :name :attribute-type :string}]
             :implements       [:network-device]
             :plural "Routers"}
    :port   {:object-type-name :port
             :attribute-list   [{:attribute-name :label :attribute-type :string}]
             :plural "Ports"}}
   :link-types
   {:has-port  {:link-type-name :has-port
                :from-object-type     :network-device
                :to-object-type       :port
                :cardinality          :one-to-many}
    :cable     {:link-type-name :cable
                :from-object-type     :port
                :to-object-type       :port
                :cardinality          :one-to-one}}
   :interfaces
   {:network-device {:interface-name :network-device
                     :extends             []
                     :property-list       [{:attribute-name :name :attribute-type :string}]
                     :link-constraints    []}}})

(defn run-tests []
  (println "\n══ Schema-Views Unit Tests ══")

  (section "view-shape is shared code — the wire never carries it")
  (let [s  (base)
        vs (sv/view-shape s)]
    (check "object-types keyed like the schema" (= (set (keys (:object-types s)))
                                                   (set (keys (:object-types vs)))))
    (check ":objects is the fields of :object-types"
           (= (:objects vs) (into {} (for [[k o] (:object-types vs)] [k (:fields o)]))))
    (check "derive-all does NOT ship it"
           (not (contains? (views/derive-all s) :view-shape))))

  (section "implementers index")
  (let [impl (views/implementers (base))]
    (check ":network-device gets both implementers"
           (= [:router :switch] (vec (:network-device impl))))
    (check "no entry for non-interface keys"
           (not (contains? impl :switch))))

  (section "endpoint-types resolves interface to concrete object-types")
  (let [ep (views/endpoint-types (base))]
    (check ":has-port :from expands :network-device → {:switch :router}"
           (= #{:switch :router} (get-in ep [:has-port :from])))
    (check ":has-port :to is just #{:port}"
           (= #{:port} (get-in ep [:has-port :to])))
    (check ":cable self-loops on :port"
           (and (= #{:port} (get-in ep [:cable :from]))
                (= #{:port} (get-in ep [:cable :to])))))

  (section "derive-all bundles all three under the expected keys")
  (let [out (views/derive-all (base))]
    (check ":implementers present"   (map? (:implementers out)))
    (check ":endpoint-types present" (map? (:endpoint-types out))))

  (section "matrix-index — owner-options")
  (let [mx (views/matrix-index (base))]
    (check "switch and router are owners"
           (= [:router :switch] (vec (sort (:owner-options mx)))))
    (check "port is not a candidate owner"
           (not (some #{:port} (:owner-options mx)))))

  (section "matrix-index — outgoing-otm interface-expanded")
  (let [mx (views/matrix-index (base))]
    (check ":switch has :has-port outgoing"
           (= [{:name :has-port :child-type :port}]
              (get (:outgoing-otm-by-owner mx) :switch)))
    (check ":router also has :has-port outgoing"
           (= [{:name :has-port :child-type :port}]
              (get (:outgoing-otm-by-owner mx) :router))))

  (section "matrix-index — peer-edges (self-loop on :port)")
  (let [mx (views/matrix-index (base))]
    (check ":port has :cable as peer-edge"
           (= [:cable] (mapv :name (get (:peer-edges-by-child mx) :port))))
    (check ":switch has no peer-edges"
           (nil? (get (:peer-edges-by-child mx) :switch))))

  (section "matrix-index — cell-edges and marker (extended schema)")
  (let [        ext (-> (base)
                (assoc-in [:object-types :vlan]
                          {:object-type-name :vlan
                           :attribute-list [{:attribute-name :vid :attribute-type :number}]
                           :plural "Vlanes"})
                (assoc-in [:link-types :on-vlan]
                          {:link-type-name :on-vlan
                           :from-object-type :port
                           :to-object-type :vlan
                           :cardinality :many-to-many})
                (assoc-in [:link-types :pvid]
                          {:link-type-name :pvid
                           :from-object-type :port
                           :to-object-type :vlan
                           :cardinality :many-to-one}))
        mx  (views/matrix-index ext)
        pe  (get-in mx [:cell-edges-by-pair [:port :vlan]])]
    (check "pair has both edges"
           (= #{:on-vlan :pvid} (set (mapv :name (:edges pe)))))
    (check "marker is :pvid (the lone :many-to-one)"
           (= :pvid (:marker pe)))
    (check ":vlan is a candidate other-type for (:port :has-port)"
           (= [:vlan] (get-in mx [:other-options-by-child-conn [:port :has-port]]))))

  (section "matrix-index — marker is nil when two M2Os exist")
  (let [ext (-> (base)
                (assoc-in [:object-types :vlan]
                          {:object-type-name :vlan :attribute-list [] :plural "Vlanes"})
                (assoc-in [:link-types :pvid-a]
                          {:from-object-type :port :to-object-type :vlan
                           :cardinality :many-to-one})
                (assoc-in [:link-types :pvid-b]
                          {:from-object-type :port :to-object-type :vlan
                           :cardinality :many-to-one}))
        mx  (views/matrix-index ext)
        pe  (get-in mx [:cell-edges-by-pair [:port :vlan]])]
    (check "two M2Os → no unambiguous marker"
           (nil? (:marker pe))))

  (section "build-object-set threads :order-by + :sort-dir (descending queries)")
  (let [bos (fn [spec] (or (os/ir->qt (#'views/build-object-set (base) spec nil)) {}))]
    (check ":sort-dir rides through alongside :order-by"
           (= :desc (:sort-dir (bos {:find :switch :order-by :name :sort-dir :desc}))))
    (check ":order-by carried"
           (= :name (:order-by (bos {:find :switch :order-by :name :sort-dir :desc}))))
    (check "bare :order-by → no :sort-dir key (compile-query defaults asc)"
           (not (contains? (bos {:find :switch :order-by :name}) :sort-dir)))
    (check ":sort-dir without :order-by is dropped (no dangling direction)"
           (not (contains? (bos {:find :switch :sort-dir :desc}) :sort-dir))))

  (section "view-fields resolves value-types — the FE sees the display, not the ref")
  (let [s   (-> (base)
                (assoc :value-types
                       {:eur-amount {:value-type-name :eur-amount :type :decimal
                                     :format {:kind :currency :currency "EUR" :decimals 2}}})
                (assoc-in [:object-types :switch :attribute-list]
                          [{:attribute-name :name :attribute-type :string}
                           {:attribute-name :preis :attribute-type :decimal
                            :opts {:value-type :eur-amount}}
                           {:attribute-name :rabatt :attribute-type :decimal
                            :opts {:value-type :eur-amount
                                   :format {:kind :percentage :decimals 1}}}]))
        by  (into {} (map (juxt :name identity))
                  (get-in (sv/view-shape s) [:object-types :switch :fields]))]
    (check "a property that only NAMES the value-type carries its format"
           (= {:kind :currency :currency "EUR" :decimals 2}
              (get-in by [:preis :opts :format])))
    (check "the property still wins per key — it may tighten one and inherit the rest"
           (= {:kind :percentage :currency "EUR" :decimals 1}
              (get-in by [:rabatt :opts :format])))
    (check "an opt-free property stays a bare {:name :type}"
           (= {:name :name :type :string} (:name by))))

  (section "empty schema is a degenerate but valid input")
  (let [out (views/derive-all {:object-types {} :link-types {} :interfaces {}})]
    (check "view-shape has empty objects"
           (= {} (:objects (sv/view-shape {:object-types {} :link-types {} :interfaces {}}))))
    (check "implementers is empty map"    (= {} (:implementers out)))
    (check "endpoint-types is empty map"  (= {} (:endpoint-types out)))
    (check "matrix has no owners"         (= [] (:owner-options (:matrix out)))))

  (println "\n══ Schema-Views Unit Tests: DONE ══\n"))
