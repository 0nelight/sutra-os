(ns ^:unit platform.tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.tree :as tree]))

(def ^:private body
  {:objects {:a {:title "A"} :b {:title "B"}}
   :links   [[:a-b :a :b :many-to-one]]
   :version 3})

(deftest a-node-is-a-section-and-a-name
  (is (= [[:objects "a"] [:objects "b"] [:links "a-b"]] (map first (tree/nodes body)))
      "a map section by its key, a vector section by its head; a scalar holds no node")
  (is (= {:key :a-b :value [:a-b :a :b :many-to-one] :vector? true}
         (get (tree/index body) [:links "a-b"]))))

(deftest a-selection-keeps-the-named-nodes-and-the-kind-of-each-section
  (is (= {:objects {:b {:title "B"}}}
         (tree/select body #{[:objects "b"]}))
      "a section left empty goes")
  (is (= {:links [[:a-b :a :b :many-to-one]]} (tree/select body #{[:links "a-b"]}))))

(deftest changes-name-each-node-added-removed-changed
  (let [after (-> body
                  (assoc-in [:objects :a :title] "A2")
                  (update :objects dissoc :b)
                  (update :links conj [:b-a :b :a :many-to-one]))
        {:keys [added removed changed]} (tree/changes body after)]
    (is (= [[:links "b-a"]] (keys added)))
    (is (= [[:objects "b"]] (keys removed)))
    (is (= {:title "B"} (:value (get removed [:objects "b"]))) "what goes, as it was")
    (is (= {[:objects "a"] {:title "A2"}} (update-vals changed :value)))))

(deftest a-list-is-named-by-its-elements
  (is (= [["nr" {:attribute-name "nr"}] ["x" {:attribute-name "x"}]]
         (tree/named [{:attribute-name "nr"} {:attribute-name "x"}]))
      "maps by the first element-key all of them carry")
  (is (= [["nr" [:nr :string]] ["name" [:name :string]]]
         (tree/named [[:nr :string] [:name :string]])) "a tuple by its head")
  (is (= [["a" :a] ["b" "b"]] (tree/named [:a "b"])) "anything else by itself")
  (is (= [] (tree/named [])) "an empty list has no element, and is still a list")
  (testing "never a naming that loses an element"
    (is (nil? (tree/named [[:nr :string] [:nr :long]])) "two of one name")
    (is (nil? (tree/named [:a "a"])) "two of one name, spelt differently")
    (is (nil? (tree/named [{:attribute-name "nr"} {:type "x"}])) "a map without the key")
    (is (nil? (tree/named "abc")) "no list")))

(deftest the-order-of-a-long-list-is-kept
  (let [xs (mapv #(vector (keyword (str "f" %)) :string) (range 20))]
    (is (= (mapv first xs) (mapv (comp first second) (tree/named xs))))))

(deftest a-node-is-taken-apart-into-elements
  (let [cut (fn [_ f v] (when (= :attributes f) (tree/named v)))]
    (is (= {:title "A" [:attributes] ::listed-marker
            [:attributes "nr"] [:nr :string]}
           (-> (tree/elements cut :objects {:title "A" :attributes [[:nr :string]]})
               (update [:attributes] (constantly ::listed-marker))))
        "a cut field is [field] — that it is a list — and [field name] per part")
    (is (= {tree/whole [:a-b :a :b]} (tree/elements cut :links [:a-b :a :b]))
        "a value that is no map is one element")))

(deftest a-world-cuts-every-list-of-named-maps-and-nothing-else
  (is (some? (tree/lists-of-maps nil :attribute-list [{:attribute-name "a"}])))
  (is (= [] (tree/lists-of-maps nil :attribute-list [])))
  (is (nil? (tree/lists-of-maps nil :primary-key ["a" "b"])) "a list of names stays whole")
  (is (nil? (tree/lists-of-maps nil :attribute-list [{:attribute-name "a"}
                                                     {:attribute-name "a"}]))))

(deftest an-element-is-labelled-as-a-reader-names-it
  (is (= "description" (tree/label :description)))
  (is (= "attribute-list" (tree/label [:attribute-list])))
  (is (= "attribute-list/datei" (tree/label [:attribute-list "datei"])))
  (is (nil? (tree/label tree/whole)))
  (is (nil? (tree/label nil))))

(def ^:private by-attributes {:cut (fn [_ f v] (when (= :attributes f) (tree/named v)))})

(defn- m3
  ([b m t] (m3 b m t nil))
  ([b m t opts] (tree/merge3 b m t opts)))

(deftest fields-one-side-each-changed-merge
  (let [b {:objects {:a {:title "A" :plural "As"}}}
        {:keys [body conflicts]} (m3 b (assoc-in b [:objects :a :title] "A1")
                                     (assoc-in b [:objects :a :plural] "As2"))]
    (is (empty? conflicts))
    (is (= {:title "A1" :plural "As2"} (get-in body [:objects :a])))))

(deftest a-node-only-one-side-changed-is-that-side-whole
  (let [b {:objects {:a {:title "A"} :b {:title "B"}}}
        {:keys [body conflicts]} (m3 b (update b :objects dissoc :b)
                                     (assoc-in b [:objects :c] {:title "C"}))]
    (is (empty? conflicts))
    (is (= {:a {:title "A"} :c {:title "C"}} (:objects body)))))

(deftest the-same-field-changed-differently-is-a-conflict-on-that-field
  (let [b {:objects {:a {:title "A" :plural "As"}}}
        mine   (-> b (assoc-in [:objects :a :title] "mine") (assoc-in [:objects :a :plural] "P"))
        theirs (assoc-in b [:objects :a :title] "theirs")
        {:keys [body conflicts]} (m3 b mine theirs)]
    (is (= [{:node [:objects "a"] :field :title}] conflicts))
    (is (= {:title "theirs" :plural "As"} (get-in body [:objects :a]))
        "an undecided node stays as theirs")))

(deftest a-delete-against-an-edit-is-a-conflict-on-the-node
  (let [b {:objects {:a {:title "A"}}}]
    (is (= [{:node [:objects "a"] :field nil}]
           (:conflicts (m3 b {:objects {}} (assoc-in b [:objects :a :title] "A2")))))
    (is (= [{:node [:objects "a"] :field nil}]
           (:conflicts (m3 b (assoc-in b [:objects :a :title] "A2") {:objects {}})))
        "either way round")))

(deftest two-who-create-one-node-merge-field-by-field
  (is (= {:title "A" :plural "As"}
         (get-in (:body (m3 {} {:objects {:a {:title "A"}}} {:objects {:a {:plural "As"}}}))
                 [:objects :a])))
  (is (= [{:node [:objects "a"] :field :title}]
         (:conflicts (m3 {} {:objects {:a {:title "A"}}} {:objects {:a {:title "B"}}})))))

(deftest a-tuple-is-one-element
  (let [b {:links [[:a-b :a :b :many-to-one]]}]
    (is (= [{:node [:links "a-b"] :field tree/whole}]
           (:conflicts (m3 b {:links [[:a-b :a :b :one-to-one]]}
                           {:links [[:a-b :a :b :many-to-many]]}))))
    (is (= [[:a-b :a :b :one-to-one] [:b-a :b :a :many-to-one]]
           (:links (:body (m3 b {:links [[:a-b :a :b :one-to-one]]}
                              (update b :links conj [:b-a :b :a :many-to-one])))))
        "a vector section stays one: the first side's nodes, then the other's")))

(def ^:private listed {:objects {:a {:attributes [[:nr :string] [:name :string]]}}})

(defn- add [b part] (update-in b [:objects :a :attributes] conj part))
(defn- attrs [r] (get-in r [:body :objects :a :attributes]))

(deftest parts-of-a-cut-field-merge-by-name
  (let [r (m3 listed (add listed [:farbe :string]) (add listed [:gewicht :double]) by-attributes)]
    (is (empty? (:conflicts r)))
    (is (= [[:nr :string] [:name :string] [:farbe :string] [:gewicht :double]] (attrs r))
        "the first side's order, then what only the other holds"))
  (is (= [[:nr :string] [:name :string] [:gewicht :double] [:farbe :string]]
         (attrs (m3 listed (add listed [:farbe :string]) (add listed [:gewicht :double])
                    (assoc by-attributes :first :theirs))))
      ":first :theirs keeps theirs' order")
  (testing "without a cut the field is one element, and the same additions collide"
    (is (= [{:node [:objects "a"] :field :attributes}]
           (:conflicts (m3 listed (add listed [:farbe :string])
                           (add listed [:gewicht :double])))))))

(deftest the-same-part-changed-both-ways-is-a-conflict-on-that-part
  (is (= [{:node [:objects "a"] :field [:attributes "name"]}]
         (:conflicts (m3 listed
                         (assoc-in listed [:objects :a :attributes 1] [:name :text])
                         (assoc-in listed [:objects :a :attributes 1] [:name :long])
                         by-attributes)))))

(deftest a-part-removed-on-one-side-goes
  (is (= [[:nr :string] [:gewicht :double]]
         (attrs (m3 listed (update-in listed [:objects :a :attributes] (comp vec butlast))
                    (add listed [:gewicht :double]) by-attributes)))))

(deftest a-list-emptied-stays-a-list
  (let [b (assoc-in listed [:objects :a :title] "A")
        r (m3 b (assoc-in b [:objects :a :attributes] [])
              (assoc-in b [:objects :a :title] "A2") by-attributes)]
    (is (empty? (:conflicts r)))
    (is (= {:title "A2" :attributes []} (get-in r [:body :objects :a])))))

(deftest two-parts-of-one-name-are-never-merged-into-one
  (let [b      {:objects {:a {:attributes [[:nr :string]]}}}
        mine   (add b [:nr :long])
        theirs (add b [:name :string])
        r      (m3 b mine theirs by-attributes)]
    (is (seq (:conflicts r)) "mine cannot be cut any more: no silent pick of one :nr")
    (is (= (get-in theirs [:objects :a :attributes]) (attrs r)))))

(deftest a-long-list-keeps-its-order-through-a-merge
  (let [xs (mapv #(vector (keyword (str "f" %)) :string) (range 20))
        b  {:objects {:a {:attributes xs}}}
        r  (m3 b (add b [:mine :string]) (add b [:theirs :string]) by-attributes)]
    (is (= (concat xs [[:mine :string] [:theirs :string]]) (attrs r)))))

(deftest a-resolution-decides-only-what-conflicts
  (let [b      {:objects {:a {:title "A" :plural "As"}}}
        mine   (assoc-in b [:objects :a :title] "mine")
        theirs (-> b (assoc-in [:objects :a :title] "theirs") (assoc-in [:objects :a :plural] "P"))]
    (is (= {:title "mine" :plural "P"}
           (get-in (:body (m3 b mine theirs {:resolve :mine})) [:objects :a]))
        "theirs' other change survives mine's decision")
    (is (= {:title "theirs" :plural "P"}
           (get-in (:body (m3 b mine theirs {:resolve :theirs})) [:objects :a])))
    (testing "a resolution per node"
      (let [b2 (assoc-in b [:objects :b] {:title "B"})
            m2 (assoc-in mine [:objects :b :title] "mine")
            t2 (assoc-in theirs [:objects :b :title] "theirs")
            r  (m3 b2 m2 t2 {:resolve {[:objects "a"] :mine}})]
        (is (= "mine" (get-in r [:body :objects :a :title])))
        (is (= [{:node [:objects "b"] :field :title}] (:conflicts r)))))
    (testing "a delete against an edit, decided"
      (is (nil? (get-in (:body (m3 b {:objects {}} theirs {:resolve :mine})) [:objects :a])))
      (is (= theirs (:body (m3 b {:objects {}} theirs {:resolve :theirs})))))))

(deftest a-merge-over-tokens-decides-as-over-values
  (let [token  #(hash [::t %])
        cut    (:cut by-attributes)
        tokens (tree/base listed cut token)
        mine   (add listed [:farbe :string])
        theirs (add listed [:gewicht :double])]
    (is (= (m3 listed mine theirs by-attributes)
           (tree/merge3-over tokens mine theirs (assoc by-attributes :token token))))
    (is (contains? (get tokens [:objects "a"]) [:attributes "nr"]))))

(deftest scalars-and-sections-come-from-the-first-side
  (let [r (m3 {:v 1 :objects {}} {:v 2 :objects {}} {:v 3 :objects {} :links []})]
    (is (= {:v 2 :objects {} :links []} (:body r))))
  (is (= 3 (:v (:body (m3 {:v 1} {:v 2} {:v 3} {:first :theirs}))))))
