(ns ^:unit apps.var-catalog-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.variables :as vars]))

(def ^:private formen-auf-prod
  [[:from :where] [:default] [:agg :of] [:default :type]
   [:default :external-id :in-url :type] [:object-property :of :type] [:source]
   [:default-agg] [:as :default] [:aggregate] [:expr] [:expr :type]
   [:join :source] [:from] [:function] [:agg :of :type]
   [:default-agg :external-id :in-url :type] [:agg :of :where]
   [:default :external-id :type] [:from :join] [:args :function]
   [:source :where] [:coalesce] [:args :function :scenario]
   [:default :external-id :in-url :input? :type] [:compare]
   [:aggregate :scenario] [:object-property :of]
   [:default :external-id :in-url :push? :type]])

(def ^:private operatoren-auf-prod
  [:= :not= :> :>= :<= :in :contains :truthy])

(defn- als-spec [ks] (zipmap ks (repeat nil)))

(deftest jede-art-ist-vollstaendig-beschrieben
  (doseq [{:keys [id label hint blank fields runtime]} cat/var-kinds]
    (testing (str id)
      (is (string? label) "eine Art ohne Beschriftung erscheint als rohes Schlüsselwort")
      (is (string? hint))
      (is (map? blank))
      (is (seq fields))
      (is (contains? #{:state :derived :aggregate :function :compare} runtime))
      (doseq [{:keys [key kind options]} fields]
        (is (keyword? key))
        (is (keyword? kind))
        (is (or (nil? options) (contains? cat/var-field-options options))
            (str "unbekannte Options-Liste: " options))))))

(deftest eine-frische-variable-behaelt-ihre-art
  (doseq [{:keys [id blank]} cat/var-kinds]
    (is (= id (cat/var-kind blank))
        (str "eine neu angelegte " id
             " liest sich sonst als etwas anderes und das Formular schlägt um"))))

(defn- gefuellt [m] (into {} (map (fn [[k v]] [k (if (nil? v) :x v)])) m))

(deftest katalog-und-laufzeit-bleiben-sich-einig
  (doseq [{:keys [id blank runtime]} cat/var-kinds]
    (is (= runtime (vars/var-kind (gefuellt blank)))
        (str id ": die Laufzeit liest diese Deklaration anders als der Katalog"))))

(deftest leer-gelassen-liest-die-laufzeit-anders-als-der-katalog
  (doseq [[spec katalog] {{:function nil} :function
                          {:compare nil}  :compare}]
    (is (= katalog (cat/var-kind spec)))
    (is (= :derived (vars/var-kind spec)))))

(deftest jede-form-auf-prod-hat-ein-formular
  (doseq [ks formen-auf-prod
          :let [spec   (als-spec ks)
                art    (cat/var-kind spec)
                fehlen (filter :unknown (cat/var-fields spec))]]
    (testing (str (pr-str ks) " → " art)
      (is (contains? cat/var-kind-index art))
      (is (empty? fehlen)
          (str "ohne Feld, also nur als EDN erreichbar: " (pr-str (mapv :key fehlen)))))))

(deftest jeder-operator-der-laeuft-ist-auch-beschriftet
  (is (= (into (set (keys vars/where-ops)) vars/row-ops) (set (map :op cat/pred-ops)))
      "ein Operator, den die Laufzeit kann und der Katalog nicht nennt, ist für
       den Autor nicht vorhanden — und einer, den nur der Katalog nennt, lügt")
  (doseq [op operatoren-auf-prod]
    (is (string? (:label (cat/pred-op-index op)))
        (str op " läuft auf prod, hat aber keine Beschriftung"))))

(deftest ein-operator-sagt-selbst-was-rechts-steht
  (testing "ohne Operand — ein Wertfeld daneben lädt zu Unsinn ein"
    (is (= :none (cat/pred-operand :truthy)))
    (is (= :none (cat/pred-operand :falsy))))
  (testing "eine Liste, nicht ein Wert"
    (is (= :values (cat/pred-operand :in))
        "prod schreibt [:status-id :in [6 1 11 14]] — mit einem Einzelfeld
         wäre das nur als EDN zu tippen"))
  (testing "der Normalfall"
    (is (= :value (cat/pred-operand :=)))
    (is (= :value (cat/pred-operand :contains))))
  (testing "ein unbekannter Operator bekommt den Normalfall, nicht nil"
    (is (= :value (cat/pred-operand :gibtsnicht)))))
