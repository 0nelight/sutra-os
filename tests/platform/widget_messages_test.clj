(ns ^:unit platform.widget-messages-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.capps.custom-widget :as cw]
            [sutra.capps.iframe-bridge :as bridge]))

(def ^:private schema
  (delay (json/read-str (slurp "boundaries/app-host/widget-messages.schema.json"))))

(defn- defs [& path] (get-in @schema (into ["$defs"] path)))

(def ^:private frame "clients/frontend/src/sutra_ui/widget_frame/core.cljs")
(def ^:private host "clients/frontend/src/sutra_ui/apphost/widgets/custom.cljs")

(defn- posted
  [text]
  (set (map second (re-seq #"post! \[:([a-z][\w-]*)" text))))

(defn- case-keys
  [text marker]
  (let [start    (str/index-of text "(case " (str/index-of text marker))
        end      (->> ["nil)" "(fail (str" "\n\n"]
                      (keep #(str/index-of text % start))
                      (apply min))
        branches (re-seq #"\n( +):([a-z][\w-]*)\s" (subs text start end))
        indent   (second (first branches))]
    (set (keep (fn [[_ i k]] (when (= i indent) k)) branches))))

(deftest the-custom-widget-frame-speaks-the-schema
  (let [f    (slurp frame)
        h    (slurp host)
        up   (set (keys (defs "customWidget" "frameToHost")))
        down (set (keys (defs "customWidget" "hostToFrame")))]
    (testing "frame → host"
      (is (= up (posted f)) "what the frame posts")
      (is (= up (case-keys h "handle (fn")) "what the host handles"))
    (testing "host → frame"
      (is (= down (posted h)) "what the host posts")
      (is (= down (case-keys f "defn- on-message")) "what the frame handles"))
    (testing "the requests a frame may make"
      (is (= (set (get-in (defs "customWidget" "rpcOps") ["enum"]))
             (case-keys h "defn- app-rpc"))))
    (is (= cw/sandbox (defs "customWidget" "x-sandbox")))))

(deftest the-iframe-bridge-speaks-the-schema
  (is (= #{bridge/set-output bridge/trigger-event bridge/resize}
         (set (keys (defs "iframeBridge" "frameToHost")))))
  (is (contains? (set (keys (defs "iframeBridge" "hostToFrame"))) bridge/variable-values)))

(deftest pairing-speaks-the-schema
  (let [text (slurp "libs/core/src/sutra/capps/pairing.cljc")
        used (into (set (map second (re-seq #":type :([a-z][\w-]*)" text)))
                   (case-keys text "defn receive"))]
    (is (= used (set (get-in (defs "pairing") ["properties" "type" "enum"]))))))

(deftest the-manifest-is-the-one-the-release-checks
  (is (= (set (map name @#'cw/csp-directives))
         (set (get-in (defs "manifest") ["properties" "csp" "propertyNames" "enum"]))))
  (is (= (set (map name cw/value-types))
         (set (get-in (defs "manifest" "$defs" "valueType") ["oneOf" 0 "enum"]))))
  (is (= (set (map name cw/scalar-types))
         (set (get-in (defs "manifest" "$defs" "valueType")
                      ["oneOf" 1 "prefixItems" 1 "enum"])))))
