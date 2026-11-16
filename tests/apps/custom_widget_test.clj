(ns ^:unit apps.custom-widget-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.custom-widget :as cw]))

(def ^:private widget
  {:main 'a.b/view
   :parameters {:count {:type :number} :tags {:type [:array :string]}}
   :events {:bump {:updates [:count]} :ping {}}})

(def ^:private files {"src/a/b.cljs" "(ns a.b)"})

(deftest versions
  (is (= "1.10.0" (cw/latest ["1.9.0" "1.10.0" "dev/me"])))
  (is (= "1.10.0" (cw/resolve-version ["1.9.0" "1.10.0"] nil)))
  (is (= "1.10.0" (cw/resolve-version ["1.9.0" "1.10.0"] "latest")))
  (is (= "1.9.0" (cw/resolve-version ["1.9.0" "1.10.0"] "1.9.0")))
  (testing "a pin that names nothing shows nothing, not something else"
    (is (nil? (cw/resolve-version ["1.9.0"] "2.0.0")))))

(deftest manifest
  (is (empty? (cw/manifest-problems {:widgets {:w widget}} files)))
  (is (seq (cw/manifest-problems {} files)))
  (is (seq (cw/manifest-problems {:widgets {:w (assoc widget :main 'nope)}} files)))
  (is (seq (cw/manifest-problems {:widgets {:w (assoc widget :main 'x.y/view)}} files)))
  (is (seq (cw/manifest-problems {:widgets {:w (assoc widget :css "gone.css")}} files)))
  (is (seq (cw/manifest-problems
            {:widgets {:w (assoc-in widget [:parameters :count :type] :blob)}} files)))
  (is (seq (cw/manifest-problems
            {:widgets {:w (assoc-in widget [:events :bump :updates] [:gone])}} files)))
  (testing "the frame's scripts stay shut, and a source cannot break out of the attribute"
    (is (seq (cw/manifest-problems {:widgets {:w widget} :csp {:script-src ["*"]}} files)))
    (is (seq (cw/manifest-problems {:widgets {:w widget} :csp {:connect-src ["\"><x"]}} files)))))

(deftest frame-document
  (let [doc (cw/frame-document "https://h" {:csp {:connect-src ["https://api.example.com"]}})]
    (is (re-find #"script-src https://h/js/widget/runtime\.js" doc))
    (is (re-find #"connect-src https://api\.example\.com" doc)))
  (is (not (re-find #"connect-src" (cw/frame-document "https://h" {}))))
  (is (not (re-find #"allow-same-origin" cw/sandbox))))

(deftest emitting
  (let [bound {:variables {:count :h} :events {:bump :e}}]
    (is (= {:effects [[:set :count 2] [:event :bump]]}
           (cw/emit-effects widget bound :bump {:count 2})))
    (testing "only what the event declares is written"
      (is (:refused (cw/emit-effects widget bound :ping {:count 2})))
      (is (:refused (cw/emit-effects widget bound :unknown {}))))
    (testing "an unbound parameter or event simply does nothing"
      (is (= {:effects []} (cw/emit-effects widget {} :bump {:count 2}))))))

(deftest bindings
  (is (empty? (cw/binding-problems widget {:variables {:count :h} :events {:bump :e}})))
  (is (= 2 (count (cw/binding-problems widget {:variables {:x :h} :events {:y :e}})))))

(defn- layout [binding]
  {:kind :rows
   :vars {:menge {:source :beitrag} :stand {:default 3} :frei {}}
   :items [{:widget :custom-widget :widget-set "s" :variables {:p binding}}]})

(deftest a-computed-variable-has-no-handle
  (is (seq (cat/reference-problems (layout {:var :menge}))))
  (is (empty? (cat/reference-problems (layout {:val :menge}))))
  (is (empty? (cat/reference-problems (layout {:var :stand}))))
  (is (empty? (cat/reference-problems (layout {:var :frei})))))
