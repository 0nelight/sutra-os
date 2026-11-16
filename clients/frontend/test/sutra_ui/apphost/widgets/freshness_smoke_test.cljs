(ns sutra-ui.apphost.widgets.freshness-smoke-test
  (:require [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [reagent.ratom :as ratom]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.widgets.freshness :as fw]
            [sutra.i18n.core :as i18n]
            [sutra-ui.render-ctx :as rctx]))

(defn- ago [ms] (.toISOString (js/Date. (- (js/Date.now) ms))))

(def ^:private items
  [{:object-type :rechnung
    :sources [{:resource :rechnung_raw :override-name "SAP-Rechnungen"}
              {:resource :rechnung-bau}
              {:resource :gesperrt}]}
   {:object-type :nie}
   {:object-type :versteckt}])

(defn- rendered
  [locale]
  (rctx/install! locale)
  (set! js/document #js {:hidden false})
  (let [asked  (atom nil)
        timers (atom 0)
        real   js/setInterval
        env    {:frame (frame/capp-frame)
                :descriptor
                {:data-freshness-fn
                 (fn [its]
                   (reset! asked its)
                   (js/Promise.resolve
                    {:updates [{:kind "object-type" :name "rechnung" :at (ago (* 2 3600000))}
                               {:kind "object-type" :name "nie" :at nil}
                               {:kind "source" :name "rechnung_raw" :at (ago (* 30 60000))}
                               {:kind "source" :name "rechnung-bau"
                                :at "2025-07-17T13:52:00Z"}]}))}}
        live   (ratom/make-reaction (fn [] nil))
        render #(binding [ratom/*ratom-context* live]
                  (rds/render-to-string (fw/data-freshness env {:items items})))]
    (set! js/setInterval (fn [_ _] (swap! timers inc) 0))
    (render)
    (js/Promise.
     (fn [ok]
       (js/setTimeout
        #(try
           (ok {:asked asked :timers timers :html (render)})
           (finally
             (set! js/setInterval real)
             (rctx/install! "de")))
        0)))))

(deftest last-updated-per-object-type-and-source
  (async done
    (.then
     (rendered "en")
     (fn [{:keys [html asked timers]}]
       (is (= items @asked) "the host reads exactly the configured items")
       (is (= 1 @timers) "the widget keeps itself current")
       (testing "within 24 hours relative"
         (is (str/includes? html "2 hours ago"))
         (is (str/includes? html "30 min ago")))
       (testing "past 24 hours absolute"
         (is (re-find #"Jul 17, 2025, \d+:52 [AP]M" html)))
       (testing "the override names the source, else the resource does"
         (is (str/includes? html "SAP-Rechnungen"))
         (is (not (str/includes? html "rechnung_raw")))
         (is (str/includes? html "rechnung-bau")))
       (testing "never indexed says so"
         (is (str/includes? html (i18n/t :ui/data-freshness-never))))
       (testing "what the server left out is not shown"
         (is (not (str/includes? html "gesperrt")))
         (is (not (str/includes? html "versteckt"))))
       (is (str/includes? html (i18n/t :ui/data-freshness-last-updated)))
       (done)))))
