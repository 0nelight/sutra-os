(ns sutra-ui.apphost.frame-smoke-test
  (:require [cljs.test :refer [deftest is testing async]]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.state.loading :as loading]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(machine/reg-event ::ask
                   (fn [{:keys [db]} [_ ask]]
                     (rq/request db {:ask  ask
                                     :then [[:capp/put [:x]]]
                                     :else [[:capp/put [:err]]]})))

(machine/reg-event ::fx (fn [_ [_ & fx]] {:fx (vec fx)}))

(machine/reg-event ::set-in-effect
                   (fn [_ [_ c seen]] {:fx [[:invoke [(fn []
                                                        (reset! c 2)
                                                        (vreset! seen @c)
                                                        (swap! c inc))]]]}))

(defn- held-ask
  []
  (let [calls (atom [])]
    [[(fn [x] (js/Promise. (fn [ok fail] (swap! calls conj {:x x :ok ok :fail fail})))) :arg]
     calls]))

(defn- later
  [& fs]
  (reduce (fn [p f] (.then p (fn [_] (js/Promise. (fn [ok] (js/setTimeout #(ok (f)) 0))))))
          (js/Promise.resolve nil) fs))

(deftest a-cursor-reads-what-it-was-set-to
  (let [f (frame/capp-frame {:scopes {:s {:n 1}}})
        c (frame/cursor f [:scopes :s :n])
        heard (atom [])]
    (add-watch c ::w (fn [_ _ o n] (swap! heard conj [o n])))
    (is (= 1 @c))
    (reset! c 5)
    (is (= 5 @c) "reset! outside a handler runs at once")
    (is (= 6 (swap! c inc)))
    (is (= 8 (swap! c + 1 1)))
    (is (= [[1 5] [5 6] [6 8]] @heard))
    (remove-watch c ::w)
    (testing "inside a handler a write is queued behind the event"
      (let [seen (volatile! nil)
            q    (frame/cursor f [:q])]
        (reset! q 0)
        (frame/send! f [::set-in-effect q seen])
        (is (= 0 @seen) "a deref right after a queued reset! reads the old value")
        (is (= 3 @q) "swap! applies its fn when its event runs")))))

(deftest a-late-answer-is-dropped-after-leave
  (async done
    (let [f            (frame/capp-frame)
          [ask calls]  (held-ask)]
      (frame/send! f [::ask ask])
      (is (= :arg (:x (first @calls))))
      ((:ok (first @calls)) 42)
      (later #(is (= 42 (:x @(:db f))))
             #(do (frame/send! f [::ask ask])
                  (frame/leave! f)
                  ((:ok (second @calls)) 99))
             #(do (is (= 42 (:x @(:db f))) "the answer came after leave")
                  (is (nil? (:err @(:db f))))
                  (done))))))

(deftest watches-and-listeners-stop-on-leave
  (let [f        (frame/capp-frame)
        stopped  (atom [])
        changed  (atom nil)
        had-win? (exists? js/window)
        added    (atom {})]
    (when-not had-win?
      (set! js/window #js {:addEventListener    (fn [e h] (swap! added assoc e h))
                           :removeEventListener (fn [e _] (swap! added dissoc e))}))
    (try
      (with-redefs [client/watch! (fn [d {:keys [on-change]}]
                                    (reset! changed on-change)
                                    #(swap! stopped conj (:id d)))]
        (frame/send! f [::fx
                        [:capp/watch {:key :w :descriptor {:id 1} :spec {} :on [:capp/put [:m]]}]
                        [:capp/watch {:key :w :descriptor {:id 2} :spec {} :on [:capp/put [:m]]}]
                        [:capp/listen {:key :l :event "sutra:data-changed"
                                       :on [:capp/put [:heard] true]}]])
        (is (= [1] @stopped) "a second watch of a key replaces the first")
        (@changed {:type "objectSetChanged"})
        (is (= {:type "objectSetChanged"} (:m @(:db f))))
        (when-not had-win?
          ((get @added "sutra:data-changed") nil)
          (is (true? (:heard @(:db f)))))
        (frame/leave! f)
        (is (= [1 2] @stopped))
        (when-not had-win? (is (empty? @added) "the listener is removed"))
        (is (empty? (:stops @(:rt f)))))
      (finally
        (when-not had-win? (js-delete js/globalThis "window"))))))

(deftest a-keyed-read-ignores-an-answer-to-a-stale-key
  (async done
    (let [f           (frame/capp-frame)
          env         {:frame f}
          [ask calls] (held-ask)
          slot        @(machine/subscribe [:capp/at [:reads :s]] {:frame f})
          stale       (atom nil)]
      (is (nil? slot))
      (is (= {:data nil :error nil :loading? true} (frame/read! env :s :k1 ask)))
      (is (= :k1 (:key (get-in @(:db f) [:reads :s]))))
      (is (map? (frame/read! env :s :k1 ask)) "the same key is not asked again")
      (frame/read! env :s :k2 ask)
      (is (= 2 (count @calls)))
      ((:ok (second @calls)) :zwei)
      ((:ok (first @calls)) :eins)
      (later
       #(is (= {:data :zwei :error nil :loading? false} (frame/read! env :s :k2 ask)))
       #(do (frame/send! f [:capp/reread :s])
            (is (= 3 (count @calls)))
            ((:fail (last @calls)) (js/Error. "kaputt")))
       #(do (is (= {:data :zwei :error "kaputt" :loading? false}
                   (frame/read! env :s :k2 ask)))
            (frame/send! f [:capp/drop-read :s])
            (is (nil? (get-in @(:db f) [:reads :s])))
            (frame/read! env :s :k1 ask)
            (reset! stale (last @calls))
            (frame/send! f [:capp/drop-read :s])
            (frame/read! env :s :k1 ask)
            ((:ok @stale) :alt))
       #(do (is (= {:data nil :error nil :loading? true} (frame/read! env :s :k1 ask)))
            (done))))))

(defn- answering-fetch
  [answer]
  (fn [_ _] (if (instance? js/Error answer)
              (js/Promise.reject answer)
              (js/Promise.resolve
               #js {:ok     (< (:status answer) 300)
                    :status (:status answer)
                    :text   (fn [] (js/Promise.resolve (:body answer)))}))))

(defn- door-failure
  [answer]
  (let [real js/fetch
        _    (set! js/fetch (answering-fetch answer))
        p    (client/door! {:id "a"} "/x" {:body {} :fail "Nicht lesbar"})]
    (set! js/fetch real)
    (.then p (fn [_] :answered) identity)))

(deftest door-rejects-with-a-readable-sentence
  (async done
    (-> (js/Promise.all
         #js [(door-failure {:status 403 :body "{}"})
              (door-failure {:status 400 :body "{\"error\":\"kaputt\"}"})
              (door-failure (js/Error. "offline"))])
        (.then (fn [[forbidden refused offline]]
                 (is (= "Nicht lesbar — dafür fehlt die Berechtigung" (ex-message forbidden)))
                 (is (= 403 (:status (ex-data forbidden))))
                 (is (= "kaputt" (loading/problem refused)) "machine :call shows the message")
                 (is (= "Netzwerkfehler: offline" (ex-message offline)))
                 (done))))))
