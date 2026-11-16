(ns sutra-ui.apphost.widgets.comments-smoke-test
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [reagent.ratom :as ratom]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.widgets.comments :as cw]
            [sutra-ui.apphost.frame :as frame]))

(def ^:private cfg {:object {:id 7 :object-type :rechnung}})

(defn- thread [& texts]
  {:me "anna" :comments (mapv (fn [t] {:id t :author "anna" :body t}) texts)})

(defn- later
  [& fs]
  (reduce (fn [p f] (.then p (fn [_] (js/Promise. (fn [ok] (js/setTimeout #(ok (f)) 0))))))
          (js/Promise.resolve nil) fs))

(deftest a-post-replaces-the-thread-it-answers
  (async done
    (let [calls (atom [])
          f     (frame/capp-frame)
          env   {:frame f :descriptor {:id "app"}}
          live  (ratom/make-reaction (fn [] nil))
          html  #(binding [ratom/*ratom-context* live]
                   (rds/render-to-string [cw/comments cfg env]))
          then  (atom nil)
          real  client/comments!
          ok!   (fn [i x] ((:ok (second (nth @calls i))) x))
          slot  (atom nil)]
      (set! client/comments!
            (fn [_ spec]
              (js/Promise. (fn [ok fail] (swap! calls conj [spec {:ok ok :fail fail}])))))
      (is (str/includes? (html) "sutra-loading"))
      (html)
      (is (= [{:object-type "rechnung" :id "7" :op :list}] (map first @calls))
          "the thread is read once")
      (ok! 0 (thread "erster"))
      (later
       #(do (is (str/includes? (html) "erster"))
            (let [[s {:keys [key]}] (first (:reads @(:db f)))]
              (reset! slot [s key])
              (frame/send! f [::cw/post s key
                              [client/comments! {} (assoc key :op :add :text "zweiter")]
                              (fn [x] (reset! then x))])
              (is (= :add (:op (first (second @calls)))))
              (ok! 1 (thread "erster" "zweiter"))))
       #(let [h (html)]
          (is (str/includes? h "zweiter") "the answer of the post is the thread shown")
          (is (= 2 (count @calls)) "and is not asked again")
          (is (= 2 (count (:comments @then))) "the post's follow-up gets its answer")
          (let [[s key] @slot]
            (frame/send! f [::cw/post s key [client/comments! {} key] nil])
            ((:fail (second (nth @calls 2))) (js/Error. "abgelehnt"))))
       #(do (is (str/includes? (html) "abgelehnt"))
            (let [[s key] @slot]
              (frame/send! f [::cw/post s {:id "8"} [client/comments! {} key] nil])
              (ok! 3 (thread "fremd"))))
       #(do (is (not (str/includes? (html) "fremd")))
            (set! client/comments! real)
            (done))))))
