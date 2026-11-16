(ns ^:unit apps.markup-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.markup :as m]))

(def ^:private opts {:marks [] :size {:w 1000 :h 800} :tol 10})

(defn- run [o st evs]
  (reduce (fn [[s fx] e] (let [[s' f] (m/step s e o)] [s' (into fx f)])) [st []] evs))

(deftest a-measure-is-dragged-or-tapped-twice-and-locks-onto-the-axis
  (is (= [[:create {:kind :dim :a [100 100] :b [400.11 100.0]}]]
         (second (run opts {} [[:tool :dim] [:down [100 100]] [:move [400 108]] [:up [400 108]]]))))
  (is (= [[:create {:kind :dim :a [100 100] :b [100.0 500.02]}]]
         (second (run opts {} [[:tool :dim] [:down [100 100]] [:up [101 100]]
                               [:down [100 500]] [:up [104 500]]])))))

(deftest a-new-measure-starts-on-the-end-of-one-already-there
  (let [o (assoc opts :marks [{:id "a" :kind :dim :a [100 100] :b [400 100]}])]
    (is (= [400 100]
           (get-in (run o {} [[:tool :dim] [:down [403 96]] [:up [403 96]]
                              [:down [400 300]] [:up [400 300]]])
                   [1 0 1 :a])))))

(deftest selecting-grabbing-and-tapping-beside
  (let [o (assoc opts :marks [{:id "a" :kind :dim :a [100 100] :b [400 100]}])]
    (is (= [[:select "a"] [:update "a" {:b [380 140]}]]
           (second (run o {:tool :select} [[:down [250 102]] [:down [398 101]]
                                           [:move [380 140]] [:up [380 140]]]))))
    (testing "a press beside that turns into panning keeps the selection"
      (is (= [] (second (run o {:tool :select :sel "a"} [[:down [700 700]] [:cancel]])))))
    (is (= [[:select nil]]
           (second (run o {:tool :select :sel "a"} [[:down [700 700]] [:up [700 700]]]))))))

(deftest lengths-as-the-apphost-types-them
  (is (= [2345 2345 2345 12 nil] (map m/parse-length ["2345" "2,345 m" "234,5 cm" "12 mm" "x"])))
  (is (= "Breite 2.345" (m/caption {:kind :dim :mm 2345 :label "Breite"}))))

(deftest the-body-and-the-live-entries-carry-the-same-model
  (let [model (-> m/empty-model
                  (assoc-in [:photos "p"] {:file "f" :w 1000 :h 800 :title "Wand" :pos 0})
                  (assoc-in [:marks "a"] {:photo "p" :kind :dim :a [1 2] :b [3 4] :mm 5})
                  (assoc-in [:comments "c"] {:mark "a" :by "x" :text "passt?"}))]
    (is (= model (m/read-body (m/body model))))
    (is (= model (m/from-entries (m/entries model))))
    (is (= m/empty-model (m/read-body "{kaputt")))))
