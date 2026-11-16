(ns ent-lsp.cursor
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- delim? [c] (or (Character/isWhitespace (char c)) (#{\, \( \) \[ \] \{ \} \" \;} c)))

(defn- read-atom [s]
  (try (edn/read-string s) (catch Exception _ s)))

(defn- add-element
  [stack v]
  (if-let [top (peek stack)]
    (let [key? (and (= :map (:kind top)) (even? (:n top)))]
      (conj (pop stack) (cond-> (update top :n inc)
                          key? (-> (assoc :key v) (update :keys conj v)))))
    stack))

(defn- open [stack kind]
  (let [top  (peek stack)
        step (when top (if (= :map (:kind top)) (:key top) (:n top)))]
    (conj stack {:kind kind :n 0 :keys #{} :key nil :step step})))

(defn locate
  [before]
  (let [n (count before)]
    (loop [i 0 stack []]
      (if (>= i n)
        (when-let [top (peek stack)]
          {:path   (vec (keep :step (rest stack)))
           :at     (cond (not= :map (:kind top)) :element
                         (even? (:n top))         :key
                         :else                    :value)
           :key    (:key top)
           :index  (:n top)
           :keys   (:keys top)
           :prefix ""})
        (let [c (.charAt ^String before i)]
          (cond
            (or (Character/isWhitespace c) (= c \,)) (recur (inc i) stack)
            (= c \;) (let [j (str/index-of before "\n" i)] (if j (recur (inc j) stack) nil))
            (= c \") (let [j (loop [j (inc i)]
                               (cond (>= j n) nil
                                     (= \\ (.charAt ^String before j)) (recur (+ j 2))
                                     (= \" (.charAt ^String before j)) j
                                     :else (recur (inc j))))]
                       (if j (recur (inc j) (add-element stack :string)) nil))
            (#{\( \[ \{} c) (recur (inc i) (open stack ({\( :list \[ :vector \{ :map} c)))
            (and (= c \#) (< (inc i) n) (= \{ (.charAt ^String before (inc i))))
            (recur (+ i 2) (open stack :set))
            (#{\) \] \}} c) (recur (inc i) (if (seq stack) (add-element (pop stack) :coll) stack))
            :else
            (let [j (loop [j (inc i)]
                      (if (and (< j n) (not (delim? (.charAt ^String before j)))) (recur (inc j))
                        j))
                  tok (subs before i j)]
              (if (= j n)
                (some-> (locate (subs before 0 i)) (assoc :prefix tok))
                (recur j (add-element stack (read-atom tok)))))))))))
