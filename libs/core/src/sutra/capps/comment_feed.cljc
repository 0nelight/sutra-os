(ns sutra.capps.comment-feed
  (:require [clojure.string :as str]))

(def ^:private token #"@\[([^\]]+)\]\((user|object):([^)\s]+)\)")

(defn mention-token
  [{:keys [label ref-kind ref]}]
  (str "@[" (str/replace (str label) #"[\[\]]" "") "](" (name ref-kind) ":" ref ")"))

(defn segments
  [body]
  (let [body (str body)]
    (loop [from 0
           ms   (re-seq token body)
           out  []]
      (if-let [[whole label k r] (first ms)]
        (let [at (str/index-of body whole from)]
          (recur (+ at (count whole))
                 (rest ms)
                 (cond-> out
                   (> at from) (conj {:text (subs body from at)})
                   true        (conj {:mention {:label label :ref-kind (keyword k) :ref r}}))))
        (cond-> out
          (< from (count body)) (conj {:text (subs body from)}))))))

(defn mentioned-users
  [body]
  (into [] (comp (keep :mention) (filter #(= :user (:ref-kind %))) (map :ref) (distinct))
        (segments body)))

(defn recipients
  [thread body author]
  (->> (concat (map :author thread)
               (mapcat (comp mentioned-users :body) thread)
               (mentioned-users body))
       (remove #(or (nil? %) (= (str %) (str author))))
       (map str)
       distinct
       vec))

(defn typed-mention
  [text caret]
  (let [head (subs (str text) 0 (min (or caret 0) (count (str text))))]
    (when-let [[_ q] (re-find #"(?:^|\s)@([^\s@\]\[]*)$" head)]
      {:query q :from (- (count head) (count q) 1)})))

(defn insert-mention
  [text caret {:keys [from]} mention]
  (let [text (str text)]
    (str (subs text 0 from) (mention-token mention) " "
         (subs text (min (or caret from) (count text))))))

(defn candidates
  [users refs query]
  (let [q    (str/lower-case (str query))
        hit? #(str/includes? (str/lower-case (str (:label %))) q)]
    (vec (take 10 (filter hit? (concat users refs))))))

(defn deletable?
  [{:keys [author]} username]
  (and (some? username) (= (str author) (str username))))

(defn- log-entry
  [{:keys [updated_at edit_kind edited_by values]}]
  {:feed :action :at updated_at :author edited_by :edit-kind edit_kind :values values})

(defn feed
  [comments log]
  (vec (sort-by (comp str :at)
                (concat (map #(assoc % :feed :comment) comments) (map log-entry log)))))
