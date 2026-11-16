(ns sutra.cli.session
  (:require [clojure.string :as str]
            [sutra.cli.out :as out]))

(defn new-session
  [{:keys [branch project ontology identity] :as opts}]
  (atom {:branch    (or branch "main")
         :project   project
         :ontology  ontology
         :identity  identity
         :staged    0
         :register  {:rows [] :named {}}
         :opts      opts}))

(defn put-register!
  [sess rows]
  (swap! sess assoc-in [:register :rows] (vec rows))
  sess)

(defn save-set!
  [sess nm]
  (swap! sess assoc-in [:register :named (str nm)]
         (get-in @sess [:register :rows] []))
  (get-in @sess [:register :named (str nm)]))

(defn rows      [sess] (get-in @sess [:register :rows] []))
(defn named-set [sess nm] (get-in @sess [:register :named (str nm)]))

(def ^:private pronoun-re
  #"^(#\d+(?:-\d+)?|\$[A-Za-z][\w-]*|\$|@me)$")

(defn pronoun? [tok] (boolean (and (string? tok) (re-matches pronoun-re tok))))

(defn- nth-row! [sess n]
  (let [rs (rows sess)]
    (when (or (< n 1) (> n (count rs)))
      (throw (ex-info (str "no #" n " in the last result (" (count rs) " row"
                           (when (not= 1 (count rs)) "s") ")")
                      {:pronoun (str "#" n) :available (count rs)})))
    (nth rs (dec n))))

(defn identity-of
  [sess]
  (force (:identity @sess)))

(defn resolve-pronoun
  [sess tok]
  (when (pronoun? tok)
    (cond
      (= tok "@me")
      (if-let [u (:username (identity-of sess))]
        [u]
        (throw (ex-info "@me is unknown — not logged in as a named principal" {})))

      (= tok "$")
      (mapv :id (rows sess))

      (str/starts-with? tok "$")
      (if-let [s (named-set sess (subs tok 1))]
        (mapv :id s)
        (throw (ex-info (str "no saved set " tok " (save one with `save-set "
                             (subs tok 1) "`)") {:pronoun tok})))

      :else
      (let [body (subs tok 1)]
        (if (str/includes? body "-")
          (let [[a b] (map parse-long (str/split body #"-"))]
            (mapv :id (map #(nth-row! sess %) (range a (inc b)))))
          [(:id (nth-row! sess (parse-long body)))])))))

(defn set-branch!   [sess b] (swap! sess assoc :branch b) sess)
(defn set-project!  [sess p] (swap! sess assoc :project p) sess)
(defn set-staged!   [sess n] (swap! sess assoc :staged (or n 0)) sess)
(defn set-identity! [sess m] (swap! sess assoc :identity m) sess)

(defn prompt
  [sess]
  (let [{:keys [branch staged]} @sess
        on-main? (= branch "main")
        inner    (str branch (when (pos? (or staged 0)) (str " *" staged)))]
    (str (out/paint (if on-main? [:bold :red] :bold) (str "sutra[" inner "]"))
         (out/paint (if on-main? :red :cyan) "› "))))
