(ns ^:unit platform.install-findings-index-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [platform.push-index-test]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.plan :as plan]
            [sutra.catalog.world-install :as world-install]
            [sutra.world.db :as kdb]
            [support.every-index :as every]))

(def ^:private g-sql
  (gen/let [q   (gen/elements ["select x, q, id from t" "select x from t" "select y, z from t"
                               "select x, is_deleted, fk from t"])
            out (gen/one-of
                 [(gen/let [t (gen/elements [:a :b :c :neu]) r (gen/elements [nil :supplement])
                            pk (gen/elements [nil :x [:x :y]])]
                    (cond-> {:kind :object-type :object-type t}
                      r  (assoc :role r)
                      pk (assoc :primary-key pk)))
                  (gen/let [l (gen/elements [:a :b :x]) fk (gen/elements [:x :fk :q])]
                    {:kind :link-type :link-type l :fk-attribute fk})
                  (gen/let [d (gen/elements [:a :b :v])] {:kind :dataset :dataset d})])
            lang (gen/elements [:sql :sql :python])
            sv   (gen/elements [nil "1.0.0" "2.0.0"])
            wm   (gen/elements [nil :replace])
            inc? gen/boolean]
    (cond-> {:language lang :query q :output out}
      sv   (assoc :semantic-version sv)
      wm   (assoc :write-mode wm)
      inc? (assoc :incremental true))))

(def ^:private g-object
  (gen/let [attrs (gen/elements [[[:x :string]] [[:x :string] [:y [:decimal 10 2]]]
                                 [[:y :string] [:q :integer]] []])
            pk    (gen/elements [nil :x])
            mks   (gen/elements [nil [:x] [:x :y]])
            res?  (gen/frequency [[4 (gen/return false)] [1 (gen/return true)]])
            back  (gen/frequency [[4 (gen/return nil)]
                                  [1 (gen/elements [{:dataset :a :primary-key :x}
                                                    {:dataset :v :primary-key :y
                                                     :replaces :b}])]])
            mode  (gen/elements [nil :merge])
            log?  (gen/frequency [[5 (gen/return false)] [1 (gen/return true)]])]
    (cond-> {:attributes attrs}
      pk   (assoc :primary-key pk)
      mks  (assoc :match-keys mks)
      res? (assoc :resolve-entity true)
      back (assoc :backing back)
      mode (assoc :reconcile-mode mode)
      log? (assoc :requires-action-log true))))

(def ^:private g-action
  (gen/let [t  (gen/elements [:a :b :c :neu])
            op (gen/elements [:update :create :create-link :delete-object])
            l  (gen/elements [:a :b :x])]
    {:params {:p {:type t}}
     :rules  [(cond-> {:object-type t :op op :target [:get :p]}
                (= :update op)      (assoc :set [:x])
                (= :create-link op) (assoc :link-type l :from-id :p :to-id :p))]}))

(def ^:private g-text
  (gen/let [os (gen/map (gen/elements [:a :b :c :neu]) g-object {:max-elements 3})
            ls (gen/vector (gen/tuple (gen/elements [:a :b :c :x]) (gen/elements [:a :b :neu])
                                      (gen/elements [:a :b :c])) 0 2)
            ts (gen/map (gen/elements [:a :b :t :u]) g-sql {:max-elements 3})
            as (gen/map (gen/elements [:a :b :setz]) g-action {:max-elements 2})
            ds (gen/elements [nil {:v {:virtual true :source :src :table "t" :columns [:x]}}])]
    (cond-> {}
      (seq os) (assoc :objects os)
      (seq ls) (assoc :links (vec (for [[n f t] ls] [n f t :many-to-one {}])))
      (seq ts) (assoc :transforms ts)
      (seq as) (assoc :actions as)
      ds       (assoc :datasets ds))))

(def ^:private g-edits
  (gen/let [ts (gen/set (gen/elements [:a :b :c]))
            ps (gen/set (gen/tuple (gen/elements [:a :b :c]) (gen/elements [:x :y :q])))
            n  (gen/elements [0 3])]
    {:types (delay ts) :properties (delay ps) :rows (delay (zipmap [:a :b :c] (repeat n)))}))

(def ^:private g-projected
  (gen/let [os (gen/set (gen/elements ["a" "b" "c"]))
            ts (gen/set (gen/elements ["a" "b" "c" "t"]))]
    {:objects os :links #{"a" "b"} :transforms ts :functions #{"a"} :datasets #{"b"}}))

(def ^:private world @#'platform.push-index-test/world)

(defn- outcome [f]
  (try {:ok (f)}
       (catch Exception e {:thrown [(class e) (ex-message e)]})))

(defn- introspect [_code body] [body nil])

(defn- found [text W plan edits ix]
  (outcome #(let [d (diff/diff text W plan edits ix)]
              {:found (vec (plan/diff-findings d))
               :sql   (vec (sql-findings/staged-sql-specs d))})))

(defspec the-findings-read-by-name-are-those-read-whole 3000
  (prop/for-all [W world text g-text edits g-edits
                 strict gen/boolean projected (gen/one-of [(gen/return nil) g-projected])]
    (let [ix   (live/index-of (kdb/db W world-install/views))
          opts (cond-> {:strict? strict :introspect introspect}
                 projected (assoc :strict-projected (delay projected)))
          p    (outcome #(plan/plan-schema text W opts))]
      (or (contains? p :thrown)
          (= (found text W (:ok p) edits (every/index W)) (found text W (:ok p) edits ix))))))
