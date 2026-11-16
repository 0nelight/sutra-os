(ns ^:unit platform.control-write-gate-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]
            [support.mem :as mem]))

(def ^:private editor-a
  {:user "u" :operations #{:read} :project-operations {"ri.p.a" #{:read :update}}})

(defn- denied? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (boolean (:sutra/acl-denied (ex-data e))))))

(defn- unnamed? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (boolean (re-find #"names its authority" (ex-message e))))))

(defn- doc [id project] (cond-> {:id id :branch "main" :name id} project (assoc :project project)))

(def ^:private sys grants/system-authority)

(deftest a-document-is-written-only-where-the-writer-may-change
  (let [store (mem/store)
        a  editor-a]
    (ctl/upsert-as! store sys :documents (doc "d-b" "ri.p.b"))
    (testing "in its own project, by every write function"
      (is (not (denied? #(ctl/upsert-as! store a :documents (doc "d-a" "ri.p.a")))))
      (is (not (denied? #(ctl/update-as! store a :documents {:branch "main" :id "d-a"}
                                         {:name "x"}))))
      (is (not (denied? #(ctl/plan-as! store a [(fn [store t]
                                               (ctl/swapping store :documents
                                                             {:branch "main" :id "d-a"}
                                                             (fn [o] (assoc o :name "y")) t))])))))
    (testing "in another project: refused, also through a plan"
      (is (denied? #(ctl/upsert-as! store a :documents (doc "d-c" "ri.p.b"))))
      (is (denied? #(ctl/update-as! store a :documents {:branch "main" :id "d-b"} {:name "x"})))
      (is (denied? #(ctl/delete-as! store a :documents {:branch "main" :id "d-b"})))
      (is (denied? #(ctl/plan-as! store a [(fn [store _] (ctl/deleting store :documents
                                                                 {:branch "main" :id "d-b"}))]))))
    (testing "a move needs both sides"
      (is (denied? #(ctl/update-as! store a :documents {:branch "main" :id "d-a"}
                                    {:project "ri.p.b"})))
      (is (= "ri.p.a" (:project (ctl/get store :documents {:branch "main" :id "d-a"})))
          "nothing was written"))
    (testing "outside any project nothing is gated, not even an unnamed write"
      (is (not (denied? #(ctl/upsert-as! store a :documents (doc "d-n" nil)))))
      (is (not (unnamed? #(ctl/upsert! store :documents (doc "d-m" nil))))))
    (testing "a write that names no authority changes nothing in a project, by any verb"
      (is (unnamed? #(ctl/upsert! store :documents (doc "d-a" "ri.p.a"))))
      (is (unnamed? #(ctl/update! store :documents {:branch "main" :id "d-a"} {:name "z"})))
      (is (unnamed? #(ctl/swap! store :documents {:branch "main" :id "d-a"}
                                (fn [o] (assoc o :name "z")))))
      (is (unnamed? #(ctl/delete! store :documents {:branch "main" :id "d-a"})))
      (is (unnamed? #(ctl/plan! store [(fn [store _] (ctl/deleting store :documents
                                                             {:branch "main" :id "d-a"}))])))
      (is (= "y" (:name (ctl/get store :documents {:branch "main" :id "d-a"})))))))

(deftest a-placement-belongs-to-its-folder's-project
  (let [store   (mem/store)
        node (fn [rid kind parent project]
               (cond-> {:rid rid :branch "main" :node-kind kind :name rid}
                 parent  (assoc :parent-rid parent)
                 project (assoc :project project)))]
    (ctl/upsert-as! store sys :atlas-nodes (node "f-a" :folder nil "ri.p.a"))
    (ctl/upsert-as! store sys :atlas-nodes (node "f-b" :folder nil "ri.p.b"))
    (is (not (denied? #(ctl/upsert-as! store editor-a :atlas-nodes
                                       (node "n-1" :placement "f-a" nil)))))
    (is (denied? #(ctl/upsert-as! store editor-a :atlas-nodes (node "n-2" :placement "f-b" nil))))
    (testing "moving a placement out of the writer's projects"
      (is (denied? #(ctl/update-as! store editor-a :atlas-nodes {:branch "main" :rid "n-1"}
                                    {:parent-rid "f-b"}))))))
