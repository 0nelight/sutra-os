(ns sutra.sources.conformance
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [is testing]]
            [org.httpkit.server :as http-kit]
            [sutra.sources.protocols :as api]
            [sutra.sources.dialect :as dialect]
            [sutra.store.scan :as scan]
            [sutra.types.coltype :as ctype])
  (:import (java.net URLDecoder)))

(set! *warn-on-reflection* true)

(def fixture
  {:tables
   {"ITEMS"    {:columns [{:name "ID" :type "INTEGER"} {:name "NAME" :type "TEXT"}
                          {:name "STATUS" :type "INTEGER"}]
                :pk "ID"
                :rows [{"ID" 1 "NAME" "alpha" "STATUS" 1}
                       {"ID" 2 "NAME" "beta" "STATUS" 1}
                       {"ID" 3 "NAME" "gamma" "STATUS" 2}]}
    "ITEM_LOG" {:columns [{:name "TABLE_NAME" :type "TEXT"} {:name "PK" :type "TEXT"}
                          {:name "COL" :type "TEXT"} {:name "PREV" :type "TEXT"}
                          {:name "NEXT" :type "TEXT"} {:name "ACTOR" :type "TEXT"}
                          {:name "ITEM_ID" :type "INTEGER"}]
                :references {"ITEM_ID" ["ITEMS" "ID"]}
                :rows []}}
   :relations [{:from-table "ITEM_LOG" :from-col "ITEM_ID" :to-table "ITEMS" :to-col "ID"
                :constraint nil}]})

(def writeback
  {:access :read-write
   :source-name "conformance"
   :writeback {"ITEMS" {:pk "ID"
                        :writable-columns ["NAME" "STATUS"]
                        :column-values {"STATUS" [1 2 3]}
                        :insertable-columns ["NAME" "STATUS"]
                        :identity "ID"
                        :deletable? true
                        :delete-guard ["NAME"]
                        :log {:table "ITEM_LOG"
                              :columns {:table "TABLE_NAME" :pk "PK" :column "COL"
                                        :previous "PREV" :next "NEXT" :actor ["ACTOR"]}}}}})

(defn- by-id [s] (into {} (map (juxt #(str (:id %)) identity)) (api/scan s "ITEMS")))

(defn- refused [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn check-reads [make]
  (let [s (make fixture writeback)]
    (try
      (testing "catalog"
        (is (= ["ITEMS" "ITEM_LOG"] (api/tables s)))
        (is (= [["ID" :integer] ["NAME" :string] ["STATUS" :integer]]
               (mapv (juxt :name :ontology-type) (api/columns s "ITEMS"))))
        (is (some #{{:from-table "ITEM_LOG" :from-col "ITEM_ID" :to-table "ITEMS" :to-col "ID"}}
                  (map #(select-keys % [:from-table :from-col :to-table :to-col])
                       (api/relations s)))))
      (testing "rows come keyed by lower-case keywords"
        (is (= #{"alpha" "beta" "gamma"} (set (map :name (api/scan s "ITEMS")))))
        (is (= #{:id :name :status} (set (keys (first (api/scan s "ITEMS"))))))
        (is (= 3 (count (api/read s "SELECT * FROM ITEMS")))))
      (testing "caps"
        (is (= 2 (count (api/query s "SELECT * FROM ITEMS" 2)))))
      (testing "the guards run before any driver"
        (is (refused #(api/read s "DELETE FROM ITEMS")))
        (is (refused #(api/query s "SELECT 1; DROP TABLE ITEMS" 5)))
        (is (refused #(api/scan s "ITEMS; DROP TABLE ITEMS")))
        (is (= 3 (count (api/scan s "ITEMS")))))
      (testing "probe"
        (is (true? (api/probe s))))
      (testing "a database lands no file"
        (is (= :reach! (:unsupported (refused #(api/reach! s))))))
      (finally
        (api/close! s)
        (api/close! s)))))

(defn check-writes [make]
  (let [s (make fixture writeback)]
    (try
      (testing "update: guarded on the previous value, logged"
        (let [r (api/write! s {:table "ITEMS" :pk-value 1 :set {"NAME" "alpha2"}
                               :expect {"NAME" "alpha"} :actor "tester"})]
          (is (= 1 (:update-count r)))
          (is (= 1 (:logged r)))
          (is (= "alpha2" (:name (get (by-id s) "1"))))
          (is (= [{:col "NAME" :prev "alpha" :next "alpha2" :actor "TESTER" :pk "1"}]
                 (mapv #(select-keys % [:col :prev :next :actor :pk]) (api/scan s "ITEM_LOG"))))))
      (testing "a row someone else changed first is a conflict, not an overwrite"
        (is (:conflict (refused #(api/write! s {:table "ITEMS" :pk-value 1 :set {"NAME" "x"}
                                                :expect {"NAME" "alpha"} :actor "tester"}))))
        (is (= "alpha2" (:name (get (by-id s) "1")))))
      (testing "the allowlist is the driver's too"
        (is (refused #(api/write! s {:table "ITEMS" :pk-value 2 :set {"STATUS" 9}
                                     :expect {"STATUS" 1} :actor "tester"})))
        (is (refused #(api/write! s {:table "OTHER" :pk-value 2 :set {"NAME" "x"}
                                     :expect {"NAME" "beta"} :actor "tester"})))
        (is (= "1" (str (:status (get (by-id s) "2"))))))
      (testing "insert answers the new row's identity"
        (let [r  (api/write! s {:table "ITEMS" :insert {"NAME" "delta" "STATUS" 3}
                                :returning "ID" :actor "tester"})
              id (str (:identity r))]
          (is (= 1 (:insert-count r)))
          (is (= "delta" (:name (get (by-id s) id))))))
      (testing "delete: guarded, and gone"
        (is (:conflict (refused #(api/write! s {:table "ITEMS" :pk-value 2 :delete true
                                                :expect {"NAME" "not-beta"} :actor "tester"}))))
        (is (= 1 (:delete-count (api/write! s {:table "ITEMS" :pk-value 2 :delete true
                                               :expect {"NAME" "beta"} :actor "tester"}))))
        (is (nil? (get (by-id s) "2"))))
      (finally (api/close! s)))))

(def scans
  [{:where [[:= :name "alpha"]]}
   {:where [[:in :status [1 2]]]}
   {:where [[:in :status []]]}
   {:where [[:in :name ["beta" nil]]]}
   {:where [[:= :status 1] [:nil? :name false]]}
   {:where [[:or [:= :name "beta"] [:= :status 2]]]}
   {:where [[:and [:= :status 1] [:or [:= :name "alpha"] [:= :name "gamma"]]]]}
   {:where [[:not [:= :status 1]]]}
   {:where [[:not [:nil? :name true]]]}
   {:where [[:> :id 1] [:<= :id 3]]}
   {:where [[:starts-with :name "al"]]}
   {:where [[:= :status "1"]]}
   {:where [[:= :name 1]]}
   {:where [[:= :missing 1]]}
   {:where [[:nil? :missing true]]}
   {:where [[:not= :name "alpha"]]}
   {:project #{:name}}
   {:project #{:name :missing} :where [[:= :status 1]]}])

(defn- as-set [rows] (frequencies rows))

(defn check-scan [make]
  (let [s (make fixture writeback)]
    (try
      (let [all (api/scan s "ITEMS")]
        (testing "a scan answers what the reference answers over every row"
          (doseq [sc scans]
            (is (= (as-set (api/run-local all sc)) (as-set (api/scan s "ITEMS" sc))) (pr-str sc))))
        (testing "a limit cuts the matching rows, whichever the source returns first"
          (doseq [sc [{:limit 2} {:where [[:= :status 1]] :limit 1}
                      {:where [[:= :name "alpha"]] :limit 5}]]
            (let [got  (api/scan s "ITEMS" sc)
                  want (api/run-local all (dissoc sc :limit))]
              (is (= (min (:limit sc) (count want)) (count got)) (pr-str sc))
              (is (every? (set want) got) (pr-str sc)))))
        (testing "sample is a scan with a limit"
          (is (= 2 (count (api/sample s "ITEMS" 2)))))
        (testing "a table with a key answers entries by it: ordered, paged past a key, projected"
          (let [k  #(str (first %))
                es (api/entries s "ITEMS")]
            (is (= #{"1" "2" "3"} (set (map k es))))
            (is (every? (fn [[key row]] (= (str key) (str (:id row)))) es))
            (is (= ["gamma" "beta" "alpha"]
                   (map (comp :name second) (api/entries s "ITEMS" {:order [[:name :desc]]}))))
            (is (= ["alpha" "beta"]
                   (map (comp :name second)
                        (api/entries s "ITEMS" {:order [[:name :asc]] :limit 2}))))
            (is (= ["2" "3"] (map k (api/entries s "ITEMS" {:after 1}))))
            (is (= [{:name "beta"}]
                   (map second (api/entries s "ITEMS" {:after 1 :limit 1 :project #{:name}}))))
            (is (= ["3"]
                   (map k (api/entries s "ITEMS" {:where [[:= :status 2]] :after 1 :limit 5}))))
            (is (= ["1" "2" "3"]
                   (loop [after nil acc []]
                     (let [[e] (api/entries s "ITEMS"
                                            (cond-> {:limit 1} after (assoc :after after)))]
                       (if e (recur (first e) (conj acc (k e))) acc))))
                "paging one entry at a time reaches every row once")))
        (testing "a foreign scan takes no :agg, and orders or pages only a table with a key"
          (is (refused #(api/scan s "ITEMS" {:agg {:count? true}})))
          (is (refused #(api/scan s "ITEM_LOG" {:order [[:pk :asc]]})))
          (is (refused #(api/entries s "ITEM_LOG")))
          (is (refused #(api/scan s "ITEMS; DROP TABLE ITEMS" {:limit 1})))))
      (finally (api/close! s)))))

(def invoking
  (assoc writeback :webhooks {:notify {:method :post :path-template "/notify/{id}" :params [:id]}}))

(defn check-invoke
  [make called]
  (let [s (make fixture invoking)]
    (try
      (testing "a declared webhook is called with its params"
        (is (= 200 (:status (api/invoke! s :notify {:id "7"}))))
        (is (= [{:webhook-id :notify :params {:id "7"}}] (called s))))
      (testing "an undeclared webhook, or params other than declared, never leave"
        (is (refused #(api/invoke! s :other {:id "7"})))
        (is (refused #(api/invoke! s :notify {:id "7" :extra 1})))
        (is (= 1 (count (called s)))))
      (finally (api/close! s)))))

(defn check-all [make]
  (check-reads make)
  (check-writes make)
  (check-scan make))

(def ^:private api-rows
  (vec (for [i (range 1 8)] {"id" i "name" (str "n" i) "status" (mod i 2)})))

(defn- query-of [qs]
  (into {} (for [kv (str/split (str qs) #"&") :when (seq kv)
                 :let [[^String k v] (str/split kv #"=" 2)]]
             [(URLDecoder/decode k "UTF-8") (URLDecoder/decode (str v) "UTF-8")])))

(defn- json-resp [status body & [headers]]
  {:status status :headers (merge {"Content-Type" "application/json"} headers)
   :body (json/write-str body)})

(defn- fake-api
  [log]
  (fn [{:keys [uri headers query-string request-method]}]
    (let [q     (query-of query-string)
          _     (swap! log conj (cond-> {:uri uri :method request-method :query q}
                                  (get headers "app-token")
                                  (assoc :app-token (get headers "app-token"))))
          rows  (filter (fn [r] (every? (fn [[k v]] (if-let [[_ a] (re-matches #"where\[(.+)\]" k)]
                                                      (= v (str (get r a)))
                                                      true))
                                        q))
                        api-rows)
          n     (count rows)
          page  (fn [from size] (vec (take size (drop from rows))))]
      (cond
        (#{"/open" "/apirest.php/initSession"} uri)
        (if (= "user_token U" (get headers "authorization"))
          (json-resp 200 {"session_token" "T"})
          (json-resp 401 {"error" "bad token"}))

        (#{"/close" "/apirest.php/killSession"} uri) (json-resp 200 {})

        (not= "T" (get headers "session-token")) (json-resp 401 {"error" "no session"})

        (str/starts-with? uri "/notify/") (json-resp 201 {"ok" true})

        (#{"/range" "/apirest.php/Computer"} uri)
        (let [[a b] (map parse-long (str/split (get q "range" "0-99") #"-"))]
          (json-resp (if (< b (dec n)) 206 200) (page a (inc (- b a)))))

        (= "/offset" uri) (json-resp 200 (page (parse-long (q "offset")) (parse-long (q "limit"))))
        (= "/page" uri)   (let [size (parse-long (q "per_page"))]
                            (json-resp 200 (page (* size (dec (parse-long (q "page")))) size)))
        (= "/cursor" uri) (let [from (parse-long (get q "cursor" "0")) size (parse-long (q "limit"))
                                more (< (+ from size) n)]
                            (json-resp 200
                                       (cond-> {"data" (page from size)}
                                         more (assoc "next" (str (+ from size))))))
        (= "/link" uri)   (let [from (parse-long (get q "from" "0")) to (+ from 3)]
                            (json-resp 200 (page from 3)
                                       (when (< to n)
                                         {"Link"
                                            (str "<http://" (get headers "host") "/link?from=" to
                                                 ">; rel=\"next\"")})))
        (= "/none" uri)   (json-resp 200 rows)
        :else             (json-resp 404 {"error" uri})))))

(defn with-fake-api
  [f]
  (let [log (atom [])
        srv (http-kit/run-server (fake-api log)
                                 {:port 0 :ip "127.0.0.1" :legacy-return-value? false})]
    (try (f (str "http://127.0.0.1:" (http-kit/server-port srv)) log)
         (finally (http-kit/server-stop! srv)))))

(defn rest-spec [base]
  {:type   :rest
   :egress [(subs base (count "http://"))]
   :rest   {:base-url base
            :auth {:mode :session :user-token "U"
                   :open {:path "/open" :headers {"Authorization" "user_token {user-token}"}}
                   :token-at [:session_token] :header "Session-Token" :close {:path "/close"}}
            :resources {:range  {:path "/range" :paging {:style :range :size 3}
                                 :filters {:= {:param "where[{attr}]" :exact? true}}}
                        :offset {:path "/offset" :paging {:style :offset :size 3}}
                        :page   {:path "/page" :paging {:style :page :size 3}}
                        :cursor {:path "/cursor" :paging {:style :cursor :size 3 :cursor-at [:next]}
                                 :records-at [:data]}
                        :link   {:path "/link" :paging {:style :link-header :size 3}}
                        :none   {:path "/none"}}
            :operations {:notify {:method :post :path-template "/notify/{id}" :params [:id]}}}})

(defn check-rest
  [make]
  (with-fake-api
    (fn [base log]
      (let [s (make (rest-spec base))]
        (testing "resources are the catalog"
          (is (= ["cursor" "link" "none" "offset" "page" "range"] (api/tables s)))
          (is (= #{"id" "name" "status"} (set (map :name (api/columns s "range"))))))
        (testing "every paging style walks every row, keyed by lower-case keywords"
          (doseq [t ["range" "offset" "page" "cursor" "link" "none"]]
            (is (= (mapv #(update-keys % keyword) api-rows) (api/scan s t)) t)))
        (testing "a filter the API declares is pushed, and the answer is the reference's"
          (reset! log [])
          (is (= [{:id 3 :name "n3" :status 1}] (api/scan s "range" {:where [[:= :name "n3"]]})))
          (is (some #(= "n3" (get-in % [:query "where[name]"])) @log)))
        (testing "a filter the API does not declare is computed here"
          (is (= [{:id 3 :name "n3" :status 1}] (api/scan s "offset" {:where [[:= :name "n3"]]}))))
        (testing "a limit stops the paging"
          (reset! log [])
          (is (= 2 (count (api/scan s "range" {:limit 2}))))
          (is (= 1 (count (filter #(= "/range" (:uri %)) @log)))))
        (testing "every session opened is closed"
          (is (= (count (filter #(= "/open" (:uri %)) @log))
                 (count (filter #(= "/close" (:uri %)) @log)))))
        (testing "an operation is an invocation"
          (is (= 201 (:status (api/invoke! s :notify {:id "9"}))))
          (is (some #(= "/notify/9" (:uri %)) @log))
          (is (refused #(api/invoke! s :other {:id "9"}))))
        (testing "an undeclared resource and an API off the egress allowlist are refused"
          (is (refused #(api/scan s "nope")))
          (is (thrown? Exception
                       (api/scan (make (assoc (rest-spec base) :egress ["elsewhere:80"]))
                                 "range"))))))))

(def lowering-columns
  {:ctext    {:name "CTEXT"    :type "TEXT"}
   :cvarchar {:name "CVARCHAR" :type "VARCHAR(20)"}
   :cint     {:name "CINT"     :type "INTEGER"}
   :cdecimal {:name "CDECIMAL" :type "DECIMAL(10,2)"}
   :cdouble  {:name "CDOUBLE"  :type "DOUBLE PRECISION"}
   :cbool    {:name "CBOOL"    :type "BOOLEAN"}})

(def ^:private probe-values
  {:text    ["" "x" "y"]
   :varchar ["" "x" "y"]
   :int     [-1 0 1 2]
   :decimal [0M 1.50M 2M]
   :double  [0.0 1.5 2.5]
   :boolean [true false]})

(def ^:private class-value
  {:string "x" :integer 1 :decimal 1.50M :double 1.5 :boolean true})

(def lowering-ops
  [:= :not= :< :<= :> :>= :in :nil? :starts-with :has])

(defn- pred-of [op attr v]
  (case op
    :nil?        [:nil? attr true]
    :in          [:in attr [v]]
    :starts-with [:starts-with attr (str v)]
    [op attr v]))

(def ^:private sql-comparisons #{:= :not= :< :<= :> :>= :in})
(def ^:private sql-text-kinds #{:varchar :text})
(def ^:private sql-exact-number-kinds #{:tinyint :smallint :int :bigint :decimal})

(defn- bindable?
  [kind vclass]
  (case vclass
    :string            (contains? sql-text-kinds kind)
    (:integer :decimal) (contains? sql-exact-number-kinds kind)
    false))

(defn- sql-narrows?
  [op kind vclass]
  (case op
    :nil?        true
    :has         false
    :starts-with (boolean (and (= :string vclass) (contains? sql-text-kinds kind)))
    (boolean (and (contains? sql-comparisons op) (bindable? kind vclass)))))

(defn- empty-answer?
  [pred attr kind]
  (not-any? #(scan/holds? [nil {attr %}] pred)
            (conj (get probe-values kind []) nil)))

(def ^:private trivial {"1=1" :any "1=0" :false})

(defn- emitted
  [type pred]
  (let [[sql] (dialect/scan-stmt type "T" lowering-columns
                                 #(scan/holds? [nil {}] %) {:where [pred]})
        cond  (str/trim (second (str/split sql #" WHERE " 2)))]
    (get trivial cond :narrowed)))

(defn- declared
  [profile pred]
  (first (:where (scan/offer profile {:where [pred]}))))

(defn lowering-matrix
  [type profile]
  (vec (for [[attr col] (sort-by key lowering-columns)
             :let  [kind (:kind (ctype/parse (:type col)))]
             op    lowering-ops
             vclass (if (= :nil? op) [:none] (sort (keys class-value)))
             :let  [pred (pred-of op attr (class-value vclass))]]
         {:op op :attr attr :kind kind :vclass vclass :pred pred
          :wanted   (cond (empty-answer? pred attr kind)   :false
                          (sql-narrows? op kind vclass)    :narrowed
                          :else                            :any)
          :emitted  (emitted type pred)
          :declared (declared profile pred)})))

(def lowering-verdicts
  {:not-built   "SQL can bind this and the dialect does not: the table is read and filtered here"
   :scans-empty "the semantics answer this for nobody, and the dialect emits 1=1: a full table read for an empty answer"
   :scans-all   "declared by the profile, emitted as 1=1: a narrowing promised that never happens"
   :dead-build  "the dialect binds this and the profile never sends it: the builder is unreachable"
   :coarse      "bound and declared :inexact, so :limit and :order are withheld from the source"})

(defn lowering-gaps
  [matrix]
  (vec (for [{:keys [wanted emitted declared] :as row} matrix
             :let [v (cond
                       (and (= :false wanted) (= :any emitted))        :scans-empty
                       (and (= :narrowed wanted) (= :any emitted))     :not-built
                       (and (= :any emitted) (not= :none declared))    :scans-all
                       (and (= :narrowed emitted) (= :none declared))  :dead-build
                       (and (= :narrowed wanted) (= :narrowed emitted)
                            (= :inexact declared))                     :coarse)]
             :when v]
         (assoc row :verdict v :why (lowering-verdicts v)))))

(defn check-lowering
  ([type profile] (check-lowering type profile {}))
  ([type profile allowed]
   (let [matrix (lowering-matrix type profile)
         gaps   (remove (fn [{:keys [verdict op kind]}]
                          (let [a (get allowed verdict #{})]
                            (or (= :all a) (contains? a [op kind]))))
                        (lowering-gaps matrix))]
     (testing (str "lowering: " (name type))
       (doseq [[verdict _] (sort-by key lowering-verdicts)
               :let [rows (filter #(= verdict (:verdict %)) gaps)]]
         (is (empty? rows)
             (str (name verdict) " — " (lowering-verdicts verdict) ":\n"
                  (str/join "\n" (for [r (sort-by (juxt :op :kind) rows)]
                                   (str "  " (pr-str (:pred r))
                                        " on " (name (:kind r))
                                        ": wanted " (name (:wanted r))
                                        ", emitted " (name (:emitted r))
                                        ", declared " (name (:declared r)))))))))
     matrix)))
