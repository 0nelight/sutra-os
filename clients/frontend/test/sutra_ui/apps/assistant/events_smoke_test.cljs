(ns sutra-ui.apps.assistant.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.apps.assistant.events :as ev]
            [sutra-ui.apps.assistant.evals-events :as ee]
            [sutra-ui.apps.assistant.providers-events :as pe]
            [sutra-ui.apps.assistant.studio-events :as se]
            [sutra-ui.state.machine-testing :refer [run step]]))

(defn- call-maps [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (call-maps r)))

(def ^:private chat (ev/initial {:agent "b" :model nil}))

(deftest loading-picks-the-remembered-agent-and-a-known-model
  (let [[s] (step (assoc chat :stored-model "m2")
                  [:assistant/loaded {:agents  [{:name "a" :model "m1"} {:name "b" :model "m1"}]
                                      :threads [] :models [{:id "m1"} {:id "m2"}]}])]
    (is (= "b" (:agent s)))
    (is (= "m2" (:model s))))
  (testing "a model no longer offered is dropped"
    (let [[s] (step (assoc chat :stored-model "weg")
                    [:assistant/loaded {:agents [{:name "b" :model "m1"}]
                                        :models [{:id "m1"}]}])]
      (is (nil? (:model s))))))

(deftest a-running-thread-is-polled-a-finished-one-reloads-the-list
  (let [s (assoc chat :thread-id "t")]
    (is (= [[:after {:key :poll :ms ev/poll-ms :event [:assistant.thread/poll "t"]}]]
           (second (step s [:assistant.thread/fetched "t" {:messages [{:status "running"}]}]))))
    (is (= [[:dispatch [:assistant/load]]]
           (second (step s [:assistant.thread/fetched "t" {:messages [{:status "done"}]}]))))
    (testing "an answer for a thread no longer open changes nothing"
      (let [[{:keys [ok]}] (call-maps (step s [:assistant.thread/fetch "t"]))
            other         (assoc s :thread-id "u")
            [s' fx]       (step other (conj ok {:messages []}))]
        (is (= {:messages []} (:data (run s (conj ok {:messages []})))))
        (is (= other s'))
        (is (empty? (remove nil? fx)))))))

(deftest asking-sends-the-draft-once
  (let [s (assoc chat :draft "  Wie viele?  " :agent "b")
        [s' :as r] (step s [:assistant/ask "Europe/Vienna"])
        [[f k body]] (calls r)]
    (is (:sending? s'))
    (is (= [api/post! :agent-threads] [f k]))
    (is (= {:agent "b" :input "Wie viele?" :attachments [] :zone "Europe/Vienna"} body))
    (is (empty? (calls (step s' [:assistant/ask nil]))) "not again while sending")
    (testing "not while an attachment uploads"
      (is (empty? (calls (step (assoc s :attachments [{:uploading? true}])
                               [:assistant/ask nil])))))))

(deftest a-new-thread-stops-polling
  (let [[s fx] (step (assoc chat :thread-id "t" :draft "x") [:assistant.thread/new])]
    (is (nil? (:thread-id s)))
    (is (= "" (:draft s)))
    (is (= [[:cancel :poll]] fx))))

(deftest a-failed-upload-drops-the-attachment-and-says-why
  (let [[s :as r]       (step chat [:assistant/attach #js {:name "a.pdf" :type "x"} "k"])
        [{:keys [err]}] (call-maps r)
        s'              (run s (conj err "zu gross"))]
    (is (:uploading? (first (:attachments s))))
    (is (= [] (:attachments s')))
    (is (= "zu gross" (:error s')))))

(deftest choosing-an-agent-is-remembered
  (let [[s [[k [_ key v]]]] (step chat [:assistant/choose-agent "c"])]
    (is (= "c" (:agent s)))
    (is (= [:invoke ev/agent-key "c"] [k key v]))))

(deftest a-running-suite-polls-its-page-and-leaving-stops-it
  (is (= [[:after {:key :poll :ms ee/poll-ms :event [:evals/load]}]]
         (second (step ee/initial [:evals/loaded {:suites [{:running ["m"]}]}]))))
  (is (= [] (second (step ee/initial [:evals/loaded {:suites [{:running []}]}]))))
  (is (= [[:cancel :poll]] (second (step ee/initial [:evals/leave])))))

(deftest a-kept-case-sends-its-figures-and-tells-the-page
  (let [s (assoc ee/case-initial :open :good :reference "r" :must "12, 7 ,")
        [_ :as r] (step s [:evals.case/save {:agent "a" :question "q" :history nil} :reload])
        [[_ _ body]] (calls r)]
    (is (= ["12" "7"] (:must-contain body)))
    (is (= [] (:history body))))
  (is (= "Antwort" (:reference (first (step ee/case-initial
                                            [:evals.case/good "Antwort [[order:12]]"])))))
  (let [[{:keys [ok]}] (call-maps (step ee/case-initial [:evals.case/save {} :reload]))
        [s fx]         (step ee/case-initial (conj ok {}))]
    (is (:saved? s))
    (is (some #{[:invoke [:reload]]} fx))))

(deftest a-provider-change-is-installed-staged-then-read-again
  (let [then              {:set {:key-for nil} :on-change :host}
        [s :as r]         (step (assoc pe/initial :key-for "a")
                                [:providers.commit/installed then {}])
        [{:keys [call ok]}] (call-maps r)
        s'                (run s (conj ok {}))]
    (is (= [api/post! :staging {}] call))
    (is (true? (:busy s)))
    (is (false? (:busy s')))
    (is (nil? (:key-for s')))
    (is (= [[:dispatch [:providers/load]] [:invoke [:host]]]
           (second (step s [:providers.commit/staged then {}]))))))

(deftest a-new-default-model-demotes-the-old-one
  (let [s (assoc pe/initial :data {:models [{:id "alt" :default true} {:id "x"}]})
        [_ :as r] (step s [:providers.model/register {:id "neu" :source :p :default true} nil])
        [[f m others]] (calls r)]
    (is (= pe/register-models! f))
    (is (= "neu" (:id m)))
    (is (= ["alt"] (map :id others)))))

(deftest a-failed-reload-keeps-the-providers-shown
  (let [fail (fn [s] (let [[{:keys [err]}] (call-maps (step s [:providers/load]))]
                       (:data (run s (conj err "weg")))))]
    (is (= {:sources []} (fail (assoc pe/initial :data {:sources []}))))
    (is (rd/failed? (fail pe/initial)))))

(deftest a-new-agent-starts-from-the-template
  (let [[s] (step (assoc se/initial :new-name "Mein Agent" :project "p") [:studio/create "m"])]
    (is (= "mein-agent" (:name s)))
    (is (:new? s))
    (is (= "" (:new-name s)))
    (is (= (se/template "m" "p") (:text s))))
  (testing "staging (not checking) marks the text saved"
    (let [[s fx] (step (assoc se/initial :text "t" :new? true)
                       [:studio/staged false "t" :host {:staged []}])]
      (is (= "t" (:saved s)))
      (is (false? (:new? s)))
      (is (= 2 (count fx))))))
