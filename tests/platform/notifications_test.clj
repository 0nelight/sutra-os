(ns ^:unit platform.notifications-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.actions.execution :as x]
            [sutra.actions.operations :as ops]
            [support.system :as tsys]
            [sutra.actions.notify :as anotify-action]
            [sutra.automation.activity :as activity]
            [sutra.automation.notify :as anotify]
            [sutra.notifications.audience :as audience]
            [sutra.notifications.content :as content]
            [sutra.notifications.prefs :as prefs]
            [sutra.parser.operational :as op]
            [sutra.automation.rules :as rules]
            [sutra.automation.runs :as autos]
            [sutra.notifications.inbox :as notes]
            [sutra.access.authority :as authority]
            [sutra.store.access.watch :as watch]
            [sutra.objects.reads :as objects]
            [sutra.store.seat :as seat]
            [support.mem :as mem]))

(def ^:private schema {:object-types {"ri.mail" {:object-type-name :mail :plural "Mails"}}})

(defn- mail [id subject & {:keys [version owner] :or {version 1}}]
  {:id id :values {:subject subject :sender "anna@example.com" :zustaendig owner :rev version}})

(def ^:private effect
  {:notify {:to ["chef"] :to-property :zustaendig
            :heading "Neue Mail: {subject}" :message "von {sender}"}})

(defn- fire! [store auto matched]
  (with-redefs [seat/branch-schema         (constantly schema)
                objects/objects-of-type     (fn [& _] matched)
                rules/owner-authority       (constantly nil)
                autos/mark-fired!           (fn [& _])
                autos/record-run!           (fn [& _])]
    (rules/run-one! store "main"
                    (merge {:id "a1" :owner "chef" :on "mail" :effect-spec (pr-str effect)} auto))))

(deftest templates-and-recipients
  (is (= "Neue Mail: Angebot (a, b)"
         (notes/render "Neue Mail: {subject} ({to})" {:subject "Angebot" :to ["a" "b"]})))
  (is (= "x " (notes/render "x {missing}" {})))
  (is (= ["chef" "bob"] (notes/recipients {:to ["chef" " bob "] :to-property :z} {:z "bob"})))
  (is (= ["a" "b"] (notes/recipients {:to-property :z} {:z ["a" "b" nil]}))))

(deftest an-inbox-holds-what-was-sent-and-what-was-read
  (let [store   (mem/store)
        got  (atom [])
        stop (notes/subscribe! "bob" #(swap! got conj %))]
    (try
      (notes/send! store "main" ["bob" "eve"] {:heading "H" :message "M"})
      (is (= ["H"] (map :heading (notes/inbox store "bob" {}))))
      (is (= ["H"] (map :heading @got)) "an open stream gets it live")
      (is (= 1 (notes/unread-count store "eve")))
      (is (= 1 (notes/mark-read! store "bob" :all)))
      (is (= 0 (notes/unread-count store "bob")))
      (is (= 1 (notes/unread-count store "eve")) "one user's read leaves the other's")
      (is (empty? (notes/inbox store "bob" {:unread? true})))
      (finally (stop)))))

(deftest a-recipient-who-may-not-read-the-object-gets-nothing
  (let [store (assoc (mem/store) :authority {:user "x"})]
    (with-redefs [authority/of-principal (constantly nil)]
      (is (empty? (notes/send! store "main" ["bob"]
                               {:heading "H" :object-type "ri.mail" :object-id "m1"})))
      (is (empty? (notes/inbox store "bob" {}))))))

(deftest a-notify-automation-tells-about-what-is-new
  (with-redefs [notes/visible? (constantly true)]
    (let [store (mem/store)]
      (testing "its first run takes the baseline: nothing already there is notified"
        (fire! store {} [(mail "m1" "Alt" :owner "bob")])
        (is (empty? (notes/inbox store "chef" {}))))
      (testing "a new object is notified to :to and to the user its :to-property names"
        (fire! store {:last-fired-at (java.util.Date.)}
              [(mail "m1" "Alt" :owner "bob") (mail "m2" "Angebot" :owner "bob")])
        (is (= ["Neue Mail: Angebot"] (map :heading (notes/inbox store "chef" {}))))
        (is (= ["von anna@example.com"] (map :message (notes/inbox store "bob" {})))))
      (testing "nothing twice, and a changed object is no new one"
        (fire! store {:last-fired-at (java.util.Date.)}
              [(mail "m1" "Alt" :owner "bob") (mail "m2" "Angebot" :version 2 :owner "bob")])
        (is (= 1 (count (notes/inbox store "chef" {})))))
      (testing ":objects :changed also takes a new version"
        (let [eff (assoc-in effect [:notify :objects] :changed)]
          (with-redefs [seat/branch-schema (constantly schema)
                        objects/objects-of-type
                          (fn [& _] [(mail "m2" "Angebot" :version 3 :owner "bob")])
                        rules/owner-authority (constantly nil)
                        autos/mark-fired! (fn [& _]) autos/record-run! (fn [& _])]
            (rules/run-one! store "main"
                            {:id "a1" :owner "chef" :on "mail" :effect-spec (pr-str eff)
                          :last-fired-at (java.util.Date.)}))
          (is (= 2 (count (notes/inbox store "chef" {})))))))))

(deftest a-live-automation-is-due-when-its-type-changed
  (let [fired (java.util.Date. 1000)
        autos [{:id "a" :on "mail" :live true :last-fired-at fired}
               {:id "b" :on "mail" :live false :last-fired-at fired}
               {:id "c" :on "mail" :live true}]]
    (with-redefs [rules/live-branches (constantly ["main"])
                  seat/branch-schema  (constantly schema)
                  autos/list-all      (constantly autos)]
      (with-redefs [watch/changed-at (constantly nil)]
        (is (= ["c"] (map :id (rules/live-due nil))) "unchanged: only the one that never fired"))
      (with-redefs [watch/changed-at (constantly 5000)]
        (is (= ["a" "c"] (map :id (rules/live-due nil)))
            "changed: every live one fired before it")))))

(deftest the-grammar-takes-a-notify-effect
  (let [ok? #(m/validate op/EntAutomation {:name "neue-mail" :on :mail :live true :effect %})]
    (is (ok? effect))
    (is (ok? {:notify {:to-property :zustaendig :heading "x" :objects :changed}}))
    (is (not (ok? {:notify {:heading "x"}})) "no recipient")
    (is (not (ok? {:notify {:to ["a"]}})) "no heading")
    (is (ok? {:notify {:to ["a"] :content {:function :meldung} :grouping {:by [:partner]}
                       :link {:object-type :partner :id "{partner_id}" :label "Partner"}}}))
    (is (not (ok? {:notify {:to ["a"] :heading "x" :link {:app :a :url "u"}}}))
        "a link has one target")))

(deftest templates-speak-handlebars-and-are-cut-to-counterparts-limits
  (is (= "Anruf von Huber an anna"
         (content/render "Anruf von {{partner.name}} an {{{recipient.username}}}"
                         {:partner {:name "Huber"} :recipient {:username "anna"}})))
  (is (= "a, b" (content/render "{{xs}}" {:xs ["a" "b"]})))
  (let [n (content/sized {:heading (apply str (repeat 300 "x")) :message "kurz"})]
    (is (= 250 (count (:heading n))))
    (is (str/ends-with? (:heading n) "..."))
    (is (= "kurz" (:message n))))
  (is (= "/object/partner/p%201" (content/link-url {:object [:partner "p 1"]} {})))
  (is (= "/apps/leitwarte" (content/link-url {:app :leitwarte} {})))
  (is (= "https://crm/x/7" (content/link-url {:url "https://crm/x/{{id}}"} {:id 7}))))

(deftest a-group-reaches-its-members-and-the-members-of-its-groups
  (let [groups {"einkauf" {:users #{"anna" "bob"} :groups #{"lager"}}
                "lager"   {:users #{"carl"} :groups #{"einkauf"}}}]
    (is (= ["anna" "bob" "carl" "dora"]
           (audience/expand groups ["einkauf" "dora" "bob"]))
        "nested groups, a cycle stops, a plain name is a user, nobody twice")
    (is (= ["a" "b"] (audience/names [" a " :b nil ["a"]])))))

(deftest each-recipient-is-told-the-way-they-asked
  (let [n        {:heading "H" :message "M" :source :health :automation-id "auto"
                  :link-url "/x" :email-html "<b>M</b>"}
        settings {"off"   {:delivery {:health :off}}
                  "mail"  {:delivery {:default :email :health :email}}
                  "mute"  {:muted ["auto"]}}
        p        (notes/plan n ["plain" "off" "mail" "mute"] settings)]
    (is (= ["plain" "off" "mail"] (map :recipient (:rows p))) "muted: not even kept")
    (is (= #{"plain"} (:live p)) "health defaults to both, so plain is told live and by mail")
    (is (= ["plain" "mail"] (map :recipient (:mails p))))
    (is (= "<b>M</b>" (:html (first (:mails p)))))
    (is (str/includes? (:body (first (:mails p))) "Öffnen: /x"))
    (is (= :muted (prefs/way {:muted ["a"]} :automation "a")))
    (is (= :platform (prefs/way {} :action nil)))
    (is (prefs/problems {:delivery {:action :fax}}))))

(deftest grouped-automations-send-one-notification-per-group
  (let [objs [(mail "m1" "A" :owner "bob") (mail "m2" "B" :owner "bob")
              (assoc-in (mail "m3" "C" :owner "bob") [:values :sender] "x@y")]]
    (is (= [3] (map count (anotify/groups :all objs))))
    (is (= [2 1] (map count (anotify/groups {:by [:sender]} objs))))
    (is (= [1 1 1] (map count (anotify/groups nil objs))))
    (is (= 3 (:count (anotify/group-values objs)))))
  (with-redefs [notes/visible? (constantly true)]
    (let [store (mem/store)
          eff   {:notify {:to ["chef"] :heading "{count} neue Mails" :grouping :all
                          :link {:label "Partner" :object-type :partner :id "{sender}"}}}]
      (with-redefs [seat/branch-schema         (constantly schema)
                    objects/objects-of-type     (fn [& _] [(mail "m1" "A") (mail "m2" "B")])
                    rules/owner-authority       (constantly nil)
                    autos/mark-fired!           (fn [& _])
                    autos/record-run!           (fn [& _])]
        (rules/run-one! store "main" {:id "g1" :owner "chef" :on "mail"
                                      :effect-spec (pr-str eff)
                                      :last-fired-at (java.util.Date.)}))
      (let [[n & more] (notes/inbox store "chef" {})]
        (is (nil? more) "one for both")
        (is (= "2 neue Mails" (:heading n)))
        (is (= "/object/partner/anna%40example.com" (:link-url n)))
        (is (= "Partner" (:link-label n)))))))

(def ^:private world
  {:object-types {"ri.ticket" {:object-type-name :ticket}}})

(defn- run-verb [step {:keys [denied ask gate]}]
  (with-redefs [x/reader  (constantly nil)
                x/object  (fn [_ _ id] {:values {:titel (str "Ticket " id) :bearbeiter "bob"}})
                x/ask     (or ask (fn [_ _ {:keys [names]}]
                                    {:result {:allowed (vec (remove (set denied) names))
                                              :denied  (vec (filter (set denied) names))}}))
                x/gate    (or gate (constantly nil))]
    (anotify-action/notify-verb
     step world {:arg0 {:ticket "t1" :prio "hoch"}}
     {:action {:action-name :eskalieren :params {:ticket {:type :ticket} :prio {:type :string}}}
      :effect-gate {:branch "main"}}
     nil)))

(deftest an-action-rule-notifies-who-it-names
  (let [step {:op :notification
              :recipients ["chef" {:param :ticket :property :bearbeiter}]
              :content {:heading "{{ticket.titel}}: {{prio}}" :message "an {{recipient.username}}"}
              :link {:label "Ticket" :object :ticket}}
        r    (run-verb step {})
        [e]  (:side-effects r)
        ds   (get-in e [:outbox :deliveries])]
    (is (= :notification (:op e)))
    (is (= ["chef" "bob"] (map :recipient ds)))
    (is (= "Ticket t1: hoch" (:heading (first ds))))
    (is (= "an bob" (:message (second ds))) "rendered per recipient")
    (is (= "/object/ticket/t1" (:link-url (first ds))))
    (testing "a recipient who may not read the object fails the action, unless :any"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"may not read"
                            (run-verb step {:denied ["bob"]})))
      (is (= ["chef"] (map :recipient (get-in (first (:side-effects
                                                       (run-verb (assoc step :require :any)
                                                                 {:denied ["bob"]})))
                                              [:outbox :deliveries])))))
    (testing "more recipients than allowed fail it"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"more than the 500"
                            (run-verb (assoc step :recipients (mapv str (range 501))) {}))))
    (testing "on a branch that holds notifications back nothing goes to the outbox"
      (let [e (first (:side-effects (run-verb step {:gate (constantly {:outcome :held
                                                                        :side-effect {:x 1}})})))]
        (is (nil? (:outbox e)))
        (is (= {:x 1} (:side-effect e)))))))

(deftest the-outbox-delivers-an-action-notification
  (let [store (mem/store)]
    (#'anotify-action/perform-notification
     {:branch "main" :deliveries [{:recipient "bob" :heading "H" :link-url "/x"}]}
     {:store store})
    (is (= [["H" "/x" "action"]]
           (map (juxt :heading :link-url :source) (notes/inbox store "bob" {}))))))

(deftest the-kernel-runs-a-notification-rule-beside-the-edits
  (tsys/ensure!)
  (with-redefs [x/ask (fn [_ _ {:keys [names]}] {:result {:allowed names :denied []}})]
    (let [world {:object-types {:ticket {:object-type-name :ticket
                                         :attribute-list [{:attribute-name :title
                                                           :attribute-type :string}
                                                          {:attribute-name :owner
                                                           :attribute-type :string}]
                                         :plural "Tickets"}}
                 :link-types {}
                 :actions {:touch {:params {:ticket {:type :ticket} :note {:type :string}}
                                   :rules [{:op :update :target :ticket :values {:title "after"}}
                                           {:op :notification
                                            :recipients [{:param :ticket :property :owner}]
                                            :content {:heading "{{ticket.title}}: {{note}}"}
                                            :link {:object :ticket}}]}}}
          res   (ops/execute-action world {} {:verbs (tsys/verbs)} nil :touch
                                    {:arg0 {:ticket {:id "t-1" :object-type-ref :ticket
                                                     :values {:title "before" :owner "bob"}}
                                            :note "eilt"}})
          [e]   (filter #(= :notification (:op %)) (:side-effects res))]
      (is (= [:update-object] (mapv :op (:ops res))))
      (is (= [{:recipient "bob" :heading "before: eilt" :link-url "/object/ticket/t-1"}]
             (mapv #(select-keys % [:recipient :heading :link-url])
                   (get-in e [:outbox :deliveries])))
          "the rule saw the object as it was before its edit"))))

(deftest an-automation-mutes-itself-after-too-many-failures
  (let [runs (fn [errors] (concat (repeat errors {:status "error"})
                                  (repeat (- 30 errors) {:status "ok"})))]
    (is (activity/auto-mute? (runs 24)) "80 % of 30")
    (is (not (activity/auto-mute? (runs 23))))
    (is (not (activity/auto-mute? (repeat 10 {:status "error"}))) "fewer than 30 runs")))

(deftest activity-falls-into-counterparts-categories
  (is (= :errors (activity/run-category nil {:status "error"})))
  (is (= :recovered (activity/run-category {:status "error"} {:status "ok" :applied 0})))
  (is (= :triggered (activity/run-category {:status "ok"} {:status "ok" :applied 2})))
  (is (nil? (activity/run-category {:status "ok"} {:status "ok" :applied 0})))
  (is (= [:paused :condition-edited]
         (activity/config-events {:paused false :when-spec "a"} {:paused true :when-spec "b"})))
  (is (= :automation-errors (:source (activity/notice {:id "a" :name "n"} :errors "x")))))

(deftest a-muted-automation-evaluates-but-sends-nothing
  (with-redefs [notes/visible? (constantly true)]
    (let [store (mem/store)
          runs  (atom [])]
      (with-redefs [seat/branch-schema         (constantly schema)
                    objects/objects-of-type     (fn [& _] [(mail "m9" "Neu" :owner "bob")])
                    rules/owner-authority       (constantly nil)
                    autos/mark-fired!           (fn [& _])
                    autos/record-run!           (fn [_ _ _ r] (swap! runs conj r))]
        (rules/run-one! store "main" {:id "a1" :owner "chef" :on "mail" :muted true
                                      :effect-spec (pr-str effect)
                                      :last-fired-at (java.util.Date.)}))
      (is (empty? (notes/inbox store "chef" {})))
      (is (= ["muted"] (map :status @runs)))
      (is (= 1 (:matched (first @runs))) "the condition still ran"))))
