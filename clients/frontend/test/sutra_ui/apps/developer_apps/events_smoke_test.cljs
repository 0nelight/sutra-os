(ns sutra-ui.apps.developer-apps.events-smoke-test
  (:require [cljs.test :refer [deftest is]]
            [sutra-ui.apps.developer-apps.events :as ev]
            [sutra-ui.state.machine-testing :refer [step]]))

(def ^:private declared {:portal {:display-name "Portal" :client-secret "s3"}})

(deftest stored-drafts-come-back-once-with-the-declared-secret
  (let [[s] (step {:drafts {:neu {:display-name "Neu"}}}
                  [:dev-apps/restore {:portal {:display-name "P2"} :neu {:display-name "alt"}}
                   declared])]
    (is (= {:display-name "P2" :client-secret "s3"} (get-in s [:drafts :portal])))
    (is (= "Neu" (get-in s [:drafts :neu :display-name])) "this session's draft wins")
    (is (= [s []] (step s [:dev-apps/restore {:x {}} declared])))))

(deftest a-new-application-gets-a-free-key-and-is-shown
  (let [[_ [[k {:keys [ok]}]]] (step {} [:dev-apps/new declared])]
    (is (= :call k))
    (is (= [:dev-apps/named declared] ok))
    (let [[s] (step {:drafts {:portal-2 {}}} (conj ok " Portal "))]
      (is (= :portal-3 (:selected s)))
      (is (= :overview (:section s)))
      (is (= "Portal" (get-in s [:drafts :portal-3 :display-name]))))
    (is (= [{} []] (step {} (conj ok "  "))))))

(deftest editing-starts-from-the-declaration-and-toggles
  (let [[s] (step {} [:dev-apps/toggle declared :portal [:scope :actions] :ship])
        [s'] (step s [:dev-apps/toggle declared :portal [:scope :actions] :ship])]
    (is (= "Portal" (get-in s [:drafts :portal :display-name])))
    (is (= [:ship] (get-in s [:drafts :portal :scope :actions])))
    (is (= [] (get-in s' [:drafts :portal :scope :actions]))))
  (let [[s] (step {:new-role "ops"} [:dev-apps/role-add declared :neu])]
    (is (= "" (:new-role s)))
    (is (= [:ops] (get-in s [:drafts :neu :permissions :roles])))
    (is (= ev/blank-app (update-in (get-in s [:drafts :neu]) [:permissions :roles] empty)))))
