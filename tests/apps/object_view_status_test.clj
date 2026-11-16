(ns ^:unit apps.object-view-status-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-view :as ent-view]
            [sutra.parser.project-emit :as pemit]))

(def ^:private body {:kind :col :items []})

(defn- lower [file]
  (:applications (ent-view/entv->schema file)))

(defn- emit [apps]
  (#'pemit/emit-view-file :mangel (seq apps)))

(deftest a-view-carries-no-status
  (testing "an Object View opens at its published version, as an app does — the file
            names no :status, and the lowering mints none, so `kernel/merge-existing`
            keeps the live publish pointer"
    (is (every? (comp nil? :status val)
                (lower {:object-type :mangel :full body :panel body}))))
  (testing "a :status the old grammar took is now an unknown key"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ent/parse-entv {:object-type :mangel :status :draft :full body}
                                 "object-views/mangel.entv")))))

(deftest the-projection-round-trips-every-shape
  (testing "what `emit-view-file` writes, `entv->schema` reads back unchanged —
            the property the old pair lacked, which is why a re-install was a
            silent write"
    (doseq [file [{:object-type :mangel :full body :panel body}
                  {:object-type :mangel :full body}
                  {:object-type :mangel :auto-publish true :full body :panel body}
                  {:object-type :mangel :project :linie :panel body}]]
      (is (= file (emit (lower file)))
          (str "round-trip: " (pr-str (dissoc file :full :panel)))))))

(defn- actions-of [file factor]
  (get-in (lower file) [(keyword (str "luftfahrzeug-view-" (name factor))) :layout :actions]))

(def ^:private mit-knopf
  {:object-type :luftfahrzeug
   :full {:kind :rows
          :items [{:widget :action-button :label "Freigeben" :action :luftfahrzeug-freigeben
                   :params {:luftfahrzeug {:item :id}}}
                  {:kind :section :title "AOG"
                   :body {:widget :action-button :label "Sperren"
                          :action :luftfahrzeug-sperren}}]}})

(deftest ein-gemalter-knopf-autorisiert-sich-selbst
  (testing "every :action-button in the tree, however deep, reaches the set the
            server checks — the button IS the declaration"
    (is (= [{:action :luftfahrzeug-freigeben} {:action :luftfahrzeug-sperren}]
           (actions-of mit-knopf :full))))

  (testing "a layout that names no action gains no key — an empty whitelist is
            not the same statement as no whitelist"
    (is (nil? (actions-of {:object-type :luftfahrzeug :full {:kind :rows :items []}} :full))))

  (testing "the :comments widget writes back too, so it counts"
    (is (= [{:action :kommentieren}]
           (actions-of {:object-type :luftfahrzeug
                        :full {:kind :rows
                               :items [{:widget :comments :action :kommentieren}]}}
                       :full))))

  (testing "a name that is only known at run time cannot be derived, and is skipped
            rather than guessed"
    (is (nil? (actions-of {:object-type :luftfahrzeug
                           :full {:kind :rows
                                  :items [{:widget :action-button :action {:var :welche}}]}}
                          :full)))))

(deftest die-ableitung-ueberschreibt-keine-eigene-angabe
  (testing "a declared entry survives verbatim — its :parameters narrowing is the
            author's, and re-deriving it would widen the action's surface"
    (let [f (assoc-in mit-knopf [:full :actions]
                      [{:action :luftfahrzeug-freigeben :parameters [:luftfahrzeug]}])]
      (is (= [{:action :luftfahrzeug-freigeben :parameters [:luftfahrzeug]}
              {:action :luftfahrzeug-sperren}]
             (actions-of f :full))))))

(deftest die-projektion-schreibt-den-abgeleiteten-schluessel-nicht-zurueck
  (let [file  {:object-type :mangel
               :full {:kind :rows
                      :items [{:widget :action-button :label "Beheben" :action :mangel-beheben
                               :params {:mangel {:item :id}}}]}}
        apps  (lower file)]
    (testing "setup: the lowering DOES derive it — that is what makes the button live"
      (is (= [{:action :mangel-beheben}]
             (get-in apps [:mangel-view-full :layout :actions]))))

    (let [back (emit apps)]
      (testing "and the file written back does NOT carry it"
        (is (nil? (get-in back [:full :actions]))))

      (testing "so the projection's own output goes back through the real door"
        (is (map? (ent/parse-entv back "object-views/mangel.entv"))))

      (testing "and re-importing it derives the same set — nothing was lost"
        (is (= [{:action :mangel-beheben}]
               (get-in (:applications (ent-view/entv->schema back))
                       [:mangel-view-full :layout :actions])))))))
