(ns ^:unit platform.frontend-apps-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]))

(def ^:private src "clients/frontend/src/sutra_ui/")

(def ^:private allowed
  {"shell/builder.cljs" #{"sutra-ui.apps.all"}
   "core.cljs"          #{"sutra-ui.apps.assistant.core" "sutra-ui.apps.ui-kit.feature"}})

(def ^:private kernel
  #{"feature.cljs" "state.cljs" "routing.cljs"})

(defn- sources []
  (for [f (file-seq (io/file src))
        :when (.isFile ^java.io.File f)
        :let [p (subs (.getPath ^java.io.File f) (count src))]
        :when (and (re-find #"\.clj[sc]?$" p)
                   (not (str/ends-with? p "_smoke_test.cljs")))]
    [p (slurp f)]))

(defn- outside-apps []
  (remove (fn [[p]] (str/starts-with? p "apps/")) (sources)))

(defn- unit-of
  [p]
  (second (re-find #"^apps/([^/.]+)" p)))

(defn- form-at
  [^String s i]
  (loop [k i depth 0]
    (let [c (.charAt s k)]
      (cond
        (= c \") (recur (loop [j (inc k)]
                          (case (.charAt s j) \\ (recur (+ j 2)) \" (inc j) (recur (inc j))))
                        depth)
        (= c \\) (recur (+ k 2) depth)
        (#{\( \[ \{} c) (recur (inc k) (inc depth))
        (#{\) \] \}} c) (if (= 1 depth) (subs s i (inc k)) (recur (inc k) (dec depth)))
        :else (recur (inc k) depth)))))

(defn- ui-state-writes
  [text]
  (let [m (re-matcher #"\((?:swap!|reset!) (?:state/)?ui-state\b" text)]
    (loop [acc []]
      (if (.find m) (recur (conj acc (form-at text (.start m)))) acc))))

(deftest shared-code-requires-no-app
  (is (seq (outside-apps)))
  (doseq [[p text] (outside-apps)
          :let [apps (set (map second (re-seq #"\[(sutra-ui\.apps\.[\w.-]+)" text)))
                bad  (remove (get allowed p #{}) apps)]]
    (is (empty? bad) (str p " requires " (vec (sort bad))
                          " — reach the app through sutra-ui.feature instead"))))

(deftest every-app-directory-is-registered
  (let [all (slurp (str src "apps/all.cljs"))]
    (doseq [d (.listFiles (io/file src "apps"))
            :when (and (.isDirectory ^java.io.File d)
                       (.exists (io/file d "feature.cljs")))
            :let [n (str "sutra-ui.apps." (str/replace (.getName ^java.io.File d) "_" "-")
                         ".feature")]]
      (is (str/includes? all n) (str n " is missing in apps/all.cljs")))))

(deftest only-the-kernel-moves-between-screens
  (is (empty? (for [[p text] (sources)
                    :when (not (kernel p))
                    form (ui-state-writes text)
                    :when (re-find #"\s:nav\s" form)]
                [p form]))
      "writes :nav itself — `(state/go! <screen> {<params>})` instead"))

(deftest an-app-writes-only-through-its-slice-and-kernel-verbs
  (is (empty? (for [[p text] (sources)
                    :when (unit-of p)
                    form (ui-state-writes text)]
                [p form]))
      "an app writes its own slice (`state/slice`) or calls a kernel verb (`state/fail!`,
       `state/show-panel!`, `state/go!` …) — never ui-state itself"))

(deftest shared-code-names-no-screen
  (is (empty? (for [[p text] (outside-apps)
                    call (re-seq #"\((?:state/)?(?:go|enter)! :[\w-]+" text)]
                [p call]))
      "shared code says what it wants (`state/open-resource!`, `feature/command!`), never
       which screen does it"))

(deftest only-the-kernel-spells-a-slice-path
  (doseq [[p text] (sources)
          :when (not (kernel p))]
    (is (not (re-find #":slices\b" text))
        (str p " reaches into [:slices …] — `state/slice` of its own app instead"))))

(deftest an-app-holds-only-its-own-slice
  (let [units (group-by (comp unit-of first) (filter (comp unit-of first) (sources)))
        owns  (fn [texts]
                (into #{}
                      (mapcat #(concat (re-seq #"register-app!\s+\{:id (:[\w-]+)" %)
                                       (re-seq #"\{:id :[\w-]+ :app (:[\w-]+)" %)))
                      texts))]
    (doseq [[unit files] units
            :let [mine (set (map second (owns (map second files))))]
            [p text] files
            [_ id] (re-seq #"\((?:state/slice|feature/slice \S+)\s+(:[\w-]+)" text)]
      (is (mine id) (str p " holds the slice of " id " — apps/" unit " registers "
                         (vec (sort mine)))))
    (doseq [[p text] (outside-apps)
            :when (not (kernel p))]
      (is (not (re-find #"\((?:state/slice|feature/slice \S+)\s+:" text))
          (str p " names an app's slice — shared code reaches an app through its screen")))))

(deftest an-app-requires-no-other-app
  (let [units (group-by (comp unit-of first) (filter (comp unit-of first) (sources)))
        ids   (fn [files]
                (into #{} (mapcat #(map second
                                        (concat (re-seq #"register-app!\s+\{:id (:[\w-]+)" %)
                                                (re-seq #"\{:id :[\w-]+ :app (:[\w-]+)" %))))
                      (map second files)))
        app   (into {} (for [[u fs] units] [u (ids fs)]))]
    (doseq [[unit files] units
            :when (not= "all" unit)
            [p text] files
            [_ dir] (re-seq #"\[sutra-ui\.apps\.([\w-]+)[\w.-]*" text)
            :let [other (str/replace dir "-" "_")]
            :when (not= other unit)]
      (is (seq (clojure.set/intersection (app unit #{}) (app other #{})))
          (str p " requires apps/" other " — another app; reach it through sutra-ui.feature"
               " (a command, a slot, `state/go!` with its :params)")))))
