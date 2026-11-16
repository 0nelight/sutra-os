(ns sutra-ui.release
  (:require [clojure.java.io :as io]
            [shadow.build :as build]
            [shadow.build.closure :as closure]
            [shadow.cljs.devtools.api :as api]
            [shadow.cljs.devtools.config :as config]
            [shadow.cljs.devtools.server.util :as util])
  (:import (java.security MessageDigest)))

(defn- fingerprint
  [state cfg]
  (let [md  (MessageDigest/getInstance "SHA-256")
        add (fn [x] (.update md (.getBytes (str x "\n") "UTF-8")))]
    (add (System/getProperty "java.class.path"))
    (add (pr-str cfg))
    (add (pr-str (map #(select-keys % [:module-id :sources :depends-on])
                      (:build-modules state))))
    (add (.getCode (closure/generate-externs state)))
    (doseq [rc (:build-sources state)
            :let [{:keys [js properties]} (get-in state [:output rc])]]
      (add (pr-str rc))
      (add js)
      (add (pr-str (sort properties))))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(defn- rm-rf [d]
  (doseq [^java.io.File f (reverse (file-seq (io/file d)))] (.delete f)))

(defn- copy-tree [from to]
  (let [base (.toPath (io/file from))]
    (doseq [^java.io.File f (file-seq (io/file from))
            :when (.isFile f)
            :let [dst (io/file to (str (.relativize base (.toPath f))))]]
      (io/make-parents dst)
      (io/copy f dst))))

(defn main [build-id]
  (api/with-runtime
    (let [cfg   (config/get-build! (keyword build-id))
          out   (:output-dir cfg)
          keep  (io/file ".shadow-cljs" "release-reuse" (name build-id))
          state (-> (util/new-build cfg :release {})
                    (build/configure :release cfg {})
                    (build/compile))
          fp    (fingerprint state cfg)
          fpf   (io/file keep "fingerprint")]
      (if (and (.exists fpf) (= fp (slurp fpf)))
        (do (println "release: compiled output unchanged — reusing the last optimized build"
                     (subs fp 0 12))
            (copy-tree (io/file keep "out") out)
            (build/execute-hooks state :flush))
        (do (println "release: compiled output changed — running the Closure pass" (subs fp 0 12))
            (-> state (build/optimize) (build/flush) (api/build-finish cfg))
            (rm-rf keep)
            (copy-tree out (io/file keep "out"))
            (spit fpf fp)))
      :done)))
