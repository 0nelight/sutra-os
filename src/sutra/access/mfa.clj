(ns sutra.access.mfa
  (:require [clojure.string :as str]
            [sutra.access.totp :as totp]
            [sutra.foundation.env :as env]
            [sutra.catalog.grants :as grants]
            [sutra.store.control :as ctl]
            [sutra.store.storage.instance-config :as ic])
  (:import (java.util Base64)))

(set! *warn-on-reflection* true)

(def issuer "Sutra")

(def ^:private key-cfg "mfa_seed_key")

(defn- seed-key
  ^bytes [store]
  (if-let [k (some-> (env/getenv "SUTRA_MFA_SEED_KEY") str/trim not-empty)]
    (.decode (Base64/getDecoder) ^String k)
    (let [^String seed (or (ic/get-cfg store key-cfg)
                           (ic/swap-cfg! store key-cfg #(or (not-empty %) (totp/new-seed))))]
      (.getBytes seed "UTF-8"))))

(defn required-for?
  ([username] (required-for? (env/getenv "SUTRA_MFA_REQUIRED") username))
  ([setting username]
   (case (some-> setting str str/trim str/lower-case)
     ("all" "true" "1") true
     "admin"            (= grants/admin-username username)
     false)))

(defn- row [store username] (ctl/get store :principals {:username username}))

(defn enrolled?
  [p]
  (boolean (or (:mfa p) (seq (:mfa-seed p)))))

(defn- updating
  [username f]
  (fn [store at]
    (if (ctl/get store :principals {:username username})
      (ctl/swapping store :principals {:username username} f at)
      {:result nil})))

(defn- plan! [store planner]
  (ctl/commit! store [[:plan :principals planner]]))

(defn begin!
  [store username]
  (let [seed (totp/new-seed)]
    (plan! store (updating username #(assoc % :mfa-pending (totp/seal (seed-key store) seed))))
    {:seed seed :uri (totp/uri seed username issuer)}))

(defn confirm!
  [store username code now-ms]
  (let [p    (row store username)
        seed (some->> (:mfa-pending p) (totp/unseal (seed-key store)))
        st   (when seed (totp/verify seed code now-ms nil))]
    (if st
      (do (plan! store (updating username #(assoc % :mfa-seed (:mfa-pending %)
                                                  :mfa-pending nil :mfa-last-step st)))
          true)
      false)))

(defn check!
  [store username code now-ms]
  (let [p    (row store username)
        seed (some->> (:mfa-seed p) (totp/unseal (seed-key store)))
        st   (when seed (totp/verify seed code now-ms (:mfa-last-step p)))]
    (if st
      (let [spent (atom false)]
        (plan! store (fn [s at]
                       (let [cur (ctl/get s :principals {:username username})]
                         (if (< (long (or (:mfa-last-step cur) -1)) st)
                           (do (reset! spent true)
                               (ctl/swapping s :principals {:username username}
                                             #(assoc % :mfa-last-step st) at))
                           {:result nil}))))
        @spent)
      false)))

(defn disable!
  [store username]
  (plan! store (updating username #(assoc % :mfa-seed nil :mfa-pending nil
                                           :mfa-last-step nil)))
  nil)
