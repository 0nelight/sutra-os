(ns sutra.coord.api
  (:require [sutra.port :as port]))

(set! *warn-on-reflection* true)

(def lease-schema
  [:map {:closed true} [:name :string] [:owner :string] [:epoch :int] [:until inst?]])

(def acquire-schema
  [:map {:closed true} [:name :string] [:owner :string] [:ttl-ms pos-int?]
   [:wait-ms {:optional true} nat-int?]])

(defprotocol Coordinator
  (-acquire! [c req])
  (-renew! [c lease ttl-ms])
  (-release! [c lease])
  (-holder [c name])
  (-revoke! [c name]))

(defn- check! [schema x what] (port/asked! :coord schema x what))

(def ^:private lease-answer [:maybe lease-schema])

(defn- of? [{:keys [name owner]} lease]
  (or (nil? lease) (and (= name (:name lease)) (= owner (:owner lease)))))

(defn acquire! [c req]
  (port/answered! :coord lease-answer (-acquire! c (check! acquire-schema req :request)) :acquire
                  (partial of? req)))
(defn renew! [c lease ttl-ms]
  (port/answered! :coord lease-answer
                  (-renew! c (check! lease-schema lease :lease) (check! pos-int? ttl-ms :ttl-ms))
                  :renew
                  #(or (nil? %) (and (of? lease %) (= (:epoch lease) (:epoch %))))))
(defn release! [c lease]
  (port/answered! :coord boolean? (-release! c (check! lease-schema lease :lease)) :release))
(defn holder [c name]
  (port/answered! :coord lease-answer (-holder c (check! :string name :name)) :holder
                  #(or (nil? %) (= name (:name %)))))
(defn revoke! [c name]
  (port/answered! :coord :int (-revoke! c (check! :string name :name)) :revoke))
