(ns sutra.notifications.api
  (:require [sutra.notifications.audience :as audience]
            [sutra.notifications.content :as content]
            [sutra.notifications.inbox :as inbox]
            [sutra.notifications.prefs :as prefs]))

(set! *warn-on-reflection* true)

(defn recipients [opts values] (inbox/recipients opts values))

(def render inbox/render)

(defn send! [store branch usernames notification] (inbox/send! store branch usernames notification))

(defn admitted [store branch names n] (inbox/admitted store branch names n))

(defn plan-each [base deliveries settings] (inbox/plan-each base deliveries settings))

(defn deliver! [store p attachments] (inbox/deliver! store p attachments))

(defn settings-of [store usernames] (prefs/of-users store usernames))

(defn usernames [store names] (audience/usernames store names))

(defn names [xs] (audience/names xs))

(def max-recipients audience/max-recipients)

(defn link-url [spec values] (content/link-url spec values))
