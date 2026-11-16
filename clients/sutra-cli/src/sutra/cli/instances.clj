(ns sutra.cli.instances
  (:require [clojure.string :as str]
            [sutra.cli.auth :as auth]
            [sutra.cli.errors :refer [ok?]]
            [sutra.cli.out :as out :refer [mark paint]]
            [sutra.cli.site :as site]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- at
  [url f]
  (let [old (tx/current-base)]
    (tx/set-base! url)
    (try (f) (finally (tx/set-base! old)))))

(defn- bounded
  [ms f]
  (deref (future (try (f) (catch Exception _ ::unreachable))) ms ::timeout))

(defn- creds-path [{:keys [url creds]}]
  (or creds (auth/creds-path-for url)))

(defn- state
  [{:keys [url] :as inst}]
  (let [stored (get-in (auth/load-creds (creds-path inst)) [:users :admin])]
    (if-not (or (:token stored) (:password stored))
      {:user nil :state "no credentials"}
      (let [r (bounded 5000 #(at url (fn [] (binding [tx/*auth-token* (:token stored)]
                                              (api/get! :me)))))]
        {:user  (:username stored)
         :state (cond (#{::timeout ::unreachable} r) "unreachable"
                      (and (ok? r) (:username (:body r))) "signed in"
                      (:password stored) "password kept (signs in at the next call)"
                      :else "session ended")}))))

(defn- list!
  []
  (let [is (site/instances)]
    (if (empty? is)
      (out/fail! (mark :yellow "⏸" "this site names no instance")
                 (str "    " (paint :bold "bin/sutra instances add <name> <url>")))
      (do (doseq [[nm inst] is
                  :let [{:keys [user state]} (state inst)]]
            (println (str "  " (paint :bold nm) "  " (:url inst)
                          "  " (paint :dim (creds-path inst))
                          "  " (or user "—") "  " state)))
          (println (paint :dim (str "  a call goes to one with a leading --instance <name>;"
                                    " prod is the default")))
          0))))

(defn- ask-line [label]
  (when-let [con (System/console)]
    (some-> (.readLine con "%s" (object-array [(str "  " label ": ")])) str/trim not-empty)))

(defn- login!
  [nm]
  (if-let [{:keys [url] :as inst} (site/instance nm)]
    (let [user (or (ask-line "user (admin)") "admin")
          pw   (out/read-secret (str user "@" nm))]
      (if (str/blank? (str pw))
        (out/fail! (mark :red "✗" "no password read — instances login needs a terminal"))
        (at url
            (fn []
              (let [r (api/post! :login {:username user :password pw})]
                (if (ok? r)
                  (let [path (creds-path inst)]
                    (auth/save-creds! path {:users {:admin {:username user :password pw
                                                            :token (:token (:body r))}}})
                    (println (mark :green "✓" (str user " at " nm " · kept in " path)))
                    0)
                  (out/fail! (mark :red "✗" (str "login at " nm " failed: HTTP "
                                                 (:status r))))))))))
    (out/fail! (mark :red "✗" (str "no instance named " nm)))))

(defn main!
  [[sub nm url]]
  (case sub
    nil      (list!)
    "list"   (list!)
    "add"    (if (and nm url (re-find #"^https?://" url))
               (do (println (mark :green "✓" (str nm " → " url " in "
                                                 (site/set-instance! nm url))))
                   0)
               (out/fail! "  instances add <name> <http(s)://url>"))
    "remove" (if nm
               (do (println (mark :green "✓" (str nm " forgotten in "
                                                 (site/set-instance! nm nil))))
                   0)
               (out/fail! "  instances remove <name>"))
    "login"  (if nm (login! nm) (out/fail! "  instances login <name>"))
    (out/fail! "  instances [list | add <name> <url> | remove <name> | login <name>]")))
