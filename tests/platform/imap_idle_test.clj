(ns ^:unit platform.imap-idle-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.sources.imap-idle :as idle]
            [sutra.sources.tls :as tls])
  (:import (java.net ServerSocket Socket)))

(deftest wire
  (testing "untagged responses of SELECT and IDLE"
    (is (= {:exists 23} (idle/untagged "* 23 EXISTS")))
    (is (= {:recent 1} (idle/untagged "* 1 RECENT")))
    (is (= {:expunge 4} (idle/untagged "* 4 EXPUNGE")))
    (is (= {:uidnext 4392} (idle/untagged "* OK [UIDNEXT 4392] Predicted next UID")))
    (is (= {:uidvalidity 7} (idle/untagged "* OK [UIDVALIDITY 7] UIDs valid")))
    (is (nil? (idle/untagged "* FLAGS (\\Answered \\Seen)")))
    (is (nil? (idle/untagged "* 3 FETCH (FLAGS (\\Seen))"))))
  (testing "tagged completion"
    (is (= :ok (idle/tagged "l2" "l2 OK [READ-WRITE] Select completed")))
    (is (= :no (idle/tagged "l1" "l1 NO [AUTHENTICATIONFAILED] Authentication failed")))
    (is (nil? (idle/tagged "l2" "l3 OK done"))))
  (testing "quoting and folder names"
    (is (= "\"a\\\"b\\\\c\"" (idle/quoted "a\"b\\c")))
    (is (thrown? clojure.lang.ExceptionInfo (idle/quoted "a\r\nb")))
    (is (= "shared/rechnung" (idle/mailbox-name "shared/rechnung")))
    (is (= "Entw&APw-rfe" (idle/mailbox-name "Entwürfe")))
    (is (= "A&-B" (idle/mailbox-name "A&B"))))
  (testing "a TLS pin is read with or without its prefix and colons"
    (is (= "abcd" (tls/pin-of "sha256:AB:CD")))
    (is (nil? (tls/pin-of " ")))))

(deftest arrivals
  (let [s (reduce idle/absorb {} ["* 5 EXISTS" "* OK [UIDNEXT 40]" "* OK [UIDVALIDITY 1]"])]
    (is (= {:exists 5 :uidnext 40 :uidvalidity 1} s))
    (testing "during IDLE"
      (is (idle/arrival? s "* 6 EXISTS"))
      (is (idle/arrival? s "* 1 RECENT"))
      (is (not (idle/arrival? s "* 5 EXISTS")))
      (is (not (idle/arrival? s "* 2 EXPUNGE")))
      (is (= 4 (:exists (idle/absorb s "* 2 EXPUNGE")))))
    (testing "between windows: only a higher UIDNEXT or a new UIDVALIDITY is mail"
      (is (not (idle/moved? nil s)))
      (is (not (idle/moved? s (assoc s :exists 3))))
      (is (idle/moved? s (assoc s :uidnext 41)))
      (is (idle/moved? s (assoc s :uidvalidity 2))))))

(deftest folders
  (is (= ["shared/rechnung"] (idle/folders-of {} ["shared/rechnung"])))
  (is (= ["shared/rechnung" "shared/frank"]
         (idle/folders-of {:include "shared/rechnung, shared/frank"} nil)))
  (is (= ["INBOX"] (idle/folders-of {} []))))

(defn- fake-imap
  [{:keys [uidnext idle-says]}]
  (let [srv  (ServerSocket. 0)
        seen (atom [])]
    (future
      (with-open [^Socket c (.accept srv)
                  in  (io/reader (.getInputStream c))
                  out (io/writer (.getOutputStream c))]
        (let [say! (fn [& ls] (doseq [l ls] (.write out (str l "\r\n"))) (.flush out))]
          (say! "* OK IMAP4rev1 ready")
          (loop []
            (when-let [line (.readLine in)]
              (swap! seen conj (str/replace line #"LOGIN .*" "LOGIN ***"))
              (let [[tag cmd] (str/split line #" " 3)]
                (case (some-> cmd str/upper-case)
                  "LOGIN"  (say! (str tag " OK logged in"))
                  "SELECT" (say! "* 5 EXISTS" "* OK [UIDVALIDITY 1] ok"
                                 (str "* OK [UIDNEXT " uidnext "] ok") (str tag " OK selected"))
                  "IDLE"   (do (say! "+ idling")
                               (when idle-says (Thread/sleep 100) (say! idle-says))
                               (when (= "DONE" (.readLine in))
                                 (swap! seen conj "DONE")
                                 (say! (str tag " OK idle done"))))
                  "NOOP"   (say! "* 6 EXISTS" (str tag " OK"))
                  "LOGOUT" (say! "* BYE" (str tag " OK"))
                  (say! (str tag " BAD unknown")))
                (when-not (= "LOGOUT" (some-> cmd str/upper-case)) (recur)))))))
      (.close srv))
    [(.getLocalPort srv) seen]))

(defn- spec [port]
  {:host "127.0.0.1" :port port :tls false :username "u" :password "p\"w"})

(deftest a-window-over-the-wire
  (testing "an EXISTS during IDLE ends the window as a change"
    (let [[port seen] (fake-imap {:uidnext 40 :idle-says "* 6 EXISTS"})
          r (idle/await-change! (spec port) {:folders ["shared/rechnung"] :window-ms 5000})]
      (is (:changed r))
      (is (= 6 (get-in r [:state "shared/rechnung" :exists])))
      (is (some #{"DONE"} @seen))
      (is (some #(str/includes? % "SELECT \"shared/rechnung\"") @seen))))
  (testing "a quiet window ends without a change and keeps the folder state"
    (let [[port _] (fake-imap {:uidnext 40})
          r (idle/await-change! (spec port) {:folders ["INBOX"] :window-ms 1000})]
      (is (not (:changed r)))
      (is (= 40 (get-in r [:state "INBOX" :uidnext])))))
  (testing "mail that came between two windows shows at SELECT, without IDLE"
    (let [[port seen] (fake-imap {:uidnext 41})
          r (idle/await-change! (spec port)
                                {:folders ["INBOX"] :window-ms 5000
                                 :state {"INBOX" {:uidvalidity 1 :uidnext 40 :exists 5}}})]
      (is (:changed r))
      (is (not-any? #(str/includes? % "IDLE") @seen)))))
