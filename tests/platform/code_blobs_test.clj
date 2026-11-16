(ns ^:unit platform.code-blobs-test
  (:require [sutra.blobs.api :as blobs]
            [sutra.blobs.mem :as blobs-mem]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.repos.code-blobs :as cb]))

(def ^:private ^:dynamic *b* nil)

(use-fixtures :each (fn [t] (binding [*b* (blobs-mem/blobs)] (t))))

(def ^:private big (apply str (repeat 40 "print('a long enough body')\n")))
(def ^:private small "print(1)")

(defn- world [& {:keys [code source]}]
  {:transforms   {:t {:name "t" :language :python :code (or code big)}}
   :applications {:a {:name "a" :layout [:col] :source (or source big)}}
   :object-types {:o {:symbol "O"}}})

(deftest lift-then-lower-is-the-identity
  (let [w (world)
        [lifted shas] (cb/lift *b* w)]
    (testing "the body is gone from the map and an address is in its place"
      (is (map? (get-in lifted [:transforms :t :code])))
      (is (not= big (get-in lifted [:transforms :t :code])))
      (is (= 1 (count shas)) "both fields hold the same bytes, so one address"))
    (testing "everything that is not source is untouched"
      (is (= (:object-types w) (:object-types lifted)))
      (is (= [:col] (get-in lifted [:applications :a :layout]))))
    (testing "lowering returns exactly what was lifted"
      (is (= w (cb/lower *b* lifted))))))

(deftest small-bodies-stay-inline
  (let [[lifted shas] (cb/lift *b* (world :code small :source small))]
    (is (= small (get-in lifted [:transforms :t :code]))
        "a one-liner costs more as a CAS file than as a string")
    (is (empty? shas))))

(deftest lift-is-idempotent-and-still-reports-its-addresses
  (let [[once shas1] (cb/lift *b* (world))
        [twice shas2] (cb/lift *b* once)]
    (is (= once twice))
    (is (= shas1 shas2)
        "a re-write of an already-lifted map must still hold its blobs against the GC")))

(deftest function-releases-are-lifted-too
  (let [w {:functions {:f {:source big
                           :releases {"1.0.0" {:source big :param-types {}}
                                      "1.1.0" {:source (str big "x") :param-types {}}}}}}
        [lifted shas] (cb/lift *b* w)]
    (is (= 2 (count shas)) "two distinct bodies across live source and two releases")
    (is (not= big (get-in lifted [:functions :f :releases "1.0.0" :source])))
    (is (= w (cb/lower *b* lifted)))))

(deftest a-lost-body-raises-rather-than-reading-as-empty
  (let [[lifted shas] (cb/lift *b* (world :code (str big "unique-to-this-test")
                                      :source small))]
    (blobs/delete! *b* (first shas))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not in the CAS"
                          (cb/lower *b* lifted))
        "an empty body would run; that is the silent version of the same loss")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transforms \"t\" :code"
                          (cb/lower *b* lifted))
        "and it names the entity, because a bare sha is not something to act on")))

(deftest identical-bodies-share-one-address
  (let [[_ shas] (cb/lift *b* {:transforms {:a {:code big} :b {:code big}}})]
    (is (= 1 (count shas))
        "this is the dedup the history table never had — N copies, one object")))
