(ns sutra.port.certify
  (:require [sutra.blobs.api :as blobs]
            [sutra.blobs.conformance :as blobs-conf]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.certify :as c :refer [rnd]]
            [sutra.coord.api :as coord]
            [sutra.coord.conformance :as coord-conf]
            [sutra.coord.mem :as coord-mem]
            [sutra.datasets.api :as datasets]
            [sutra.datasets.conformance :as datasets-conf]
            [sutra.streams.api :as streams]
            [sutra.streams.conformance :as streams-conf]
            [sutra.streams.mem :as streams-mem]))

(set! *warn-on-reflection* true)

(defn- program [duty shape {:keys [steps seed]} spec x]
  (c/program (merge {:duty duty :shape shape :steps (or steps 150) :seed (or seed 1)} spec) x))

(def ^:private pool
  (vec (for [i (range 6)] (byte-array (map #(unchecked-byte (* (inc i) %)) (range (* i 37)))))))

(def ^:private pool-hashes (set (map #(blobs/address-of %) pool)))

(defn- blobs-run [_ b _ [k i]]
  (let [h (blobs/address-of (pool i))]
    (case k
      :put    (blobs/put! b (pool i))
      :open   (with-open [^java.io.InputStream in
                          (or (blobs/open b h) (java.io.ByteArrayInputStream. (byte-array 0)))]
                (when (blobs/exists? b h) (vec (.readAllBytes in))))
      :exists (blobs/exists? b h)
      :size   (blobs/size-of b h)
      :landed (some? (blobs/stored-at b h))
      :delete (blobs/delete! b h nil)
      :hashes (set (filter pool-hashes (blobs/hashes b))))))

(defn- blobs-gen [r _]
  [([:put :put :open :exists :size :landed :delete :hashes] (rnd r 8)) (rnd r (count pool))])

(defn- blobs-check [{:keys [make] :as driver}]
  (let [d (make) m (blobs-mem/blobs)]
    (doseq [h pool-hashes] (blobs/delete! d h nil))
    (program :duty/blobs :driver/blobs-command driver {:gen blobs-gen :run blobs-run}
             {:driver d :reference m})))

(defn- coord-run [side c {:keys [run] :as state} [k n o]]
  (let [name (str run "/" n) mine (get-in state [side :held n])]
    (case k
      :acquire (coord/acquire! c {:name name :owner o :ttl-ms 600000 :wait-ms 0})
      :renew   (when mine (coord/renew! c mine 600000))
      :release (when mine (coord/release! c mine))
      :holder  (coord/holder c name)
      :revoke  (coord/revoke! c name))))

(defn- epoch [state side [k n o] answer]
  (case k
    :acquire (let [mine (get-in state [side :held n])]
               (when-not (and mine (= o (:owner mine)) (= (:epoch mine) (:epoch answer)))
                 (:epoch answer)))
    :revoke  answer
    nil))

(defn- coord-advance [state [k n :as cmd] answers]
  (reduce (fn [s [side a]]
            (cond-> s
              (some? (epoch state side cmd a)) (assoc-in [side :epochs n] (epoch state side cmd a))
              (and (= :acquire k) a)           (assoc-in [side :held n] a)
              (#{:release :revoke} k)          (update-in [side :held] dissoc n)))
          state (map vector c/sides answers)))

(defn- shown [[k] a]
  (case k
    (:acquire :renew :holder) (some-> a (select-keys [:name :owner]))
    :revoke                   (some? a)
    a))

(defn- coord-judge [state [_ n :as cmd] a b]
  (or (some (fn [[side x]]
              (let [e (epoch state side cmd x) prior (get-in state [side :epochs n])]
                (when (and e prior (<= e prior))
                  {:duty :duty/coord-epoch :msg "an epoch did not rise"
                   :data {:side side :name n :epoch e :before prior}})))
            (map vector c/sides [a b]))
      (when (not= (shown cmd a) (shown cmd b))
        {:duty :duty/coord :msg "the driver answered otherwise than the reference"
         :data {:driver (shown cmd a) :reference (shown cmd b)}})))

(defn- coord-gen [r _]
  [([:acquire :acquire :renew :release :holder :revoke] (rnd r 6)) (str "n" (rnd r 3))
   (["a" "b"] (rnd r 2))])

(defn- coord-check [{:keys [make run] :as driver}]
  (let [d (make)]
    (doseq [n ["n0" "n1" "n2"]] (coord/revoke! d (str run "/" n)))
    (program :duty/coord :driver/coord-command driver
             {:init {:run run} :gen coord-gen :run coord-run :advance coord-advance
              :judge coord-judge}
             {:driver d :reference (coord-mem/coordinator)})))

(def ^:private t0 1700000000000)

(defn- streams-run [_ [s ref] _ [k x]]
  (case k
    :append (streams/append! s ref (vec (for [[p i] x]
                                          {:partition p :key (str "k" i) :record {:v i}
                                           :at (java.util.Date. (long (+ t0 (* 1000 i))))})))
    :read   (streams/read s ref x)
    :ends   (streams/ends s ref)
    :stats  (streams/stats s ref)
    :trim   (streams/trim! s ref x)
    :reset  (do (streams/reset! s ref) :reset)))

(defn- streams-shown [_ _ _ [k] a]
  (case k
    :read  (mapv #(update % :at inst-ms) a)
    :stats (update a :last-at #(some-> % inst-ms))
    a))

(defn- streams-gen [r _]
  (case (long (rnd r 7))
    (0 1) [:append (vec (repeatedly (inc (rnd r 4)) #(vector (rnd r 3) (rnd r 50))))]
    2     [:read (cond-> {}
                   (pos? (rnd r 2)) (assoc :from {0 (rnd r 4) 1 (rnd r 4)})
                   (pos? (rnd r 2)) (assoc :partition (rnd r 3))
                   (pos? (rnd r 2)) (assoc :limit (inc (rnd r 5))))]
    3     [:ends]
    4     [:stats]
    5     [:trim (cond-> {:below {0 (rnd r 5) 1 (rnd r 5) 2 (rnd r 5)}}
                   (pos? (rnd r 2))
                   (assoc :before (java.util.Date. (long (+ t0 (* 1000 (rnd r 50)))))))]
    6     (if (zero? (rnd r 4)) [:reset] [:ends])))

(defn- streams-check [{:keys [make run] :as driver}]
  (let [ref {:branch "main" :stream (str "cert_" run)}
        d (make)]
    (streams/drop! d ref)
    (try (program :duty/streams :driver/streams-command driver
                  {:gen streams-gen :run streams-run :observe streams-shown}
                  {:driver [d ref] :reference [(streams-mem/streams) ref]})
         (finally (streams/drop! d ref)))))

(defn- suite [duty f] (c/suite duty "the driver broke the port's conformance suite" f))

(def checks
  {:blobs    {:program blobs-check   :suite #(suite :duty/blobs (fn [] (blobs-conf/check-all %)))}
   :coord    {:program coord-check   :suite #(suite :duty/coord (fn [] (coord-conf/check-all %)))}
   :streams  {:program streams-check :suite #(suite :duty/streams
                                                    (fn [] (streams-conf/check-all %)))}
   :datasets {:suite #(suite :duty/datasets
                             (fn [] (datasets-conf/check-all %)
                               (when (:query (datasets/profile (%)))
                                 (datasets-conf/check-query %))))}})

(def proves
  {:blobs    {:certify/program #{:duty/blobs :duty/failures} :certify/suite #{:duty/blobs}}
   :coord    {:certify/program #{:duty/coord :duty/coord-epoch :duty/failures}
              :certify/suite   #{:duty/coord}}
   :streams  {:certify/program #{:duty/streams :duty/failures} :certify/suite #{:duty/streams}}
   :datasets {:certify/suite #{:duty/datasets}}})

(defn certify
  [port {:keys [make suite-make] :as driver}]
  (let [{:keys [program suite]} (checks port)]
    (c/certification (cond-> {:certify/suite (fn [_] (suite (or suite-make make)))}
                       program (assoc :certify/program
                                      #(program (merge {:run (str "c" (System/nanoTime))} %))))
                     driver)))
