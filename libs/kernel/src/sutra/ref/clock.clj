(ns sutra.ref.clock)

(set! *warn-on-reflection* true)

(def system
  {:fresh-uuid #(java.util.UUID/randomUUID)
   :now        #(java.time.Instant/now)
   :zone       #(java.time.ZoneId/systemDefault)})
