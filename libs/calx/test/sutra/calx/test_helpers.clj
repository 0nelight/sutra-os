(ns sutra.calx.test-helpers
  (:require [sutra.calx.core :as c]
            [sutra.calx.value :as v]))

(defn calc
  ([formula] (calc (c/workbook) formula))
  ([wb formula] (c/calculate wb formula)))

(defn err [t] (v/err t))

(defn approx=
  ([a b] (approx= a b 1e-9))
  ([a b eps]
   (and (number? a) (number? b)
        (<= (Math/abs (- (double a) (double b)))
            (* eps (max 1.0 (Math/abs (double a)) (Math/abs (double b))))))))
