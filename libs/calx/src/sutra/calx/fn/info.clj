(ns sutra.calx.fn.info
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.address :as addr]
            [sutra.calx.eval :as ev]
            [sutra.calx.fn.util :as u]))

(set! *warn-on-reflection* true)

(defn- predicate
  [pred]
  {:arity [1 1] :lift :all :errors :pass :impl (comp boolean pred)})

(defn- parity [pred]
  {:arity [1 1] :lift :all
   :impl (fn [x]
           (if (boolean? x)
             v/ERR-VALUE
             (u/check [n (u/num x)] (pred (long (Math/abs (double n)))))))})

(def ^:private error-codes
  {:null 1.0 :div0 2.0 :value 3.0 :ref 4.0 :name 5.0 :num 6.0 :na 7.0 :spill 9.0 :calc 14.0})

(defn- type-code [x]
  (cond
    (or (nil? x) (number? x)) 1.0
    (string? x) 2.0
    (boolean? x) 4.0
    (error? x) 16.0
    (v/array? x) 64.0
    :else 128.0))

(defn- n* [x]
  (cond (number? x) (double x)
        (boolean? x) (if x 1.0 0.0)
        :else 0.0))

(defn- formula-text [ctx ref]
  ((:formula-at ctx (constantly nil)) (:sheet ref) (:r1 ref) (:c1 ref)))

(defn- isformula [ctx x]
  (cond
    (error? x) x
    (not (v/ref? x)) v/ERR-VALUE
    (v/single-cell? x) (some? (formula-text ctx x))
    :else (let [{:keys [sheet r1 c1 r2 c2]} (ev/clip-ref ctx x)]
            (v/make-array2 (inc (- r2 r1)) (inc (- c2 c1))
                           (fn [r c]
                             (some? (formula-text ctx (v/->Ref sheet (+ r1 r) (+ c1 c) 0 0))))))))

(defn- formulatext [ctx x]
  (cond (error? x) x
        (v/ref? x) (or (formula-text ctx x) v/ERR-NA)
        :else v/ERR-NA))

(defn- sheet-number [ctx id]
  (double ((:sheet-index ctx) id)))

(defn- sheet* [ctx & [x]]
  (let [x (if (v/omitted? x) nil x)]
    (cond
      (nil? x) (sheet-number ctx (:sheet ctx))
      (v/ref? x) (sheet-number ctx (:sheet x))
      (error? x) x
      (string? x) (if-let [id ((:sheet-id ctx) x)] (sheet-number ctx id) v/ERR-NA)
      :else v/ERR-VALUE)))

(defn- sheets* [ctx & [x]]
  (let [x (if (v/omitted? x) nil x)]
    (cond
      (nil? x) (double (count ((:sheet-names ctx))))
      (v/ref? x) 1.0
      (error? x) x
      :else v/ERR-VALUE)))

(defn- cell-address [ctx {:keys [sheet r1 c1]}]
  (str (when (not= sheet (:sheet ctx))
         (str (addr/quote-sheet-name ((:sheet-name ctx) sheet)) "!"))
       (addr/cell->a1 r1 c1 true true)))

(defn- cell* [ctx info-type & [reference]]
  (let [info (v/scalarize (ev/deref-value ctx info-type))
        ref (if (or (nil? reference) (v/omitted? reference))
              (v/->Ref (:sheet ctx) (:row ctx) (:col ctx) (:row ctx) (:col ctx))
              reference)]
    (cond
      (error? info) info
      (not (string? info)) v/ERR-VALUE
      (error? ref) ref
      (not (v/ref? ref)) v/ERR-VALUE
      :else
      (let [contents #((:value-at ctx) (:sheet ref) (:r1 ref) (:c1 ref))]
        (case (str/lower-case info)
          "address" (cell-address ctx ref)
          "col" (double (inc (:c1 ref)))
          "row" (double (inc (:r1 ref)))
          "contents" (contents)
          "type" (let [x (contents)] (cond (nil? x) "b" (string? x) "l" :else "v"))
          "filename" ""
          "format" "G"
          "prefix" ""
          "protect" 1.0
          "parentheses" 0.0
          "color" 0.0
          "width" 8.0
          v/ERR-VALUE)))))

(defn- info* [ctx type-text]
  (u/check [t (u/text type-text)]
    (case (str/lower-case t)
      "numfile" (double (count ((:sheet-names ctx))))
      "recalc" "Automatic"
      "system" "pcdos"
      "release" "16.0"
      "osversion" "Windows (64-bit) NT 10.00"
      "directory" ""
      "origin" "$A:$A$1"
      v/ERR-VALUE)))

(def functions
  {"ISBLANK"   (predicate nil?)
   "ISNUMBER"  (predicate number?)
   "ISTEXT"    (predicate string?)
   "ISNONTEXT" (predicate (complement string?))
   "ISLOGICAL" (predicate boolean?)
   "ISERROR"   (predicate error?)
   "ISERR"     (predicate #(and (error? %) (not= v/ERR-NA %)))
   "ISNA"      (predicate #(= v/ERR-NA %))
   "ISEVEN"    (parity even?)
   "ISODD"     (parity odd?)
   "ISREF"     {:arity [1 1] :args :refs :impl (fn [_ x] (v/ref? x))}
   "ISFORMULA" {:arity [1 1] :args :refs :impl isformula}
   "FORMULATEXT" {:arity [1 1] :args :refs :impl formulatext}

   "NA"         {:arity [0 0] :impl (constantly v/ERR-NA)}
   "ERROR.TYPE" {:arity [1 1] :lift :all :errors :pass
                 :impl #(or (when (error? %) (error-codes (:type %))) v/ERR-NA)}
   "TYPE"       {:arity [1 1] :errors :pass :impl type-code}
   "N"          {:arity [1 1] :lift :all :impl n*}

   "SHEET"  {:arity [0 1] :args :refs :impl sheet*}
   "SHEETS" {:arity [0 1] :args :refs :impl sheets*}
   "CELL"   {:arity [1 2] :args :refs :impl cell*}
   "INFO"   {:arity [1 1] :ctx? true :impl info*}})
