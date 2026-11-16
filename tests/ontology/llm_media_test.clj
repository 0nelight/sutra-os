(ns ^:unit ontology.llm-media-test
  (:require [support.system :as tsys]
            [support.replay :as replay]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]
            [sutra.llm.media :as llm-media]
            [sutra.actions.logic :as logic]
            [sutra.catalog.program :as prog]
            [sutra.blobs.api :as blobs]
            [sutra.blobs.mem :as blobs-mem])
  (:import (java.awt Color)
           (java.awt.image BufferedImage)
           (java.io ByteArrayOutputStream)
           (javax.imageio ImageIO)
           (org.apache.pdfbox.pdmodel PDDocument PDPage PDPageContentStream)
           (org.apache.pdfbox.pdmodel.font PDType1Font Standard14Fonts$FontName)
           (org.apache.pdfbox.pdmodel.graphics.image LosslessFactory)))

(defn- pdf-bytes
  ^bytes [pages]
  (with-open [doc (PDDocument.)]
    (doseq [p pages]
      (let [page (PDPage.)]
        (.addPage doc page)
        (with-open [cs (PDPageContentStream. doc page)]
          (if (string? p)
            (doto cs
              (.beginText)
              (.setFont (PDType1Font. Standard14Fonts$FontName/HELVETICA) (float 12))
              (.newLineAtOffset (float 72) (float 700))
              (.showText ^String p)
              (.endText))
            (let [img (BufferedImage. 200 100 BufferedImage/TYPE_INT_RGB)]
              (doto (.createGraphics img) (.setColor Color/WHITE) (.fillRect 0 0 200 100)
                    (.setColor Color/BLACK) (.drawString "RECHNUNG 4711" 20 50))
              (.drawImage cs (LosslessFactory/createFromImage doc img) (float 72) (float 600)))))))
    (let [out (ByteArrayOutputStream.)]
      (.save doc out)
      (.toByteArray out))))

(defn- png-bytes ^bytes []
  (let [img (BufferedImage. 4 4 BufferedImage/TYPE_INT_RGB) out (ByteArrayOutputStream.)]
    (ImageIO/write img "png" out)
    (.toByteArray out)))

(defn- with-store [f]
  (let [store (blobs-mem/blobs)]
    (f store (fn [^bytes bs] (:hash (blobs/put! store bs))))))

(def ^:private rechnung
  "Rechnung Nr. RE-2026-0815 vom 12.09.2026, Netto 1.000,00 EUR, USt 200,00 EUR")

(deftest a-pdf-with-a-text-layer-is-read-as-text
  (with-store
    (fn [store put!]
      (let [h (put! (pdf-bytes [rechnung "Seite zwei mit genug Text fuer eine Textebene"]))
            {:keys [parts media]} (llm-media/parts store [h] {})]
        (is (= 2 (count parts)))
        (is (every? #(= "text" (get % "type")) parts))
        (is (str/includes? (get (first parts) "text") "RE-2026-0815"))
        (is (= [{:hash h :kind "application/pdf" :pages 2 :read 2 :as [:text]}] media))))))

(deftest a-scanned-page-is-shown-as-an-image
  (with-store
    (fn [store put!]
      (let [h (put! (pdf-bytes [rechnung :scan]))
            {:keys [parts media]} (llm-media/parts store [h] {})]
        (testing ":auto reads the text layer where there is one and renders the scan"
          (is (= ["text" "image_url"] (mapv #(get % "type") parts)))
          (is (str/starts-with? (get-in (second parts) ["image_url" "url"])
                                "data:image/jpeg;base64,"))
          (is (= [:text :image] (:as (first media)))))
        (testing ":image renders every page, :text shows only the text layer"
          (is (= ["image_url" "image_url"]
                 (mapv #(get % "type") (:parts (llm-media/parts store [h] {:as :image})))))
          (is (= ["text"] (mapv #(get % "type") (:parts (llm-media/parts store [h] {:as :text}))))))
        (testing ":media-pages bounds what is read"
          (is (= 1 (count (:parts (llm-media/parts store [h] {:pages 1})))))
          (is (= 1 (:read (first (:media (llm-media/parts store [h] {:pages 1})))))))))))

(deftest images-and-text-and-what-cannot-be-read
  (with-store
    (fn [store put!]
      (let [png (put! (png-bytes))
            txt (put! (.getBytes "Mailtext: bitte zahlen" "UTF-8"))]
        (is (= [{"type" "image_url"}]
               (mapv #(select-keys % ["type"]) (:parts (llm-media/parts store [png] {})))))
        (is (str/includes? (get (first (:parts (llm-media/parts store [txt] {}))) "text")
                           "bitte zahlen")))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"is not in the blob store"
                            (llm-media/parts store [(apply str (repeat 64 "a"))] {})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a media reference"
                            (llm-media/parts store ["../etc/passwd"] {})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"neither a PDF, an image nor text"
                            (llm-media/parts store
                                             [(put!
                                               (byte-array
                                                [0 (unchecked-byte 0xff) 1 (unchecked-byte 0xfe)]))]
                                             {}))))))

(deftest a-reference-may-be-one-hash-or-many
  (is (= ["a"] (llm-media/hashes-of "a")))
  (is (= ["a" "b"] (llm-media/hashes-of ["a" nil "b"])))
  (is (= [] (llm-media/hashes-of nil))))

(def ^:private world
  {:object-types {:eingangsdokument {:object-type-name :eingangsdokument
                                     :attribute-list
                                       [{:attribute-name :pdf :attribute-type :media-reference}
                                        {:attribute-name :betreff :attribute-type :string}]
                                     :plural "Eingangsdokumente"}}
   :link-types {} :actions {}
   :llm-models {"grok-test" {:in 1 :out 1}}})

(def ^:private params {:dokument {:binding :dokument :type :eingangsdokument}})

(defn- final [v]
  {:tool_calls [{:id "c1" :type "function"
                 :function {:name "final_answer" :arguments (json/write-str {"value" v})}}]})

(deftest use-llm-shows-the-model-the-pdf
  (with-store
    (fn [store put!]
      (let [h        (put! (pdf-bytes [rechnung]))
            seen     (atom [])
            compiled (prog/compile-function
                      world
                      [{:op :use-llm :model "grok-test" :source :xai
                        :prompt "Lies den Beleg {{dokument.betreff}}." :media ["{{dokument.pdf}}"]
                        :output {:struct {:rechnungsnr :string}} :as :lesung}]
                      params)
            r        (tsys/with-provided
                       {:blobs/db     (constantly store)
                        :llm/complete (fn [req] (swap! seen conj req)
                                           {:request-id "req-1"
                                            :message (final {"rechnungsnr" "RE-2026-0815"})})}
     (replay/evaluate
      {:verbs logic/verbs :store {}}
      #(kernel/eval-steps (:steps compiled) world
                        {:dokument {:id "d1" :object-type-ref :eingangsdokument
                                    :values {:pdf h :betreff "Mail von Teile Maier"}}}
                        %1 %2)))
            decided  (:decisions r)
            content  (get-in (first @seen) [:request "messages" 0 "content"])]
        (testing "the prompt stays the first part, the PDF's text layer follows"
          (is (= "Lies den Beleg Mail von Teile Maier." (get (first content) "text")))
          (is (str/includes? (get (second content) "text") "RE-2026-0815")))
        (testing "the answer is bound as declared"
          (is (some #{{:rechnungsnr "RE-2026-0815"}} (vals (:bindings r)))))
        (testing "the decision log and the step's event carry what the model was shown"
          (is (= [h] (mapv :hash (:media (first decided)))))
          (is (re-matches #"[0-9a-f]{64}" (:prompt-sha (first decided))))
          (let [e (some #(when (= :use-llm (:op %)) %) (:side-effects r))]
            (is (= [h] (:media e)))
            (is (= "req-1" (first (:request-ids e))))
            (is (= (:prompt-sha (first decided)) (:prompt-sha e)))))))))

(deftest a-media-list-that-cannot-run-is-refused-at-compile
  (let [step {:op :use-llm :model "grok-test" :source :xai :prompt "?"}
        msg  #(try (prog/compile-function world [(merge step %)] params) nil
                (catch Exception e (ex-message e)))]
    (is (nil? (msg {:media ["{{dokument.pdf}}"] :media-as :image :media-pages 2})))
    (is (re-find #"single" (msg {:media ["Beleg {{dokument.pdf}}"]})))
    (is (re-find #"Unknown binding" (msg {:media ["{{beleg.pdf}}"]})))
    (is (re-find #":media-as" (msg {:media ["{{dokument.pdf}}"] :media-as :fax})))))
