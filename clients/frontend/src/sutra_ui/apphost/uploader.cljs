(ns sutra-ui.apphost.uploader
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apphost.client :refer [upload! upload-status!]]
            [sutra-ui.apphost.frame :as fr]))

(defn file->b64
  [file cb on-err]
  (let [rdr (js/FileReader.)]
    (set! (.-onload rdr) (fn [_] (cb (str/replace (.-result rdr) #"^data:[^;]*;base64," ""))))
    (set! (.-onerror rdr) (fn [_] (on-err (str "Datei nicht lesbar: " (.-name file)))))
    (.readAsDataURL rdr file)))

(defn- glob->re [g]
  (js/RegExp. (str "^" (-> (str g)
                           (str/replace #"[.+^${}()|\[\]\\]" "\\$&")
                           (str/replace "*" ".*")
                           (str/replace "?" "."))
                   "$")
              "i"))

(defn- route-meta
  [routes fname]
  (some (fn [{:keys [match meta]}] (when (.test (glob->re match) fname) (or meta {}))) routes))

(defn- upload-note
  [r]
  (cond
    (:already r)                   {:tone "info"
                                    :text "schon hochgeladen — identischer Inhalt, nichts Neues"}
    (get-in r [:build :scheduled]) {:tone "info" :text "gespeichert — wird verarbeitet"
                                    :scheduled? true}
    (get-in r [:build :queued])   {:tone "info" :text (get-in r [:build :error])}
    (or (get-in r [:build :error]) (get-in r [:action :error]))
    {:tone "warn" :text (or (get-in r [:build :error]) (get-in r [:action :error]))}
    :else
    {:tone "ok"
     :text (let [n (some->> (:counts r) vals (filter number?) (reduce + 0))]
             (str "Verarbeitet" (when (and n (pos? n)) (str " — " n " Zeile(n)")) "."))}))

(defn media-uploader
  [_descriptor {:keys [frame]}]
  (let [own   (when-not frame (fr/capp-frame))
        busy  (r/atom false)
        over  (r/atom false)
        notes (r/atom nil)]
    (fn [descriptor
         {:keys [on-ok on-error on-processed accept label meta multiple routes drop-label
                 intent left-icon right-icon max-files capture destination media-column branch
                 on-file frame]}]
      (let [f      (or frame own)
            ask!   (fn [ask ok err] (fr/request! f (assoc (fr/kept ok err) :ask ask)))
            kb    (fn [b] (let [n (/ b 1024.0)]
                             (if (< n 1024)
                               (str (js/Math.round n) " KB")
                               (str (.toFixed (/ n 1024) 1) " MB"))))
            note!  (fn [nm n] (swap! notes (fnil conj []) (assoc n :name nm)))
            settle (fn [{:keys [status error]}]
                     (reset! busy false)
                     (swap! notes
                            (fn [ns]
                              (mapv (fn [n]
                                      (if (:scheduled? n)
                                        (if (= status "failed")
                                          (assoc n :tone "danger" :scheduled? false
                                                 :text
                                                 (str "Verarbeitung fehlgeschlagen"
                                                      (when error (str ": " error))))
                                          (assoc n :tone "ok" :scheduled? false
                                                 :text "verarbeitet"))
                                        n))
                                    ns)))
                     (when on-processed (on-processed status)))
            watch! (fn watch! [since n]
                     (reset! busy {:processing true})
                     (ask! [upload-status! descriptor since]
                           (fn [status]
                             (cond
                               (:done status) (settle status)
                               (< n 100)  (js/setTimeout #(watch! since (inc n)) 3000)
                               :else      (do (reset! busy false)
                                              (note! ""
                                                     {:tone "warn"
                                                      :text "Die Verarbeitung dauert ungewöhnlich lange — später neu laden."}))))
                           (fn [msg] (reset! busy false)
                             (note! "" {:tone "warn" :text msg}))))
            handle
            (fn [files]
              (let [cap     (if multiple (or max-files (count files)) 1)
                    files   (vec (take cap files))
                    planned (for [f files
                                  :let [m (if (seq routes) (route-meta routes (.-name f)) meta)]]
                              {:file f :meta m})
                    refused (if (seq routes) (remove :meta planned) [])
                    todo    (vec (if (seq routes) (filter :meta planned) planned))]
                (reset! notes nil)
                (doseq [{f :file} refused]
                  (note! (.-name f)
                         {:tone "danger"
                          :text "keine Regel für diesen Dateinamen — nicht hochgeladen"}))
                (when (seq todo)
                  (letfn [(step [i since]
                            (if (>= i (count todo))
                              (if since (watch! since 0) (reset! busy false))
                              (let [{file :file m :meta} (nth todo i)
                                    last? (= i (dec (count todo)))]
                                (reset! busy {:size (.-size file) :i (inc i) :n (count todo)})
                                (file->b64
                                 file
                                 (fn [b64]
                                   (ask! [upload! descriptor
                                          (merge {:bytes b64 :dateiname (.-name file)
                                                  :mime (.-type file)}
                                                 (when destination
                                                   {:destination  destination
                                                    :media-column media-column})
                                                 (when (map? m) m))
                                          {:branch branch}]
                                         (fn [r]
                                           (note! (.-name file) (upload-note r))
                                           (when on-file (on-file r (.-name file)))
                                           (when (and last? on-ok) (on-ok r))
                                           (step (inc i)
                                                 (or since (get-in r [:build :since]))))
                                         (fn [msg]
                                           (note! (.-name file) {:tone "danger" :text msg})
                                           (when on-error (on-error msg))
                                           (step (inc i) since))))
                                 (fn [msg]
                                   (note! (.-name file) {:tone "danger" :text msg})
                                   (step (inc i) since))))))]
                    (step 0 nil)))))]
        [:label {:class         (cond-> (str "capp-uploader"
                                             (when intent (str " capp-uploader--" (name intent))))
                                  @busy (str " capp-uploader--busy")
                                  @over (str " capp-uploader--over"))
                 :on-drag-over  (fn [e] (.preventDefault e) (reset! over true))
                 :on-drag-leave (fn [_] (reset! over false))
                 :on-drop       (fn [e] (.preventDefault e) (reset! over false)
                                  (handle (array-seq (.. e -dataTransfer -files))))}
         [:input
          {:type "file" :accept accept :multiple (boolean multiple) :class "capp-uploader__file"
           :capture capture
           :on-change (fn [e] (handle (array-seq (.. e -target -files))))}]
         [:div.capp-uploader__icon (or left-icon "⬆")]
         [:div.capp-uploader__label
          (cond
            (:processing @busy) "Wird verarbeitet…"
            @busy (let [{:keys [size i n]} @busy]
                    (str "Lädt" (when (> n 1) (str " " i "/" n)) " " (kb size)
                         " — und verarbeitet…"))
            @over (or drop-label "Loslassen zum Hochladen")
            :else (or label (if multiple "Dateien hochladen oder hierher ziehen"
                                "Datei hochladen oder hierher ziehen")))]
         (when right-icon [:div.capp-uploader__icon right-icon])
         (when @busy [ui/progress-bar {:class "capp-uploader__bar" :intent :primary}])
         (for [[k {:keys [name tone text]}] (map-indexed vector @notes)]
           ^{:key k}
           [:div {:class (str "capp-uploader__note capp-uploader__note--" tone)}
            (if (and (> (count @notes) 1) (seq name)) (str name ": " text) text)])]))))
