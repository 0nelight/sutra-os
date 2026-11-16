(ns sutra.packages.standard
  (:require [sutra.kit.tree :as tree]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.parser.ent-file :as ent]
            [sutra.parser.presets :as presets]
            [sutra.types.membership :as member])
  (:import (java.io File)
           (java.net URL)
           (java.nio.file Files StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def resource-dir
  "presets/standard/")

(def catalogue
  [{:id          "basis"
    :version     "1.0.0"
    :file        "1_basis.ent"
    :requires    []
    :name        "Basis"
    :description (str "Basistypen, die jeder hat: Interfaces fuer Partei, Geldkonto, "
                      "Kontobewegung, Beleg, Vertrag, Frist, Besitz, Vorgang, Ausgehend "
                      "und die Anker Person, Organisation, Ort, Dokument, Ausgleich.")}
   {:id          "privat"
    :version     "1.0.0"
    :file        "3_privat.ent"
    :requires    [:basis]
    :name        "Haushalt"
    :description (str "Anwendungsfaelle Haushalt: Budget, Kategorie-Regel, Ausweis, "
                      "Steuerposten, Schadensfall; Kanaele Zahlungsdienst und X.")}
   {:id          "betrieb"
    :version     "1.0.0"
    :file        "3_betrieb.ent"
    :requires    [:basis]
    :name        "Betrieb"
    :description (str "Anwendungsfaelle Betrieb: Kundenbeziehung, Projekt, Angebot, "
                      "Auftrag, Anstellung, Zeitbuchung, Ausgangsrechnung, Mahnung.")}
   {:id          "einzelunternehmen"
    :version     "1.0.0"
    :file        "4_einzelunternehmen.ent"
    :requires    [:basis :privat :betrieb]
    :name        "Einzelunternehmen"
    :description (str "Haushalt und Betrieb in einer Ontologie -- Einzelunternehmen, "
                      "Freiberufler, Landwirtschaft; ein gemeinsames Bankkonto-Modell.")}
   {:id          "mail"
    :version     "1.0.0"
    :file        "5_mail.ent"
    :requires    []
    :name        "Mail"
    :description (str "Das Postfach als Objekte: Mails und Anhaenge ueber alle Ordner, Threads "
                      "per In-Reply-To. Ein IMAP-Connector fuellt es: source adopt <quelle> "
                      "messages --into postfach_mails.")}])

(defn- resource ^URL [file]
  (or (io/resource (str resource-dir file))
      (throw (ex-info (str "package file not on the classpath: " resource-dir file)
                      {:file file}))))

(defn- requires-of
  [file]
  (vec (:require (edn/read-string (slurp (resource file))))))

(defn file-chain
  [file]
  (tree/reach requires-of [file]))

(defonce ^:private extracted
  (delay
    (let [dir (.toFile (Files/createTempDirectory "sutra-presets" (make-array FileAttribute 0)))]
      (.deleteOnExit dir)
      dir)))

(defn resource-path
  ^String [file]
  (let [u (resource file)]
    (if (= "file" (.getProtocol u))
      (.getPath (io/file u))
      (let [^File dir @extracted]
        (doseq [f (file-chain file)
                :let [target (io/file dir f)]]
          (with-open [in (io/input-stream (resource f))]
            (Files/copy in (.toPath target)
                        ^"[Ljava.nio.file.CopyOption;"
                        (into-array [StandardCopyOption/REPLACE_EXISTING])))
          (.deleteOnExit target))
        (.getPath (io/file dir file))))))

(def stage-projects
  {"1_basis.ent"             {:name :basis             :description "Basistypen, die jeder hat"}
   "2_abgeleitet.ent"        {:name :alltag
                              :description
                                "Konkrete Typen und die Arbeit, die Haushalt und Betrieb gleich tun"}
   "3_privat.ent"            {:name :haushalt          :description "Anwendungsfaelle Haushalt"}
   "3_betrieb.ent"           {:name :betrieb           :description "Anwendungsfaelle Betrieb"}
   "4_einzelunternehmen.ent" {:name :einzelunternehmen
                              :description "Haushalt und Betrieb in einer Ontologie"}})

(def ^:private placed-sections
  (disj (set (vals member/author-sections)) :projects))

(defn- declared-in
  [file]
  (let [chain (sort-by (comp count file-chain) (file-chain file))]
    (reduce (fn [acc f]
              (let [s (:schema (edn/read-string (slurp (resource f))))]
                (reduce (fn [acc [section m]]
                          (if (and (placed-sections section) (map? m))
                            (reduce (fn [acc k] (update acc [section (keyword (name k))] #(or % f)))
                                    acc (keys m))
                            acc))
                        acc s)))
            {} chain)))

(defn place
  [file schema]
  (let [owners (declared-in file)
        used   (volatile! #{})
        placed (reduce-kv (fn [acc [section n] f]
                            (let [p (:name (stage-projects f))]
                              (if (and p (map? (get-in acc [section n]))
                                       (nil? (get-in acc [section n :project])))
                                (do (vswap! used conj f) (assoc-in acc [section n :project] p))
                                acc)))
                          schema owners)]
    (reduce (fn [acc f]
              (let [{:keys [name description]} (stage-projects f)]
                (update-in acc [:projects name] #(or % {:description description}))))
            placed @used)))

(defn read-package
  [file]
  (place file (presets/mit-startanwendungen (:schema (ent/read-ent (resource-path file))))))

(defn by-id [id] (some #(when (= (name id) (:id %)) %) catalogue))

(def module
  {:name        :packages
   :contributes {:products/offers
                 (into {}
                       (map-indexed (fn [i {:keys [id file name description requires version]}]
                                      [(keyword id) {:name        name
                                                     :description description
                                                     :version     version
                                                     :order       i
                                                     :requires    requires
                                                     :schema      #(read-package file)}]))
                       catalogue)}})
