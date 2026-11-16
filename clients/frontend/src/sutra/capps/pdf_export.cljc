(ns sutra.capps.pdf-export)

(defn- ascii-len [s] (count s))

(defn- page-objects
  [id {:keys [w h px-w px-h]} i image-len]
  (let [content (str "q " w " 0 0 " h " 0 0 cm /I Do Q\n")]
    [[id (str "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 " w " " h "] "
              "/Resources << /XObject << /I " (+ id 2) " 0 R >> >> "
              "/Contents " (+ id 1) " 0 R >>")]
     [(+ id 1) (str "<< /Length " (ascii-len content) " >>\nstream\n" content "endstream")]
     [(+ id 2) [(str "<< /Type /XObject /Subtype /Image /Width " px-w " /Height " px-h
                     " /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode"
                     " /Length " image-len " >>\nstream\n")
                {:image i}
                "\nendstream"]]]))

(defn- part-len
  [image-lens p]
  (if (string? p) (ascii-len p) (nth image-lens (:image p))))

(defn parts
  [pages image-lens]
  (let [n      (count pages)
        kid-id (fn [i] (+ 3 (* 3 i)))
        kids   (apply str (map #(str (kid-id %) " 0 R ") (range n)))
        objs   (into [[1 "<< /Type /Catalog /Pages 2 0 R >>"]
                      [2 (str "<< /Type /Pages /Kids [" kids "] /Count " n " >>")]]
                     (mapcat (fn [i p] (page-objects (kid-id i) p i (nth image-lens i)))
                             (range n) pages))
        header "%PDF-1.4\n"
        step   (fn [{:keys [out at offsets]} [id body]]
                 (let [body-parts (if (string? body) [body] body)
                       ps (-> [(str id " 0 obj\n")] (into body-parts) (conj "\nendobj\n"))
                       sz (reduce + (map #(part-len image-lens %) ps))]
                   {:out (into out ps) :at (+ at sz) :offsets (assoc offsets id at)}))
        {:keys [out at offsets]} (reduce step
                                         {:out [header] :at (ascii-len header) :offsets {}}
                                         objs)
        total  (inc (count objs))
        pad10  (fn [x] (let [s (str x)] (str (apply str (repeat (- 10 (count s)) "0")) s)))
        xref   (apply str
                      "xref\n0 " total "\n0000000000 65535 f \n"
                      (map (fn [id] (str (pad10 (get offsets id)) " 00000 n \n"))
                           (range 1 total)))]
    (conj out xref
          (str "trailer\n<< /Size " total " /Root 1 0 R >>\nstartxref\n" at "\n%%EOF\n"))))
