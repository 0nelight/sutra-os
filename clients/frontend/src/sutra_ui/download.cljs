(ns sutra-ui.download)

(defn blob!
  [blob file-name]
  (let [url (js/URL.createObjectURL blob)
        a   (.createElement js/document "a")]
    (set! (.-href a) url)
    (set! (.-download a) file-name)
    (.appendChild (.-body js/document) a)
    (.click a)
    (.removeChild (.-body js/document) a)
    (js/setTimeout #(js/URL.revokeObjectURL url) 10000)
    true))

(defn b64->blob
  [b64 mime]
  (let [bin (js/atob b64)
        arr (js/Uint8Array. (.-length bin))]
    (dotimes [i (.-length bin)] (aset arr i (.charCodeAt bin i)))
    (js/Blob. #js [arr] #js {:type mime})))

(defn text!
  [text mime file-name]
  (blob! (js/Blob. #js [text] #js {:type mime}) file-name))

(defn b64!
  [b64 mime file-name]
  (blob! (b64->blob b64 mime) file-name))
