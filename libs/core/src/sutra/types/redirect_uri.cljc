(ns sutra.types.redirect-uri)

(def message
  "a redirect URI is absolute https://… , or http://localhost… for a desktop or development client")

(defn valid?
  [u]
  (boolean (or (re-matches #"https://[A-Za-z0-9.-]+(:\d+)?/\S*" (str u))
               (re-matches #"http://localhost(:\d+)?/\S*" (str u)))))

(defn invalid
  [uris]
  (vec (remove valid? uris)))
