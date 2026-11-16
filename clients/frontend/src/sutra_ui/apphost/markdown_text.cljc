(ns sutra-ui.apphost.markdown-text
  (:require [clojure.string :as str]
            [sutra.kit.url :as url]))

(defn- esc
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- inline->html
  [s]
  (-> (esc s)
      (str/replace #"`([^`]+)`" "<code>$1</code>")
      (str/replace #"\*\*([^*]+)\*\*" "<strong>$1</strong>")
      (str/replace #"~~([^~]+)~~" "<s>$1</s>")
      (str/replace #"\*([^*]+)\*" "<em>$1</em>")
      (str/replace #"\[([^\]]+)\]\(([^)\s]+)\)"
                   (fn [[_ t u]] (str "<a href=\"" (url/href u) "\">" t "</a>")))))

(defn- list-html
  [tag lines re]
  (str "<" tag ">"
       (str/join (map (fn [l] (str "<li><p>" (inline->html (second (re-find re l))) "</p></li>"))
                      lines))
       "</" tag ">"))

(def ^:private bullet #"^\s*[-*]\s+(.*)$")
(def ^:private ordered #"^\s*\d+\.\s+(.*)$")

(defn markdown->html
  [text]
  (loop [ls (str/split-lines (str text)) out []]
    (if-let [l (first ls)]
      (cond
        (str/blank? l)
        (recur (rest ls) out)

        (str/starts-with? l "```")
        (let [[body more] (split-with #(not (str/starts-with? % "```")) (rest ls))]
          (recur (rest more)
                 (conj out (str "<pre><code>" (esc (str/join "\n" body)) "</code></pre>"))))

        (re-find #"^(#{1,3})\s+(.*)$" l)
        (let [[_ h t] (re-find #"^(#{1,3})\s+(.*)$" l)
              n (count h)]
          (recur (rest ls) (conj out (str "<h" n ">" (inline->html t) "</h" n ">"))))

        (re-find bullet l)
        (let [[xs more] (split-with #(re-find bullet %) ls)]
          (recur more (conj out (list-html "ul" xs bullet))))

        (re-find ordered l)
        (let [[xs more] (split-with #(re-find ordered %) ls)]
          (recur more (conj out (list-html "ol" xs ordered))))

        (str/starts-with? l ">")
        (let [[xs more] (split-with #(str/starts-with? % ">") ls)]
          (recur more (conj out (str "<blockquote><p>"
                                     (inline->html
                                      (str/join " " (map #(str/replace % #"^>\s?" "") xs)))
                                     "</p></blockquote>"))))

        :else
        (recur (rest ls) (conj out (str "<p>" (inline->html l) "</p>"))))
      (str/join out))))

(defn- marked
  [{:keys [text marks]}]
  (reduce (fn [s {:keys [type attrs]}]
            (case type
              "bold"   (str "**" s "**")
              "italic" (str "*" s "*")
              "strike" (str "~~" s "~~")
              "code"   (str "`" s "`")
              "link"   (str "[" s "](" (:href attrs) ")")
              s))
          (str text) marks))

(declare block->md)

(defn- inlines [content] (str/join (map marked content)))

(defn- items
  [content prefix]
  (str/join "\n" (map-indexed (fn [i {:keys [content]}]
                                (str (prefix i) (str/join " " (map block->md content))))
                              content)))

(defn- block->md
  [{:keys [type attrs content]}]
  (case type
    "paragraph"   (inlines content)
    "heading"     (str (apply str (repeat (:level attrs 1) "#")) " " (inlines content))
    "bulletList"  (items content (constantly "- "))
    "orderedList" (items content #(str (inc %) ". "))
    "blockquote"  (str "> " (str/join " " (map block->md content)))
    "codeBlock"   (str "```\n" (inlines content) "\n```")
    "horizontalRule" "---"
    (inlines content)))

(defn doc->markdown
  [{:keys [content]}]
  (str/join "\n\n" (map block->md content)))

(def toolbar
  [{:id :bold :label "B" :title "Fett" :around "**"}
   {:id :italic :label "I" :title "Kursiv" :around "*"}
   {:id :strike :label "S" :title "Durchgestrichen" :around "~~"}
   {:id :code :label "</>" :title "Code" :around "`"}
   {:id :bullet :label "•" :title "Liste" :line "- "}
   {:id :ordered :label "1." :title "Nummerierte Liste" :line "1. "}
   {:id :heading :label "H" :title "Überschrift" :line "## "}])

(defn format-raw
  [text start end {:keys [around line]}]
  (let [s      (str text)
        before (subs s 0 start)
        sel    (subs s start end)
        after  (subs s end)]
    (if around
      [(str before around sel around after) (+ end (* 2 (count around)))]
      (let [at (inc (or (str/last-index-of before "\n") -1))]
        [(str (subs s 0 at) line (subs s at)) (+ end (count line))]))))
