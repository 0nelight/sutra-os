(ns ^:unit apps.safe-url-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.kit.url :as url]
            [sutra.capps.hyperlink :as hyperlink]
            [sutra.capps.iframe-bridge :as bridge]
            [sutra.parser.lint.source :as lint-source]
            [sutra-ui.apphost.markdown-text :as mt]))

(defn- c [n] (str (char n)))

(deftest a-link-keeps-the-schemes-of-the-allowlist
  (doseq [u ["https://example.org/a?b=1#c" "http://x" "HTTPS://X" "mailto:a@b.example"
             "tel:+43123" "/object/a/b" "#teil" "?q=1" "./a" "../a" "seite.html"]]
    (is (= u (url/safe-url u)) u)))

(deftest a-scheme-hidden-behind-what-a-browser-strips-is-still-seen
  (doseq [u [(str (c 1) "javascript:alert(1)")
             (str " " (c 0) "javascript:alert(1)")
             "java\tscript:alert(1)"
             "java\nscript:alert(1)"
             (str "java" (c 13) "script:alert(1)")
             (str (c 0xFEFF) "javascript:alert(1)")
             (str (c 0x2028) "javascript:alert(1)")
             "JaVaScRiPt:alert(1)"
             "vbscript:msgbox(1)"
             "data:text/html,<script>alert(1)</script>"
             "&#106;avascript:alert(1)"
             "&#x6A;avascript:alert(1)"
             "file:///etc/passwd"
             "//evil.example/x"
             "/\\evil.example/x"
             "\\\\evil.example/x"]]
    (is (nil? (url/safe-url u)) (pr-str u))
    (is (= "#" (url/href u)) (pr-str u))))

(deftest what-a-link-carries-has-no-control-characters
  (is (= "https://x/a" (url/safe-url (str "  https://x/" (c 1) "a\t ")))))

(deftest an-image-takes-a-base64-image-but-no-other-data
  (is (url/image-src "data:image/png;base64,iVBORw0KGgo="))
  (is (url/image-src "data:image/svg+xml;base64,PHN2Zy8+"))
  (is (nil? (url/image-src "data:text/html;base64,PHNjcmlwdD4=")))
  (is (nil? (url/image-src "data:image/svg+xml,<svg onload=alert(1)>")))
  (is (nil? (url/image-src "mailto:a@b.example")))
  (is (= "/blob/1" (url/image-src "/blob/1"))))

(deftest the-callers-share-the-rule
  (testing "markdown of a rich text field"
    (let [h (mt/markdown->html (str "[x](" (c 1) "javascript:alert(1))"))]
      (is (str/includes? h "href=\"#\""))
      (is (not (str/includes? h "javascript"))))
    (is (str/includes? (mt/markdown->html "[x](https://a.at/b)") "href=\"https://a.at/b\"")))
  (testing "a hyperlink button falls back to the landing page"
    (is (= hyperlink/landing-page
           (hyperlink/href {:url-type :hardcoded :url "java\tscript:alert(1)"} {}))))
  (testing "an iframe frames http(s) and this host only"
    (is (nil? (bridge/safe-src "mailto:a@b.example")))
    (is (nil? (bridge/safe-src "//evil.example")))
    (is (= "https://a.at" (bridge/safe-src "https://a.at")))))

(deftest the-linter-warns-of-what-the-app-sandbox-refuses
  (let [found (fn [src]
                (set (map :severity (lint-source/lint-app-source [{:name "a" :source src}]))))]
    (is (= #{:warn} (found "(fn [] (.-cookie js/document))")))
    (is (= #{:warn} (found "(fn [] [:div {:dangerouslySetInnerHTML {:__html x}}])")))
    (is (= #{} (found "(fn [] (js/Math.max 1 (app/event-value e)))")))))
