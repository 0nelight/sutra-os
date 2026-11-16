(ns sutra-ui.editor.rich-text.module
  (:require [sutra-ui.editor.rich-text.editor :as rich]
            [sutra-ui.apphost.markdown-editor :refer [markdown-editor]]
            [sutra-ui.editor.rich-text :as rich-text]))

(rich-text/provide! {:create!         rich/create!
                     :markdown-editor markdown-editor})
