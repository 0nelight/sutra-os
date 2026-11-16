(ns sutra-ui.vendor.plotly
  (:require ["plotly.js-dist-min" :as Plotly]))

(set! (.-Plotly js/window) Plotly)

