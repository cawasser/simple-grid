(ns simple-grid.widget.alpha
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [taoensso.timbre :as log]
            [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
            [simple-grid.widget.registry :as registry]))


(defn- make-content [widget]
  (let [data  @(rf/subscribe [:timer (:data widget)])
        local @(rf/subscribe [:local (:name widget)])]
    [:div
     [:button.button {:on-click #(rf/dispatch [:reset (:data widget)])} "reset"]
     [:button.button {:on-click #(rf/dispatch [:local (:name widget)])} "local"]
     [:p (str "local: " local)]
     [:p (str "timer " (:data widget) ": " data)]]))



; register this widget type so the grid can instantiate instances as needed
;
(def widget-def {:type "alpha" :build-fn simple-grid.widget.alpha/make-content})
(registry/register widget-def)

