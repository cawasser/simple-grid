(ns simple-grid.widget.alpha
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [taoensso.timbre :as log]
            [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
            [simple-grid.widget.registry :as registry]))




(defn- make-content
  "FORM-1 by de-ref's ':data' and ':local'"
  [widget global]
  [:div
   [:button.button {:on-click #(rf/dispatch [:local (:name widget)])} "local"]
   [:p (str "local: " (:local widget))]
   [:p (str "global: " global)]])



; register this widget type so the grid can instantiate instances as needed
;
(def widget-def {:type "alpha" :config {:local 0} :build-fn simple-grid.widget.alpha/make-content})
(registry/register widget-def)

