(ns simple-grid.widget.beta
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [taoensso.timbre :as log]
            [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
            [simple-grid.widget.registry :as registry]))


;; region ; Re-frame handlers and subscriptions

(rf/reg-event-db
  :append-local
  (fn-traced [db [_ widget-id val]]
    (let [old-val (get-in db [:widgets widget-id :local])]
      (assoc-in db [:widgets widget-id :local] (conj old-val val)))))


(rf/reg-event-db
  :append-global
  (fn-traced [db [_ widget-id val]]
    (log/info ":append-global" widget-id val)
    (let [old-val (get-in db [:global widget-id])]
      (assoc-in db [:global widget-id] (conj old-val val)))))


(rf/reg-sub
  :global
  (fn [db [_ global-id]]
    (get-in db [:global global-id])))

;; endregion


(defn- row [idx item]
  ^{:key idx} [:tr [:td item]])


(defn- table [title data]
  [:div.table-container {:style {:width       "50%"
                                 :height      "5em"
                                 :overflow-y  :auto
                                 :white-space :nowrap
                                 :border      "1px outset dark-blue"
                                 :color       :black}}
   [:table.table
    [:thead {:style {:width  "100%"
                     :border      "1px outset dark-blue"
                     :color  :black}}
     [:tr [:th title]]]
    [:tbody
     (doall
       (for [[idx item] (map-indexed vector data)]
         (row idx item)))]]])


(defn- make-content
  "FORM-1 by de-ref's ':data' and ':local'"
  [widget global]
  [:div
   [:button.button {:on-click #(rf/dispatch [:append-local (:name widget) (rand-int 30)])} "local"]
   [:button.button {:on-click #(rf/dispatch [:append-global (:name widget) (rand-int 30)])} "global"]
   [:div.flex-container
    (table "Local" (:local widget))
    (table "Global" global)]])



; register this widget type so the grid can instantiate instances as needed
;
(def widget-def {:type "beta" :config {:local []} :build-fn simple-grid.widget.beta/make-content})
(registry/register widget-def)


; try out the handlers and subscriptions
(comment
  (def db @re-frame.db/app-db)
  (rf/dispatch [:append-local "four" (rand-int 30)])

  (rf/dispatch-sync [:reset-db])
  (:widgets @re-frame.db/app-db)

  (rf/dispatch [:add-widget "three" "one" (get @registry/registry "beta")])
  (rf/dispatch [:append-local "three" (rand-int 30)])

  (rf/subscribe [:global "three"])


  ())