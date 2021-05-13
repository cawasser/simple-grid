(ns simple-grid.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [taoensso.timbre :as log]
            [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]))

(def default-db {:widgets {"one" {:data "one" :local 0}}
                 :timers  {"one" 0}
                 :layout  [{:id "one" :grid-data {:x 0 :y 0 :w 2 :h 2}}]})


;; region ; Subscriptions

(rf/reg-event-db
  :init
  (fn-traced [db [_ data]]
    (merge db data)))


(rf/reg-sub
  :widgets
  (fn [db _]
    (:widgets db)))


(rf/reg-sub
  :timer
  (fn [db [_ id]]
    (get-in db [:timers id])))


(rf/reg-sub
  :timers
  (fn [db _]
    (:timers db)))


(rf/reg-sub
  :local
  (fn [db [_ widget-id]]
    (get-in db [:widgets widget-id :local])))

;; endregion


;; region ; Event Handlers

(rf/reg-event-db
  :add-widget
  (fn-traced [db [_ name data]]
    (-> db
      (assoc-in [:widgets name :data] data)
      (assoc-in [:widgets name :local] 0)
      (#(if (contains? (:timers db) data)
          %
          (assoc-in % [:timers data] 0))))))



(rf/reg-event-db
  :tick
  (fn-traced [db _]
    (assoc db :timers
              (reduce-kv
                (fn [new-map key value]
                  (assoc new-map key (inc value)))
                {}
                (:timers db)))))


(rf/reg-event-db
  :reset
  (fn-traced [db [_ widget-id]]
    (assoc-in db [:timers widget-id] 0)))



(rf/reg-event-db
  :local
  (fn-traced [db [_ widget-id]]
    (let [local (get-in db [:widgets widget-id :local])]
      (assoc-in db [:widgets widget-id :local] (inc local)))))



(rf/reg-event-db
  :delete-widget
  (fn-traced [db [_ widget-id]]
    (assoc db :widgets (dissoc (:widgets db) widget-id))))

;; endregion


;; region ; Widgets

(defn- make-content [name sub data]
  (str sub " :: " @data))


(defn- title-bar [name]
  [:div.title-bar {:on-click #(rf/dispatch [:delete-widget name])}
   [:h3 name]])


(defn- base [name content]
  [:div.widget.parent {:key name}
   (title-bar name)
   [:div.widget.widget-content content]])


(defn- widget [name w]
  (log/info "widget" w)

  (log/info "widget OUTER" name)

  (let [data (rf/subscribe [:timer (:data w)])
        local (rf/subscribe [:local name])]
    (fn []
      (log/info "widget INNER" name)
      (base
        name
        [:div
         [:button.button {:on-click #(rf/dispatch [:reset (:data w)])} "reset"]
         [:button.button {:on-click #(rf/dispatch [:local name])} "local"]
         [:p (str "local: " @local)]
         [:p (make-content name (:data w) data)]]))))

;; endregion

;; region ; Grid

(defn- simple-grid []
  (log/info "simple-grid OUTER")
  (let [widgets (rf/subscribe [:widgets])]
    (fn []
      (log/info "simple-grid INNER")
      [:div.grid
       (doall
         (for [[k w] @widgets]
           ^{:key k} [widget k w]))])))

;; endregion

;; region ; main-page

(defn- timers []
  (let [timers (rf/subscribe [:timers])]
    (fn []
      [:div.flex-container
       (doall
         (map (fn [[k t]] ^{:key k}
                [:div {:on-click #(rf/dispatch [:reset k])} t])
           @timers))])))


(defn- main-page []
  [:div
   [:h3 "Simple Grid"]
   [:button.button {:on-click #(rf/dispatch-sync [:tick])} "Tick!"]
   [:button.button {:on-click #(rf/dispatch [:add-widget "one" "one"])} "Add \"one\""]
   [:button.button {:on-click #(rf/dispatch [:add-widget "two" "two"])} "Add \"two\""]
   [:button.button {:on-click #(rf/dispatch [:add-widget "three" "one"])} "Add \"three\""]
   [timers]
   [simple-grid]])

;; endregion


(defn- ^:dev/after-load-async mount-components []
  (rd/render #'main-page (.getElementById js/document "app")))


(defn main []
  (log/info "Running!")

  (rf/dispatch-sync [:init default-db])
  (mount-components))


; rich comments
(comment
  @re-frame.db/app-db
  (rf/dispatch-sync [:add-widget "two" "two" 0])

  (def db @re-frame.db/app-db)

  (def name "one")
  (def data "two")
  (-> db
    (assoc-in [:widgets name :data] data)
    (#(if (contains? (:timers db) data)
        %
        (assoc-in % [:timers data] 0))))

  (assoc db :widgets
            (reduce-kv
              (fn [new-map key value]
                (assoc new-map key (assoc value :tick (inc (:tick value)))))
              {}
              (:widgets db)))

  (rf/dispatch [:tick])
  (rf/dispatch [:add-widget "two" "two"])
  (rf/dispatch [:add-widget "three" "two"])
  (rf/dispatch [:reset "one"])
  (rf/dispatch [:reset "two"])


  (rf/dispatch [:delete-widget "two"])

  ())