(ns simple-grid.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [taoensso.timbre :as log]
            [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
            ["react-grid-layout" :refer (Responsive) :as ReactGridLayout]))

;; region ; DEFAULTS

(def default-db {:widgets {"one" {:data "one" :local 0}}
                 :timers  {"one" 0}
                 :layout  {"one" {:i "one" :x 0 :y 0 :w 2 :h 2}}})

(def default-widget
  {:x 0 :y 0 :w 2 :h 2})

;; endregion


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
  :layout
  (fn [db _]
    (vals (:layout db))))



(rf/reg-sub
  :widget
  (fn [db [_ widget-id]]
    (get-in db [:widgets widget-id])))



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
      (#(if (contains? (:layout db) name)
          %
          (-> %
            (assoc-in [:layout name] (assoc default-widget :i name))
            (assoc-in [:widgets name :data] data)
            (assoc-in [:widgets name :local] 0)
            ((fn [x] (if (contains? (:timers db) data)
                       x
                       (assoc-in x [:timers data] 0))))))))))


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
  :tick-one
  (fn-traced [db [_ id]]
    (assoc-in db [:timers id] (inc (get-in db [:timers id])))))


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
    (-> db
      (assoc :widgets (dissoc (:widgets db) widget-id))
      (assoc :layout (dissoc (:layout db) widget-id)))))


(rf/reg-event-db
  :reset-local
  (fn-traced [db [_ widget-id]]
    (assoc-in db [:widgets widget-id :local] 0)))


(defn- update-layout [db {:keys [i] :as new-layout}]
  (assoc-in db [:layout i] new-layout))


(rf/reg-event-db
  :update-layout
  (fn-traced [db [_ new-layout]]
    (let [cooked (map #(zipmap '(:i :x :y :w :h) %)
                   (map (juxt :i :x :y :w :h) new-layout))]
      (assoc db :layout (zipmap (map :i cooked) cooked)))))

;; endregion


;; region ; Widgets

(defn- make-content [name sub data]
  [:p (str sub " :: " @data)])


(defn- title-bar [name]
  [:div.widget-banner.title-wrapper.grid-toolbar.move-cursor
   [:h3 {:style {:color :black}} name]
   [:button.button {:on-click #(rf/dispatch [:delete-widget name])} "Delete"]])


(defn- base [name content]
  [:div.widget-parent
   (title-bar name)
   [:div.widget.widget-content content]])


(defn- widget [name]
  (log/info "widget OUTER" name)

  (rf/dispatch-sync [:reset-local name])

  (let [widget @(rf/subscribe [:widget name])
        data   (rf/subscribe [:timer (:data widget)])
        local  (rf/subscribe [:local name])]
    (fn []
      (log/info "widget INNER" name)
      (base
        name
        [:div
         [:button.button {:on-click #(rf/dispatch [:reset (:data widget)])} "reset"]
         [:button.button {:on-click #(rf/dispatch [:local name])} "local"]
         [:p (str "local: " @local)]
         (make-content name (:data widget) data)]))))

;; endregion


;; region ; Grid

(defn- simple-grid []
  (log/info "simple-grid OUTER")

  (let [widgets (rf/subscribe [:layout])]
    (fn []
      (log/info "simple-grid INNER")
      [:div.grid {:style {}}
       (doall
         (for [{:keys [i]} @widgets]
           ^{:key i} [widget i]))])))


(def dummy-layout (r/atom [{:i "one" :x 0 :y 0 :w 2 :h 2}
                           {:i "two" :x 0 :y 0 :w 2 :h 2}
                           {:i "three" :x 0 :y 0 :w 2 :h 2}]))
(def dummy-widgets (r/atom [[:div.widget {:key "one" :style {:color :white}} "a"]
                            [:div.widget {:key "two" :style {:color :white}} "b"]
                            [:div.widget {:key "three" :style {:color :white}} "c"]]))
(def dummy-reactive-widgets (r/atom [[:div.widget {:key "one"} [(fn [] [:div {:style {:color :white}} "a"])]]
                                     [:div.widget {:key "two"} [(fn [] [:div {:style {:color :white}} "b"])]]
                                     [:div.widget {:key "three"} [(fn [] [:div {:style {:color :white}} "c"])]]]))


(defn- simple-responsive-grid []
  (log/info "responsive-grid OUTER")

  (let [layout (rf/subscribe [:layout])]
    (fn []
      (log/info "responsive-grid INNER")
      [:div.grid-container
       (into [:> ReactGridLayout
              {:id         "dashboard-widget-grid"
               :class      "layout"
               :layout     @dummy-layout
               :cols       12                               ;{:lg 12 :md 10 :sm 6 :xs 4 :xxs 2};12
               :width      1536
               :row-height 50
               :style      {:height 500}
               ;:breakpoints {:lg 2048 :md 1024 :sm 768 :xs 480 :xxs 0}
               :on-change  #()
               :item-props {:class "widget-component"}}]
         @dummy-reactive-widgets)])))


(defn on-layout-change [new]
  (let [chg (js->clj new :keywordize-keys true)
        fst (first chg)]
    (if (and
          (not (empty? chg))
          (<= 1 (count chg))
          (not= (:i fst) "null"))
      (do
        (rf/dispatch [:update-layout chg])))))


(defn- responsive-grid []
  (log/info "responsive-grid OUTER")

  (let [layout (rf/subscribe [:layout])]
    (fn []
      (log/info "responsive-grid INNER")
      [:div.grid-container
       (into [:> ReactGridLayout
              {:id             "dashboard-widget-grid"
               :class          "layout"
               :layout         @layout
               :cols           12
               :width          1536
               :row-height     50
               :style          {:height 500}
               :onLayoutChange #(on-layout-change %)
               :item-props     {:class "widget-component"}}]

         (doall
           (for [{:keys [i]} @layout]
             ^{:key i} [:div.widget {:key i} [widget i]])))])))

;; endregion


;; region ; main-page

(defn- timers []
  (let [timers (rf/subscribe [:timers])]
    (fn []
      [:div.flex-container
       (doall
         (map (fn [[k t]] ^{:key k}
                [:div {:on-click #(rf/dispatch [:tick-one k])} t])
           @timers))])))


(defn- main-page []
  (log/info "main-page")
  [:div
   [:h3 "Simple Grid"]
   [:button.button {:on-click #(rf/dispatch-sync [:tick])} "Tick!"]
   [:button.button {:on-click #(rf/dispatch [:add-widget "one" "one"])} "Add \"one\""]
   [:button.button {:on-click #(rf/dispatch [:add-widget "two" "two"])} "Add \"two\""]
   [:button.button {:on-click #(rf/dispatch [:add-widget "three" "one"])} "Add \"three\""]
   [timers]
   ;[simple-grid]
   ;[simple-responsive-grid]
   [responsive-grid]])

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

  (contains? (:layout db) "one")


  ())


(comment
  (def db @re-frame.db/app-db)
  (def old-layout (:layout db))
  (def widget-id "one")
  (remove #(= widget-id (:id %)) (:layout db))


  (def layout (rf/subscribe [:layout]))
  (into []
    (for [{:keys [i]} @layout]
      ^{:key i} [:div.widget {:key i}
                 [widget i]]))
  ())


(comment
  (def db @re-frame.db/app-db)
  (def i "one")
  (def new-layout [{:i "one" :x 0 :y 0 :w 2 :h 2}
                   {:i "two" :x 0 :y 0 :w 2 :h 2}])

  (map (fn [x] (:i x)) new-layout)
  (zipmap (map :i new-layout) new-layout)

  (assoc db :layout (zipmap (map :i new-layout) new-layout))

  (map #(zipmap '(:i :x :y :w :h) %)
    (map (juxt :i :x :y :w :h) new-layout))
  ())