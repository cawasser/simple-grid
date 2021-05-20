(ns simple-grid.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [taoensso.timbre :as log]
            [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
            ["react-grid-layout" :refer (Responsive) :as ReactGridLayout]
            [simple-grid.widget.registry :as registry]
            [simple-grid.widget.alpha]
            [simple-grid.widget.beta]))

;; region ; DEFAULTS

(def default-db {:widgets {}
                 :layout {}
                 :timers {"one" 0 "two" 0}})

(def default-layout {:x 0 :y 0 :w 2 :h 3})

(def starting-widgets {"one"  {:name   "one" :type "alpha" :data "one" :local 0
                               :layout {:i "one" :x 0 :y 0 :w 2 :h 3}}
                       "four" {:name   "four" :type "alpha" :data "two" :local 0
                               :layout {:i "four" :x 4 :y 0 :w 2 :h 3}}})

(def widget-store (atom starting-widgets))

;; endregion


;; region ; Subscriptions

(rf/reg-sub
  :widgets
  (fn [db _]
    (:widgets db)))


(rf/reg-sub
  :layout
  (fn [db _]
    (or (vals (:layout db)) [])))


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
  :renders
  (fn [db _]
    (:renders db)))


(rf/reg-sub
  :local
  (fn [db [_ widget-id]]
    (get-in db [:widgets widget-id :local])))

;; endregion


;; region ; Event Support

(defn- save-layout [db]
  (into {}
    (map (fn [[k v]]
           (let [widget (get-in db [:widgets k])]
             {k (assoc widget :layout v)}))
      (:layout db))))

;; endregion


;; region ; Event Handlers

(rf/reg-event-db
  :init
  (fn-traced [db [_ data]]
    (merge db data)))


(rf/reg-event-db
  :reset-db
  (fn-traced [db [_ data]]
    (merge db default-db)))


(rf/reg-event-db
  :add-widget
  (fn-traced [db [_ name data widget-def]]
    (let [locals (:config widget-def)
          type (:type widget-def)]
      (-> db
        (#(if (contains? (:layout db) name)
            %
            (-> %
              (assoc-in [:layout name] (assoc default-layout :i name))
              (assoc-in [:widgets name] (merge locals
                                          {:name name :title name :type type :data data}))
              ((fn [x] (if (contains? (:timers db) data)
                         x
                         (assoc-in x [:timers data] 0)))))))))))


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
  :re-title
  (fn-traced [db [_ name]]
    (let [new-val (str (rand 30))
          old-val (get-in db [:widgets name :title])]
      (log/info ":re-title" name old-val new-val)
      (assoc-in db [:widgets name :title] new-val))))


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


(rf/reg-event-db
  :load-layout
  (fn-traced [db [_ data]]
    (let [widgets (into {} (map (fn [[k v]] {k (dissoc v :layout)}) data))
          layout  (into {} (map (fn [[k v]] {k (:layout v)}) data))]
      (-> db
        (assoc :widgets widgets)
        (assoc :layout layout)))))


(rf/reg-event-fx
  :save-layout
  (fn-traced [{:keys [db]} _]
    (let [ret (save-layout db)]
      (log/info ":save" ret)
      (reset! widget-store ret)
      {:db db})))


;; endregion


;; region ; Widgets

(defn- make-content
  "FORM-1"
  [widget]
  (let [type     (:type widget)
        build-fn (get-in @registry/registry [type :build-fn])]
    (build-fn widget)))


(defn- title-bar
  "FORM-1"
  [widget]
  (log/info "title-bar" (:name widget))

  (let [widget-id (:name widget)
        title (:title widget)]
    [:div.widget-banner.title-wrapper.grid-toolbar.move-cursor
     [:h3 {:style {:color :black}} title]
     [:button.button {:on-click #(rf/dispatch [:delete-widget widget-id])} "Delete"]
     [:button.button {:on-click #(rf/dispatch [:re-title widget-id])} "Title"]]))


(defn- base
  "FORM-1"
  [widget]
  (log/info "base" widget)

  [:div.widget-parent
   (title-bar widget)
   [:div.widget.widget-content
    (make-content widget)]])


(defn- widget
  "FORM-2 on [:widget name]"
  [name]
  (log/info "widget OUTER" name)

  (let [widget (rf/subscribe [:widget name])]
    (fn []
      (log/info "widget INNER" @widget)
      (base @widget))))


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
(defn- dummy-make-widget [name]
  (fn []
    [:div {:style {:color :white}} name]))
(def dummy-reactive-widgets (r/atom ["a" "b" "c"]))


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
               :on-change  #()
               :item-props {:class "widget-component"}}]
         (doall
           (for [w @dummy-reactive-widgets]
             [:div.widget {:key w} [dummy-make-widget w]])))])))


(defn on-layout-change [new]
  (let [chg (js->clj new :keywordize-keys true)
        fst (first chg)]
    (if (and
          (not (empty? chg))
          (<= 1 (count chg))
          (not= (:i fst) "null"))
      (do
        (rf/dispatch [:update-layout chg])))))


(defn- responsive-grid
  "FORM-2 on :layout"
  []
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
      [:div#timers.flex-container
       [:button.button {:on-click #(rf/dispatch-sync [:tick])} "Tick!"]
       (doall
         (map (fn [[k t]] ^{:key k}
                [:div {:on-click #(rf/dispatch [:tick-one k])} t])
           @timers))])))


(defn- main-page []
  (log/info "main-page")
  [:div
   [:h3 "Simple Grid"]
   [:div
    [:button.button {:on-click #(rf/dispatch-sync [:load-layout @widget-store])} "LOAD"]
    [:button.button {:on-click #(rf/dispatch-sync [:save-layout])} "SAVE"]]
   [:div
    [:button.button
     {:on-click #(rf/dispatch
                   [:add-widget "one" "one" (get @registry/registry "alpha")])}
     "Add \"one\""]
    [:button.button
     {:on-click #(rf/dispatch
                   [:add-widget "two" "two" (get @registry/registry "alpha")])}
     "Add \"two\""]
    [:button.button
     {:on-click #(rf/dispatch
                   [:add-widget "three" "one" (get @registry/registry "beta")])}
     "Add \"three\""]]
   [timers]
   ;[simple-grid]])
   ;[simple-responsive-grid]
   [responsive-grid]])

;; endregion


(defn- ^:dev/after-load-async mount-components []

  (rd/render #'main-page (.getElementById js/document "app")))


(defn main []
  (log/info "Running!")

  (rf/dispatch-sync [:init default-db])
  (mount-components))


;; region ; rich comments
(comment
  @re-frame.db/app-db
  (rf/dispatch-sync [:add-widget "two" "alpha" "two"])

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
  (rf/dispatch [:add-widget "two" "alpha" "two"])
  (rf/dispatch [:add-widget "three" "alpha" "two"])
  (rf/dispatch [:reset "one"])
  (rf/dispatch [:reset "two"])


  (rf/dispatch [:delete-widget "two"])

  (contains? (:layout db) "one")


  ())


; layout stuff
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


; zipmapping when the layout changes
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


; refactoring :add-widget to include :name
(comment
  (rf/dispatch [:add-widget "two" "alpha" "two"])

  (def db @re-frame.db/app-db)
  (def name "two")
  (def data "two")

  (assoc-in db [:layout name] (assoc default-layout :i name))
  (assoc-in db [:widgets name] {:name name :data data :local 0})

  (-> db
    (#(if (contains? (:layout db) name)
        %
        (-> %
          (assoc-in [:layout name] (assoc default-layout :i name))
          (assoc-in [:widgets name] {:name name :data data :local 0})
          ((fn [x] (if (contains? (:timers db) data)
                     x
                     (assoc-in x [:timers data] 0))))))))

  ())


; play with make-content
(comment
  @re-frame.db/app-db
  (def name "one")
  (def widget @(rf/subscribe [:widget name]))

  (def type (:type widget))
  (get-in @registry/registry [type :build-fn])

  (def data (rf/subscribe [:timer (:data @widget)]))
  (def local (rf/subscribe [:local i]))

  (:data @widget)
  [:div
   [:button.button {:on-click #(rf/dispatch [:reset (:data @widget)])} "reset"]
   [:button.button {:on-click #(rf/dispatch [:local i])} "local"]
   [:p (str "local: " @local)]
   [:p (str (:data @widget) " :: " @data)]]
  ())


; load and save, splitting/combining :layout and :widgets
(comment
  (def db @re-frame.db/app-db)
  (def data starting-widgets)
  ; load
  (let [widgets (into {} (map (fn [[k v]] {k (dissoc v :layout)}) data))
        layout  (into {} (map (fn [[k v]] {k (:layout v)}) data))]
    (-> db
      (assoc :widgets widgets)
      (assoc :layout layout)))


  ; save
  (def db @re-frame.db/app-db)
  (into {}
    (map (fn [[k v]]
           (let [widget (get-in db [:widgets k])]
             {k (assoc widget :layout v)}))
      (:layout db)))

  (rf/dispatch-sync [:save])


  ())


(comment
  (rf/dispatch-sync [:init default-db])
  (:widgets @re-frame.db/app-db)
  (rf/dispatch [:add-widget "one" "one" (get @registry/registry "alpha")])
  (rf/dispatch [:add-widget "three" "one" (get @registry/registry "beta")])
  (rf/dispatch [:add-widget "ten" "ten" (get @registry/registry "beta")])

  (def widget-def (get @registry/registry "alpha"))
  (def locals (:config widget-def))
  (merge locals
    {:name name :type type :data data})

  ())

;; endregion