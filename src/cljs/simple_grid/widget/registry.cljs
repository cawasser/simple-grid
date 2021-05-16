(ns simple-grid.widget.registry)


(def registry (atom {}))


(defn register [{:keys [type] :as widget}]
  (swap! registry assoc type widget))
