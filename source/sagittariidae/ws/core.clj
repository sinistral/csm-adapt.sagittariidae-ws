
(ns sagittariidae.ws.core
  (require [compojure.core
            :refer [GET defroutes]]
           [compojure.route
            :refer [not-found]]
           [ring.middleware.params
            :refer [wrap-params]]
           [ring.middleware.keyword-params
            :refer [wrap-keyword-params]]))

(defn index
  []
  (str "Hello, world"))

(defroutes routes
  (GET "/" [] (index))
  (not-found "404"))

(def service-handler
  (-> routes
      (wrap-keyword-params)
      (wrap-params)))
