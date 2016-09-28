
(ns sagittariidae.ws.core
  (:require [clojure.data.json              :as    json]
            [compojure.core                 :refer [GET defroutes]]
            [compojure.route                :refer [files not-found]]
            [ring.middleware.params         :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [sagittariidae.ws.db            :refer [db] :as db]
            [sagittariidae.ws.liaison       :refer [get-methods get-projects]]))

(defroutes routes
  ;; API routes for HTTP verbs.
  (GET "/projects" []
       (json/write-str (get-projects (db))))
  (GET "/methods" []
       (json/write-str (get-methods (db))))
  ;; Routes for static files and error handlers.
  (files "/" {:root "site"})
  (not-found "404"))

(def service-handler
  (-> routes
      (wrap-keyword-params)
      (wrap-params)))

(defn initialize
  []
  (db/initialize)
  nil)
