
(ns sagittariidae.ws.core
  (:require [clojure.data.json              :as    json]
            [clojure.string                 :as    s]
            [compojure.core                 :refer [GET defroutes]]
            [compojure.route                :refer [files not-found]]
            [ring.middleware.params         :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [sagittariidae.ws.db            :refer [db] :as db]
            [sagittariidae.ws.liaison       :as    <>]))

(defroutes routes
  ;; API routes for HTTP verbs.
  (GET "/projects" []
       (json/write-str (<>/get-projects (db))))
  (GET "/methods" []
       (json/write-str (<>/get-methods (db))))
  (GET "/projects/:p/samples" [p q]
       (json/write-str {:p p :q (s/split q #" ")}))
  (GET "/projects/:p/samples/:s" [p s]
       (if-let [r (<>/get-sample (db) p s)]
         (json/write-str r)
         (not-found (json/write-str {:project/id p :sample/id s}))))
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
