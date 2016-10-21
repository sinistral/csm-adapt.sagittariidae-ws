
(ns sagittariidae.ws.core
  (:require [clojure.data.json              :as    json]
            [clojure.string                 :as    s]
            [compojure.core                 :refer [GET PUT defroutes]]
            [compojure.route                :refer [files not-found]]
            [mantle.io                      :refer [fmtstr]]
            [ring.middleware.json           :refer [wrap-json-params]]
            [ring.middleware.params         :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [sagittariidae.ws.db            :refer [cn db] :as db]
            [sagittariidae.ws.liaison       :as    <>]
            [slingshot.slingshot            :refer [try+]]))

;;
;; FIXIT: validate API parameters; cf. https://github.com/metosin/compojure-api
;;

(defroutes routes
  ;; API routes for HTTP verbs.
  (GET "/projects" []
       (json/write-str (<>/get-projects (db))))
  (GET "/methods" []
       (json/write-str (<>/get-methods (db))))
  (GET "/projects/:p/samples" [p q]
       (if (empty? q)
         (json/write-str (<>/get-samples (db) p))
         (throw UnsupportedOperationException)))
  (GET "/projects/:p/samples/:s" [p s]
       (if-let [r (<>/get-sample (db) p s)]
         (json/write-str r)
         (not-found (json/write-str {:project/id p :sample/id s}))))
  (GET "/projects/:p/samples/:s/stages" [p s]
       (let [stages (<>/get-stages (db) p s)]
         (json/write-str {:sample s
                          :stages stages
                          :token (<>/get-stage-token (db) (inc (count stages)))})))
  (PUT "/projects/:p/samples/:s/stages/:t" {params :params}
       (try+
        (let [p (:p params)
              s (:s params)
              t (:t params)
              m (:method params)
              a (:annotation params)]
          (json/write-str {:sample s :stage (<>/add-stage (cn) p s m a t)}))
        (catch [:type :sagittariidae.ws.dbfn/stage-token-conflict] _
          {:status 409                  ; conflict
           :body (json/write-str {:error {:message (:message &throw-context)}})})
        (catch [:type :sagittariidae.ws.liaison/malformed-annotation] _
          {:status 400                  ; client error
           :body (json/write-str {:error {:message (:message &throw-context)}})})))
  ;; Routes for static files and error handlers.
  (files "/" {:root "site"})
  (not-found "404"))

(def service-handler
  (-> routes
      (wrap-keyword-params)
      (wrap-json-params)
      (wrap-params)))

(defn initialize
  []
  (db/initialize)
  nil)
