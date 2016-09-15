
(ns sagittariidae.ws.db
  "In which are defined the mechansims by which the Sagittariidae web service
  interacts with its Datomic transactor; cf. http://datomic.com/"
  (:require [clojure.java.io :as jio]
            [clojure.string  :as str]
            [datomic.api     :as d]))

;; FIXIT: Move to config.
(def ^{:private true} transactor-uri-base
  "datomic:dev://localhost:4334")

(def ^{:private true} db-schema-file
  "sagittariidae-schema.edn")

(def ^{:private true} db-name
  "sagittariidae")

(def db-uri
  (str/join "/" [transactor-uri-base db-name]))

(def ^{:private true} cn
  (atom nil))

(defn- create-db
  [uri rdr]
  (let [created? (d/create-database uri)]
    (swap! cn (fn [_] (d/connect uri)))
    (when created?
      @(d/transact @cn (read-string (slurp rdr))))
    (d/db @cn)))

;; ------------------------------------------------------------------------- ;;

(defn initialize
  []
  (create-db db-uri (-> db-schema-file jio/resource jio/reader)))
