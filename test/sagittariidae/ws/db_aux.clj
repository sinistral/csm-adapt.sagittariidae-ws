
(ns sagittariidae.ws.db-aux
  "In which is defined auxiliary functionality to support testing of the DB."
  (:require [datomic.api         :as d]
            [sagittariidae.ws.db :as db]))

(def db-uri "datomic:mem://sagittariidae-test")

(defn mk-db
  []
  (d/db (db/initialize db-uri)))

(defn speculate
  ([tx]
   (speculate (mk-db) tx))
  ([db tx]
   (:db-after (d/with db tx))))
