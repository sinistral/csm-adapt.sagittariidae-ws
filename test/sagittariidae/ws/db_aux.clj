
(ns sagittariidae.ws.db-aux
  "In which is defined auxiliary functionality to support testing of the DB."
  (:require [clojure.string      :as s]
            [datomic.api         :as d]
            [sagittariidae.ws.db :as db]))

(def db-uri "datomic:mem://sagittariidae-test")

(defn mk-db
  []
  (d/db (db/initialize db-uri)))

(defn rm-db
  []
  (d/delete-database db-uri))

(defn name->obid
  [db t n]
  (let [type-attr (fn [t a] (keyword (s/join "/" [(name t) a])))
        name-attr (type-attr t "name")
        obid-attr (type-attr t "obfuscated-id")]
    (obid-attr (ffirst (d/q '[:find  (pull ?e [*])
                              :in    $ ?a ?n
                              :where [?e ?a ?n]]
                            db name-attr n)))))

(defn speculate
  ([tx]
   (speculate (mk-db) tx))
  ([db tx]
   (:db-after (d/with db tx))))
