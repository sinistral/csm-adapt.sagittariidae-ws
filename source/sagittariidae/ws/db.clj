
(ns sagittariidae.ws.db
  "In which are defined the mechansims by which the Sagittariidae web service
  interacts with its Datomic transactor; cf. http://datomic.com/"
  (:require [clojure.java.io       :as jio]
            [clojure.string        :as str]
            [datomic.api           :as d]
            [sagittariidae.ws.dbfn]))

;; FIXIT: Move to config.
(def ^{:private true} transactor-uri-base
  "datomic:dev://localhost:4334")

(def ^{:private true} db-schema-file
  "db/schema.edn")

(def ^{:private true} db-initial-data-file
  "db/data0.edn")

(def ^{:private true} db-name
  "sagittariidae")

(def ^{:private true :dynamic true} db-uri
  (str/join "/" [transactor-uri-base db-name]))

(def ^{:private true} cn
  (atom nil))

(defn- dbfns
  "Retrieves from a namespace all Vars that have been annotated as a `dbfn`,
  i.e. those that have `{:dbfn true}` in their metadata map."
  [ns-sym]
  (filter #(:dbfn (meta %)) (vals (ns-interns (find-ns ns-sym)))))

(defn- create-db
  [uri schema data]
  (let [created? (d/create-database uri)]
    (swap! cn (fn [_] (d/connect uri)))
    (when created?
      ;; Install our database functions, schema and data, dereferencing the
      ;; result each time to raise any exceptions that may have occurred.
      @(d/transact @cn (map (fn [v]
                              (let [m (meta v)]
                                {:db/id    (d/tempid :db.part/user)
                                 :db/doc   (:doc m)
                                 :db/ident (keyword (:name m))
                                 :db/fn    @v}))
                            (dbfns 'sagittariidae.ws.dbfn)))
      @(d/transact @cn (read-string (slurp schema)))
      @(d/transact @cn (read-string (slurp data))))
    (d/db @cn)))

(defn- resource-reader
  [r]
  (-> r jio/resource jio/reader))

;; ------------------------------------------------------------------------- ;;

(defn db
  []
  (d/db @cn))

(defn initialize
  []
  (create-db
   db-uri
   (resource-reader db-schema-file)
   (resource-reader db-initial-data-file)))

(defn tx
  [tx-data]
  (:db-after @(d/transact @cn tx-data)))

(defn tx-data:add-resource
  [rtype attrs]
  [:christen-resource
   (merge {:db/id (d/tempid :db.part/user) :res/type rtype} attrs)])
