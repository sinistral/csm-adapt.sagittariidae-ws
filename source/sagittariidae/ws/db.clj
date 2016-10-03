
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

(def ^{:private true} db-uri
  (str/join "/" [transactor-uri-base db-name]))

(defonce ^{:private true} conn
  (atom nil))

(defn- dbfns
  "Retrieves from a namespace all Vars that have been annotated as a `dbfn`,
  i.e. those that have `{:dbfn true}` in their metadata map."
  [ns-sym]
  (filter #(:dbfn (meta %)) (vals (ns-interns (find-ns ns-sym)))))

(defn- install-dbfns
  [cn ns]
  (d/transact cn (map (fn [v]
                        (let [m (meta v)]
                          {:db/id    (d/tempid :db.part/user)
                           :db/doc   (:doc m)
                           :db/ident (keyword (:name m))
                           :db/fn    @v}))
                      (dbfns ns))))

(defn- create-db
  [uri schema data]
  (let [created? (d/create-database uri)
        cn       (d/connect uri)]
    (when created?
      ;; Install our database functions, schema and data, dereferencing the
      ;; result each time to raise any exceptions that may have occurred.
      @(install-dbfns cn 'sagittariidae.ws.dbfn)
      @(d/transact cn (read-string (slurp schema)))
      @(d/transact cn (read-string (slurp data))))
    cn))

(defn- resource-reader
  [r]
  (-> r jio/resource jio/reader))

;; ------------------------------------------------------------------------- ;;

(defn cn
  "Returns the connection for the 'default' database; will be `nil` if the
  no-args form of `initialize` has not been called."
  []
  @conn)

(defn db
  "Returns an instance of the 'default' database; will be `nil` if the no-args
  form of `initialize` has not been called."
  []
  (when-let [c (cn)]
    (d/db c)))

(defn initialize
  "Initialize the Sagittariidae database and return a connection to it.  The
  no-args form of this function will also initialize the 'default' database
  used by `(cn)` and `(db)`.

  If the form that requires the `db-uri` is used, it is assumed that the
  database connection will be managed by the caller, and the 'default' database
  instance is not affected."
  ([]
   (swap! conn (fn [_] (initialize db-uri))))
  ([db-uri]
   (create-db
    db-uri
    (resource-reader db-schema-file)
    (resource-reader db-initial-data-file))))

(defn tx
  ([tx-data]
   (tx (cn) tx-data))
  ([cn tx-data]
   (:db-after @(d/transact cn tx-data))))
