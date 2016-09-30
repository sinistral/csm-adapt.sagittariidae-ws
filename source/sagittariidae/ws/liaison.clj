
(ns sagittariidae.ws.liaison
  "In which is defined the functionality that liaises between the application
  interface that is exposed by the web service, and the database that maintains
  the state of the entities managed by the web service."
  (:require [clojure.string      :as    s]
            [datomic.api         :as    d]
            [ring.util.codec     :refer [url-encode]]
            [mantle.io           :refer [fmtstr]]
            [sagittariidae.ws.db :refer [tx]]))

;;; ----------------------------------------------------------------------- ;;;

(defn ^{:dynamic true} *resource-not-found*
  [type id]
  (throw
   (ex-info
    (fmtstr "The requested resource ~a:~a could not be found"
            (-> type name s/capitalize)
            id)
    {:type type
     :id   id})))

(defn ^{:dynamic true} *malformed-sample-name*
  [name re]
  (throw
   (ex-info
    (fmtstr "The sample name \"~a\" does not conform to the required pattern ~a"
            name
            (str re))
    {:name    name
     :pattern re})))

(defn- tx-data:add-resource
  ([rtype attrs]
   (tx-data:add-resource (d/tempid :db.part/user) rtype attrs))
  ([eid rtype attrs]
   [[:christen-resource
     (merge {:db/id eid :res/type rtype} attrs)]]))

(defn- tx-data:add-project
  [db name mask]
  (tx-data:add-resource
   :res.type/project {:project/name name
                      :project/sample-mask (str mask)}))

(defn- tx-data:add-method
  [db name desc]
  (tx-data:add-resource
   :res.type/method {:method/name name
                     :method/description desc}))

(declare mk-sample-name)

(defn- tx-data:add-sample
  [db project-id sample-name]
  (let [mask (or (:project/sample-mask
                  (first
                   (d/q '[:find  [(pull ?e [:project/sample-mask])]
                          :in    $ ?p
                          :where [?e :project/obfuscated-id ?p]]
                        db project-id)))
                 (*resource-not-found* :res.type/project project-id))
        name (or (re-matches (re-pattern mask) sample-name)
                 (*malformed-sample-name* sample-name mask))]
    (let [eid (d/tempid :db.part/user)]
      ;; We require sample names to be unique within a project.  In a
      ;; relational database we'd achieve this by creating a UNIQUE index on
      ;; (project-id, sample-id).  Although it is possible to enforce this in
      ;; Datomic using a transaction function, having it enforced by the schema
      ;; guards against bugs in our own code; our schema enforces uniquness
      ;; across all instances of the `:sample/name` attribute, so we
      ;; concatenate the sample name to the project name to obtain the desired
      ;; behaviour.
      (conj (tx-data:add-resource
             eid
             :res.type/sample {:sample/name (mk-sample-name project-id sample-name)})
            {:db/id          [:project/obfuscated-id project-id]
             :project/sample eid}))))

;;; ----------------------------------------------------------------------- ;;;

(defn- untype-attrs
  [kvs]
  (map #(let [[k v] %] [(-> k name keyword) v]) kvs))

(defn- extern-name
  [x]
  (letfn [(esc->- [x]
            (s/replace x #"%.{2}|\/|_" "-"))
          (shrink [x]
            (s/replace x #"-{2,}" "-"))
          (unqual [x]
            (last (s/split x #"\$")))]
    (-> x unqual s/lower-case url-encode esc->- shrink)))

(defn- extern-id
  [m]
  (dissoc (if-let [name (:name m)]
            (assoc m :id (s/join "-" [(:obfuscated-id m) (extern-name name)]))
            (assoc m :id (:obfuscated-id m)))
          :obfuscated-id))

(defn- extern-resource-entity
  "Transform a Datomic entity that represents a Sagittariidae resource into a
  form suitable for presentation to external agents.  In general this means
  stripping out implementation-specific fields and replacing internal
  identifiers with the values that we want the outside world to see."
  [x]
  (let [kvs (-> x
                (dissoc :db/id :res/type)
                (seq)
                (untype-attrs)
                (flatten))]
    (extern-id (apply hash-map kvs))))

(defn- mk-sample-name
  [project-id sample-name]
  (s/join "$" [project-id sample-name]))

;;; ----------------------------------------------------------------------- ;;;

(defn add-project
  [cn name mask]
  (tx cn (tx-data:add-project (d/db cn) name mask)))

(defn get-projects
  [db]
  (map extern-resource-entity
       (d/q '[:find  [(pull ?e [* {:res/type [:db/ident]}]) ...]
              :where [?e :res/type :res.type/project]]
            db)))

(defn add-method
  [cn name desc]
  (tx cn (tx-data:add-method (d/db cn) name desc)))

(defn get-methods
  [db]
  (map extern-resource-entity
       (d/q '[:find  [(pull ?e [* {:res/type [:db/ident]}]) ...]
              :where [?e :res/type :res.type/method]]
            db)))

(defn add-sample
  [cn project-id sample-name]
  (tx cn (tx-data:add-sample (d/db cn) project-id sample-name)))
