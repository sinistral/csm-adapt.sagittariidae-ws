
(ns sagittariidae.ws.liaison
  "In which is defined the functionality that liaises between the application
  interface that is exposed by the web service, and the database that maintains
  the state of the entities managed by the web service."
  (:require [clojure.string      :as    s]
            [clojure.walk        :refer [prewalk]]
            [datomic.api         :as    d]
            [ring.util.codec     :refer [url-encode]]
            [mantle.io           :refer [fmtstr]]
            [sagittariidae.ws.db :refer [tx]]
            [swiss.arrows        :refer :all]))

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

(defn- tx-data:add-annotation
  ([[k v :as a]]
   (tx-data:add-annotation (d/tempid :db.part/user) a))
  ([eid [k v]]
   [{:db/id        eid
     :annotation/k k
     :annotation/v v}]))

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
    (let [sample-eid  (d/tempid :db.part/user)
          sample-attr {:sample/name (mk-sample-name project-id sample-name)}]
      ;; We require sample names to be unique within a project.  In a
      ;; relational database we'd achieve this by creating a UNIQUE index on
      ;; (project-id, sample-id).  Although it is possible to enforce this in
      ;; Datomic using a transaction function, having it enforced by the schema
      ;; guards against bugs in our own code; our schema enforces uniquness
      ;; across all instances of the `:sample/name` attribute, so we
      ;; concatenate the sample name to the project name to obtain the desired
      ;; behaviour.
      (conj (tx-data:add-resource sample-eid :res.type/sample sample-attr)
            {:db/id          [:project/obfuscated-id project-id]
             :project/sample sample-eid}))))

(defn- tx-data:add-stage
  [db sample-id method-id annotations]
  (let [stg-eid   (d/tempid :db.part/user)
        ann-eids  (repeatedly (count annotations) #(d/tempid :db.part/user))
        stg-attrs {:stage/method     [:method/obfuscated-id method-id]
                   :stage/annotation (apply hash-set ann-eids)}]
    (concat
     ;; build the annotation entities
     (mapcat tx-data:add-annotation ann-eids (seq annotations))
     ;; txform for the stage resource
     (tx-data:add-resource stg-eid :res.type/stage stg-attrs)
     ;; link the stage to the sample
     [{:db/id [:sample/obfuscated-id sample-id]
       :sample/stage stg-eid}])))

;;; ----------------------------------------------------------------------- ;;;

(defmulti ^{:private true} untype-attrs
  #(cond (map? %) :map
         (sequential? %) :seq))

(defmethod ^{:private true} untype-attrs :seq
  [kvs]
  (map #(let [[k v] %] [(-> k name keyword) v]) kvs))

(defmethod ^{:private true} untype-attrs :map
  [m]
  (if (seq m)                           ; handle empty maps
    (-<> m seq untype-attrs (apply concat <>) (apply hash-map <>))
    m))

(defn- unqual-name
  [x]
  (last (s/split x #"\$")))

(defn- extern-name
  [x]
  (letfn [(esc->- [x]
            (s/replace x #"%.{2}|\/|_" "-"))
          (shrink [x]
            (s/replace x #"-{2,}" "-"))]
    (-> x unqual-name s/lower-case url-encode esc->- shrink)))

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
  (assert (-> x :res/type :db/ident) "Type entity for attribute has not been expanded; did you forget to expand this in a `pull` expression?")
  (let [ext-name (unqual-name
                  ((-> x
                       :res/type                  ; construct the name of
                       :db/ident                  ; the `name` attribute of
                       name                       ; the entity, and fetch it
                       (#(s/join "/" [% "name"])) ; e.g. `(:project/name x)`
                       keyword) x))]
    (extern-id (-<> x
                    (dissoc :db/id :res/type)
                    (untype-attrs)
                    (assoc :name ext-name)))))

(defn- extern-entity
  "Helper function to eternalise Sagittariidae Datomic entities.  Intended for
  use with `clojure.walk/prewalk` to recursively process structures."
  [e]
  (cond (and (map? e) (contains? e :res/type))
        (extern-resource-entity e)
        (map? e)
        (untype-attrs (dissoc e :db/id))
        :else e))

(defn- externalize
  [x]
  (prewalk extern-entity x))

(defn- mk-sample-name
  [project-id sample-name]
  (s/join "$" [project-id sample-name]))

;;; ----------------------------------------------------------------------- ;;;

(defn add-project
  [cn name mask]
  (tx cn (tx-data:add-project (d/db cn) name mask)))

(defn get-projects
  [db]
  (externalize
   (d/q '[:find  [(pull ?e [:project/id
                            :project/obfuscated-id
                            :project/name
                            :project/sample-mask
                            {:res/type [:db/ident]}]) ...]
          :where [?e :res/type :res.type/project]]
        db)))

(defn add-method
  [cn name desc]
  (tx cn (tx-data:add-method (d/db cn) name desc)))

(defn get-methods
  [db]
  (externalize
   (d/q '[:find  [(pull ?e [:method/id
                            :method/obfuscated-id
                            :method/name
                            :method/description
                            {:res/type [:db/ident]}]) ...]
          :where [?e :res/type :res.type/method]]
        db)))

(defn add-sample
  [cn project-id sample-name]
  (tx cn (tx-data:add-sample (d/db cn) project-id sample-name)))

(defn get-sample
  [db project-id sample-id]
  (let [query '[:find  [(pull ?s [* {:res/type [:db/ident]
                                     :sample/stage [{:stage/method [{:res/type [:db/ident]}
                                                                    :method/id
                                                                    :method/obfuscated-id
                                                                    :method/name]}
                                                    {:stage/annotation [:annotation/k :annotation/v]}]}])]
                :in    $ ?p-id ?s-id
                :where [?e :sample/obfuscated-id ?s-id]
                [?p :project/obfuscated-id ?p-id]
                [?p :project/sample ?s]]]
    (when-let [es (d/q query db project-id sample-id)]
      (externalize (first es)))))

(defn add-stage
  [cn project-id sample-id method-id annotations]
  (tx cn (tx-data:add-stage (d/db cn) sample-id method-id annotations)))
