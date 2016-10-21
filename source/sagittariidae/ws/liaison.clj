
(ns sagittariidae.ws.liaison
  "In which is defined the functionality that liaises between the application
  interface that is exposed by the web service, and the database that maintains
  the state of the entities managed by the web service."
  (:require [backtick            :refer [template]]
            [clojure.string      :as    s]
            [clojure.walk        :refer [prewalk]]
            [datomic.api         :as    d]
            [hashids.core        :as    hashid]
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

(defn- mk-sample-name
  [project-id sample-name]
  (s/join "$" [project-id sample-name]))

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
  ([db sample-id method-id annotations]
   (tx-data:add-stage db sample-id method-id annotations nil))
  ([db sample-id method-id annotations token]
   (let [stg-eid   (d/tempid :db.part/user)
         ann-eids  (repeatedly (count annotations) #(d/tempid :db.part/user))
         stg-attrs {:stage/method     [:method/obfuscated-id method-id]
                    :stage/annotation (apply hash-set ann-eids)}]
     (concat
      (when token [[:validate-stage-token sample-id token]])
      ;; build the annotation entities
      (mapcat tx-data:add-annotation ann-eids (seq annotations))
      ;; txform for the stage resource
      (tx-data:add-resource stg-eid :res.type/stage stg-attrs)
      ;; link the stage to the sample
      [{:db/id [:sample/obfuscated-id sample-id]
        :sample/stage stg-eid}]))))

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
  (when x
    (last (s/split x #"\$"))))

(defn- extern-name
  [x]
  (letfn [(esc->- [x]
            (s/replace x #"%.{2}|\/|_" "-"))
          (shrink [x]
            (s/replace x #"-{2,}" "-"))]
    (-> x unqual-name s/lower-case url-encode esc->- shrink)))

(defn- rn->id
  "Convert a \"resource-name\" into an ID. To make resource IDs more readable,
  we return them with a sanitized version of the name appended.  Clients are
  allowed to use the entire string as an identifier so we must be able to undo
  our own ID munging."
  [x]
  (-> x (s/split #"-" 2) (first)))

(defn- id->rn
  "Construct a human-grokkable label for a resource, a \"resource name\".  This
  name still uniquely identifies the resource, but includes some context that
  makes the label more meaningful to humans."
  [id nm]
  (s/join "-" [id nm]))

(defn- extern-id-in-resource
  "To make IDs grokkable to humans, append a sanitized version of the name to
  construct a \"resource name\"."
  [m]
  (dissoc (if-let [name (:name m)]
            (assoc m :id (id->rn (:obfuscated-id m) (extern-name name)))
            (assoc m :id (:obfuscated-id m)))
          :obfuscated-id))

(defn- extern-resource
  "Transform a Datomic entity that represents a Sagittariidae resource into a
  form suitable for presentation to external agents.  In general this means
  stripping out implementation-specific fields and replacing internal
  identifiers with the values that we want the outside world to see."
  [x]
  (assert (-> x :res/type :db/ident) "Type entity for attribute has not been expanded; did you forget to expand this in a `pull` expression?")
  (let [ext-name (unqual-name
                  ((-> x
                       :res/type                  ; Construct the name of
                       :db/ident                  ; the `name` attribute of
                       name                       ; the entity, and fetch it
                       (#(s/join "/" [% "name"])) ; e.g. `(:project/name x)`
                       keyword) x))]
    (extern-id-in-resource
     (let [ext-res (-<> x (dissoc :db/id :res/type) (untype-attrs))]
       (if ext-name                     ; Not all resources have names.  Some,
         (assoc ext-res :name ext-name) ; like `stage`s have only numeric IDs.
         ext-res)))))

(derive ::resource         ::entity)
(derive :res.type/project  ::resource)
(derive :res.type/method   ::resource)
(derive :res.type/sample   ::resource)
(derive :res.type/stage    ::resource)
(derive :res.type/datafile ::resource)

(defmulti extern
  (fn [x]
    (when (map? x)
      (or (get-in x [:res/type :db/ident])
          ::entity))))

(defmethod extern :default
  [x]
  x)

(defmethod extern ::entity
  [e]
  (untype-attrs (dissoc e :db/id)))

(defmethod extern ::resource
  [r]
  (extern-resource r))

(defmethod extern :res.type/stage
  [stage]
  (let [stage (extern-resource stage)
        method (extern (:method stage))
        annotations (map #(s/join "=" [(:annotation/k %) (:annotation/v %)])
                         (:annotation stage))]
    (as-> stage stage
      (if-not (empty? annotations)
        (assoc stage :annotation (s/join "; " annotations))
        stage)
      (assoc stage :method (:id method)))))

(defn- externalize
  [x]
  (prewalk extern x))

;;; ----------------------------------------------------------------------- ;;;

(def ^{:private true} pull-specs
  {:sample '[* {:res/type [:db/ident]
                :sample/stage [{:stage/method [{:res/type [:db/ident]}
                                               :method/id
                                               :method/obfuscated-id
                                               :method/name]}
                               {:stage/annotation [{:res/type [:db/ident]} :annotation/k :annotation/v]}]}]
   :stage '[* {:res/type [:db/ident]
               :stage/method [{:res/type [:db/ident]}
                              :method/name
                              :method/obfuscated-id]
               :stage/annotation [{:res/type [:db/ident]} :annotation/k :annotation/v]}]})

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
  (tx cn (tx-data:add-sample (d/db cn) (rn->id project-id) sample-name)))

(defn get-sample
  ([db sample-id]
   (get-sample db nil sample-id))
  ([db project-id sample-id]
   (let [q (template
            [:find  [(pull ?s ~(:sample pull-specs))]
             :in    ~@(keep identity ['$ (when project-id '?p-id) '?s-id])
             :where ~@(let [base '[[?s :sample/obfuscated-id ?s-id]]]
                        (if project-id
                          (conj base
                                '[?p :project/obfuscated-id ?p-id]
                                '[?p :project/sample ?s])
                          base))])
         a (keep identity [q db project-id (rn->id sample-id)])]
     (when-let [es (apply d/q a)]
       (externalize (first es))))))

(defn get-samples
  [db project-id]
  (externalize
   (d/q (template
         [:find  [(pull ?s ~(:sample pull-specs)) ...]
          :in    $ ?p-id
          :where [?p :project/obfuscated-id ?p-id]
          [?p :project/sample ?s]])
        db (rn->id project-id))))

(defn- -add-stage
  "The side-effecting part of `add-stage`, extracted for testability.  Returns
  a DB with the changes applied."
  [cn project-id sample-id method-id annotations token]
  (tx cn (tx-data:add-stage (d/db cn)
                            (rn->id sample-id)
                            (rn->id method-id)
                            annotations
                            token)))

(defn ^{:dynamic true} *malformed-annotation*
  ([a]
   (*malformed-annotation* a nil))
  ([a cause]
   (throw
    (ex-info
     (fmtstr "The annotation ~s is not a valid key-value pair (e.g. \"x=y\")" a)
     {:type ::malformed-annotation :annotation a}
     cause))))

(defn- parse-annotations
  "Parse a string that contains annotations in key-value pairs into its
  constituent parts."
  [s]
  (if-not (empty? s)
    (try
      (letfn [(parse-annotation [s]
                (if-let [groups (re-seq #"([^=]+)=([^=]+)" s)]
                  (->> groups first rest (map s/trim))
                  (*malformed-annotation* s)))]
        (apply hash-map
               (apply concat
                      (map parse-annotation (s/split s #";")))))
      (catch Throwable t
        (*malformed-annotation* s t)))
    {}))

(defn add-stage
  ([cn project-id sample-id method-id annotations]
   (add-stage cn project-id sample-id method-id annotations nil))
  ([cn project-id sample-id method-id annotations token]
   (let [am (parse-annotations annotations)
         db (-add-stage cn project-id sample-id method-id am token)]
     (->> (d/q '[:find  [(max ?n)]
                 :where [_ :stage/id ?n]]
               db)
          (first)
          (d/q '[:find  [?stage]
                 :in    $ ?stage-id
                 :where [?stage :stage/id ?stage-id]]
               db)
          (first)
          (d/pull db (:stage pull-specs))
          (externalize)))))

(defn- get-stage-hashid-opts
  [db]
  (zipmap [:salt :min-length]
          (d/q '[:find  [?s ?l]
                 :where [?e :res-archetype/type :res.type/stage-token]
                        [?e :res-archetype/hashid-salt ?s]
                        [?e :res-archetype/hashid-length ?l]]
               db)))

(defn get-stage-token
  [db n]
  (hashid/encode (get-stage-hashid-opts db) n))

(defn get-stages
  [db project-id sample-id]
  (map
   ;; Unlike other resources, a stage has no name.  So we construct a name
   ;; based on the position of the stage in the sequence.
   #(assoc %1 :id (s/join "-" [(:id %1) (format "%03d" (inc %2))]))
   (externalize (sort #(compare (:stage/id %1) (:stage/id %2))
                      (d/q (template
                            [:find  [(pull ?stage ~(:stage pull-specs)) ...]
                             :in    $ ?s-id
                             :where [?sample :sample/obfuscated-id ?s-id]
                             [?sample :sample/stage ?stage]])
                           db (rn->id sample-id))))
   (range)))
