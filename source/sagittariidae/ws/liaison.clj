
(ns sagittariidae.ws.liaison
  "In which is defined the functionality that liaises between the application
  interface that is exposed by the web service, and the database that maintains
  the state of the entities managed by the web service."
  (:require [clojure.string      :as    s]
            [datomic.api         :as    d]
            [ring.util.codec     :refer [url-encode]]))

;; ------------------------------------------------------------------------- ;;

(defn- tx-data:add-resource
  ([rtype attrs]
   (tx-data:add-resource (d/tempid :db.part/user) rtype attrs))
  ([eid rtype attrs]
   [:christen-resource
    (merge {:db/id eid :res/type rtype} attrs)]))

;; ------------------------------------------------------------------------- ;;

(defn- untype-attrs
  [kvs]
  (map #(let [[k v] %] [(-> k name keyword) v]) kvs))

(defn- extern-name
  [x]
  (letfn [(esc->- [x]
            (s/replace x #"%.{2}|\/|_" "-"))
          (shrink [x]
            (s/replace x #"-{2,}" "-"))]
    (-> x s/lower-case url-encode esc->- shrink)))

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

(defn get-projects
  [db]
  (map extern-resource-entity
       (d/q [:find  '[(pull ?e [* {:res/type [:db/ident]}]) ...]
             :where '[?e :res/type :res.type/project]]
            db)))

(defn get-methods
  [db]
  (map extern-resource-entity
       (d/q [:find  '[(pull ?e [* {:res/type [:db/ident]}]) ...]
             :where '[?e :res/type :res.type/method]]
            db)))
