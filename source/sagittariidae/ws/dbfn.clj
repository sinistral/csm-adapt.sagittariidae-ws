
(ns sagittariidae.ws.dbfn
  "In which are defined functions that will be exected within the Datomic
  transactor; cf http://docs.datomic.com/database-functions.html"
  (:require [datomic.api :as d]))

(defmacro ^{:private true} defdbfn
  [fname doc-string opts args & body]
  (let [reqs (or (:requires opts) [])]
    `(def ~(vary-meta fname assoc :private true :dbfn true)
       ~doc-string
       (d/function {:lang     :clojure
                    :requires (quote ~reqs)
                    :params   (quote ~args)
                    :code     (quote (do ~@body))}))))

(defdbfn get-hashid-opts
  "Retrieve the hash ID options for the named resource."
  {}
  [db t]
  (zipmap [:salt :min-length]
          (d/q '[:find  [?s ?l]
                 :in    $ ?t
                 :where [?e :res-archetype/type ?t]
                        [?e :res-archetype/hashid-salt ?s]
                        [?e :res-archetype/hashid-length ?l]]
               db t)))

(defdbfn christen-resource
  "Assign an external identity to a new resource.  By this name shall it be
  known to agents beyond the Sagittariidae API."
  ;; The downside to allocating the primary identifier in a transaction
  ;; function is that because we inspect the DB in order to allocate the ID,
  ;; only one resource of any type can be added in each transaction.  This is
  ;; because *every* `:db/add` in a transaction sees the same value of the DB.
  ;; Thus, an attempt to add multiple resources of the same type will result in
  ;; each being allocated the *same* numeric ID.
  ;;
  ;; This MUST NOT happen, and we depend on the schema enforcing uniqueness to
  ;; prevent it and to prevent Datomic's 'upsert' functionality from folding
  ;; what should be the addition of a new resource into an update of an
  ;; existing resource.
  {:requires [[clojure.string :as str]
              [datomic.api :refer [entity q]]
              [hashids.core :refer [encode]]]}
  [db m]
  (let [e-type
        (:res/type m)
        [n-attr o-attr]
        (map #(keyword (str/join "/" [(name e-type) %])) ["id" "obfuscated-id"])
        max-id
        (ffirst (q '[:find  (max ?n)
                     :in    $ ?t ?a
                     :where [?e :res/type ?t]
                     [?e ?a ?n]]
                   db e-type n-attr))
        new-id
        (inc (or max-id 0))
        get-hashid-opts
        (-> (entity db :get-hashid-opts) :db/fn)]
    ;; FIXIT: It must not be possible to explicitly set either
    ;; the `id` or the `obfuscated-id`, and these must not
    ;; already be set.  We can (and do) override them, but it
    ;; may be confusing to the user to an entity created with
    ;; different IDs.  Better to fail-fast (since in this case
    ;; its actually a programming error).
    [(merge m {:db/id (:db/id m)
               n-attr (biginteger new-id) ; [1]
               o-attr (encode (get-hashid-opts db e-type) new-id)})]))

;; [1] We shouldn't need to do this, but there's a bug in Datomic's
;;     serialisation of Clojure's BigInt type.
;;     cf. https://groups.google.com/forum/#!topic/datomic/QpGHICkLMzQ

(defdbfn validate-stage-token
  "Add a new stage, validating that the stage token (if present) matches what
  we expect the \"next\" token to be."
  {:requires [[clojure.pprint :refer [cl-format]]
              [datomic.api    :refer [entity invoke q]]
              [hashids.core   :refer [encode]]]}
  [db sample-id token]
  (let [stage-count
        (or (first (q '[:find  [(count ?stage)]
                        :where [?sample :sample/obfuscated-id ?sample-id]
                        [?sample :sample/stage ?stage]
                        [?stage :res/type :res.type/stage]]
                      db sample-id))
            0)
        get-hashid-opts
        (-> (entity db :get-hashid-opts) :db/fn)
        required-token
        (encode (get-hashid-opts db :res.type/stage-token) (inc stage-count))]
    (when-not (= token required-token)
      (throw (ex-info
              (cl-format nil "Stage token mismatch; required ~s, received ~s" required-token token)
              {:type           ::stage-token-conflict
               :required-token required-token
               :received-token token
               :sample         sample-id
               :stage-count    stage-count})))))
