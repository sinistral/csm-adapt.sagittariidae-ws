
(ns sagittariidae.ws.dbfn-test
  (:require [clojure.test             :refer [deftest is testing]]
            [datomic.api              :as    d]
            [sagittariidae.ws.db-aux  :refer [mk-db speculate]]
            [sagittariidae.ws.liaison]))


(def tx-data:add-resource #'sagittariidae.ws.liaison/tx-data:add-resource)

(deftest test:christen-first-entity
  (let [tx [(tx-data:add-resource :res.type/project {:project/name "p1"})]
        en (d/entity (speculate tx) [:project/name "p1"])]
    (is (= 1 (:project/id en)))
    (is (= "PqrX9" (:project/obfuscated-id en)))))

(deftest test:christen-second-entity
  (let [db (-> (mk-db)
               (speculate [(tx-data:add-resource
                            :res.type/project {:project/name "p1"})])
               (speculate [(tx-data:add-resource
                            :res.type/project {:project/name "p2"})]))]
    (is (= 2 (count (d/q '[:find [?e ...] :where [?e :project/name]] db))))
    (is (= [[1 "PqrX9"] [2 "84z39"]]
           (map #(let [en (d/entity db [:project/name %])]
                   (list (:project/id en) (:project/obfuscated-id en)))
                ["p1" "p2"])))))

(deftest test:christen-resources
  (testing "ID values are not globally unique"
    (let [db (-> (mk-db)
                 (speculate [(tx-data:add-resource
                              :res.type/project {:project/name "p1"})
                             (tx-data:add-resource
                              :res.type/method {:method/name "m1"})]))]
      (is (= 1 (:project/id (d/entity db [:project/name "p1"]))))
      (is (= 1 (:method/id (d/entity db [:method/name "m1"])))))))

(deftest test:christen-duplicate-project
  (testing "two projects mayn't have the same name"
    (is (thrown-with-msg? java.lang.IllegalStateException
            #"Unique conflict"
          (-> (mk-db)
              (speculate [(tx-data:add-resource
                           :res.type/project {:project/name "p1"})])
              (speculate [(tx-data:add-resource
                           :res.type/project {:project/name "p1"})])))))
  (testing "two projects mayn't have the same numeric ID"
    (is (thrown-with-msg? java.lang.IllegalArgumentException
            #"datoms in the same transaction conflict"
          (-> (mk-db)
              (speculate
               (map #(tx-data:add-resource
                      :res.type/project {:project/name %})
                    ["p1" "p2"])))))))
