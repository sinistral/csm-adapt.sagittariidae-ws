
(ns sagittariidae.ws.db-test
  (:require [clojure.test             :refer [deftest is testing]]
            [datomic.api              :as    d]
            [sagittariidae.ws.db      :as    db]
            [sagittariidae.ws.db-aux  :refer [mk-db speculate]]
            [sagittariidae.ws.liaison]))

(def tx-data:add-resource #'sagittariidae.ws.liaison/tx-data:add-resource)

(deftest test:rename-project
  (let [db1 (-> (mk-db)
                (speculate (tx-data:add-resource
                            :res.type/project {:project/name "p1"})))
        en1 (d/entity db1 [:project/name "p1"])
        db2 (speculate db1 [{:db/id (:db/id en1) :project/name "p1.1"}])
        en2 (d/entity db2 [:project/name "p1.1"])]
    (is (= en1 en2))
    (is (= "p1.1" (:project/name en2)))))
