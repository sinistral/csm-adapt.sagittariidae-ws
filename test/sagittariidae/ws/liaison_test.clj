
(ns sagittariidae.ws.liaison-test
  (:require [clojure.test             :refer [deftest is testing]]
            [sagittariidae.ws.db      :as    db]
            [sagittariidae.ws.db-aux  :refer [mk-db speculate]]
            [sagittariidae.ws.liaison :as    l]))

(def extern-name #'l/extern-name)

(deftest test:extern-name
  (is (apply = "foo-bar"
             (map extern-name
                  ["Foo Bar" "Foo   Bar" "Foo$Bar" "Foo$$$Bar" "foo|bar"])))
  (is (= "f-o-o" (extern-name "f o o"))))

(def untype-attrs #'l/untype-attrs)

(deftest test:untype-attrs
  (is (= [[:foo 0] [:bar 1]]
         (untype-attrs [[:ns0/foo 0] [:ns1/bar 1]]))))

(def extern-id #'l/extern-id)

(deftest test:extern-id
  (is (= {:id "f00-bar" :name "Bar"}
         (extern-id {:id 0 :obfuscated-id "f00" :name "Bar"})))
  (is (= {:id "f00"}
         (extern-id {:id 0 :obfuscated-id "f00"}))))

(def extern-resource-entity #'l/extern-resource-entity)

(deftest test:extern-resource-entity
  (is (= {:id "f00-bar" :name "Bar" :attr0 "nil" :attr1 "one"}
         (extern-resource-entity {:thing/id            0
                                  :thing/obfuscated-id "f00"
                                  :thing/name          "Bar"
                                  :thing/attr0         "nil"
                                  :other-thing/attr1   "one"}))))

(def tx-data:add-resource #'l/tx-data:add-resource)

(deftest test:get-projects
  (let [db (-> (mk-db)
               (speculate [(tx-data:add-resource
                            :res.type/project {:project/name "p1"})])
               (speculate [(tx-data:add-resource
                            :res.type/project {:project/name "p2"})])
               (speculate [(tx-data:add-resource
                            :res.type/method {:project/name "m1"})]))]
                                        ; This isn't a legal construction, but
                                        ; it serves to illustrate that we find
                                        ; projects based on their "type", and
                                        ; not by other assigned attributes.
    (is (= [{:name "p1", :id "PqrX9-p1"} {:name "p2", :id "84z39-p2"}]
           (l/get-projects db)))))

(deftest test:get-methods
  (let [db (-> (mk-db)
               (speculate [(tx-data:add-resource
                            :res.type/project {:method/name "p1"})])
               (speculate [(tx-data:add-resource
                            :res.type/method {:method/name "m1"})])
               (speculate [(tx-data:add-resource
                            :res.type/method {:method/name "m2"})]))]
                                        ; This isn't a legal construction, but
                                        ; it serves to illustrate that we find
                                        ; methods based on their "type", and
                                        ; not by other assigned attributes.
    (is (= [{:name "m1", :id "XZOQ0-m1"} {:name "m2", :id "Xd9k2-m2"}]
           (l/get-methods db)))))
