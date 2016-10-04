
(ns sagittariidae.ws.liaison-test
  (:require [clojure.string           :as    s]
            [clojure.test             :refer [deftest is testing]]
            [datomic.api              :as    d]
            [sagittariidae.ws.db      :as    db]
            [sagittariidae.ws.db-aux  :refer [mk-db speculate]]
            [sagittariidae.ws.liaison :as    <>]))

(deftest test:extern-name
  (loop [x ["Foo Bar" "Foo   Bar" "Foo#Bar" "Foo###Bar" "foo|bar" "ns$foo-bar"]]
    (when x
      (do
        (is (= "foo-bar" (#'<>/extern-name (first x))))
        (recur (next x)))))
  (is (= "f-o-o" (#'<>/extern-name "f o o"))))

(deftest test:untype-attrs
  (is (= [[:foo 0] [:bar 1]]
         (#'<>/untype-attrs [[:ns0/foo 0] [:ns1/bar 1]]))))

(deftest test:extern-id
  (is (= {:id "f00-bar" :name "Bar"}
         (#'<>/extern-id {:id 0 :obfuscated-id "f00" :name "Bar"})))
  (is (= {:id "f00"}
         (#'<>/extern-id {:id 0 :obfuscated-id "f00"}))))

(def extern-resource-entity #'<>/extern-resource-entity)

(deftest test:extern-resource-entity
  (is (= {:id "f00-bar" :name "Bar" :attr0 "nil" :attr1 "one"}
         (extern-resource-entity {:res/type            {:db/ident :res.type/thing}
                                  :thing/id            0
                                  :thing/obfuscated-id "f00"
                                  :thing/name          "Bar"
                                  :thing/attr0         "nil"
                                  :other-thing/attr1   "one"}))))

(deftest test:get-projects
  (let [db (as-> (mk-db) db
             (speculate db (#'<>/tx-data:add-project db "p1" ""))
             (speculate db (#'<>/tx-data:add-project db "p2" ""))
             (speculate db (#'<>/tx-data:add-resource
                            :res.type/method {:project/name "m1"})))]
                                        ; This isn't a legal construction, but
                                        ; it serves to illustrate that we find
                                        ; projects based on their "type", and
                                        ; not by other assigned attributes.
    (is (= [{:name "p1", :id "PqrX9-p1" :sample-mask ""}
            {:name "p2", :id "84z39-p2" :sample-mask ""}]
           (<>/get-projects db)))))

(deftest test:get-methods
  (let [db (as-> (mk-db) db
             (speculate db (#'<>/tx-data:add-resource
                            :res.type/project {:method/name "p1"}))
                                        ; This isn't a legal construction, but
                                        ; it serves to illustrate that we find
                                        ; methods based on their "type", and
                                        ; not by other assigned attributes.
             (speculate db (#'<>/tx-data:add-method db "m1" ""))
             (speculate db (#'<>/tx-data:add-method db "m2" "")))]
    (is (= [{:name "m1", :id "XZOQ0-m1" :description ""}
            {:name "m2", :id "Xd9k2-m2" :description ""}]
           (<>/get-methods db)))))

(defn name->obid
  [db t n]
  (let [type-attr (fn [t a] (keyword (clojure.string/join "/" [(name t) a])))
        name-attr (type-attr t "name")
        obid-attr (type-attr t "obfuscated-id")]
    (obid-attr (ffirst (d/q '[:find  (pull ?e [*])
                              :in    $ ?a ?n
                              :where [?e ?a ?n]]
                            db name-attr n)))))

(deftest test:add-sample
  (testing "one new sample"
    (let [db (as-> (mk-db) db (speculate db (#'<>/tx-data:add-project db "p1" ".*")))
          pn (name->obid db :project "p1")
          db (speculate db (#'<>/tx-data:add-sample db pn "s1"))]
      (is (= [(s/join "$" [pn "s1"])]
             (map :sample/name
                  (d/q '[:find  [(pull ?s [:sample/name]) ...]
                         :where [?p :project/sample ?s]
                         [?p :project/name "p1"]]
                       db))))))
  (testing "two new samples"
    (let [db (as-> (mk-db) db
               (speculate db (#'<>/tx-data:add-project db "p1" ".*")))
          pn (name->obid db :project "p1")
          db (as-> db db
               (speculate db (#'<>/tx-data:add-sample db pn "s1"))
               (speculate db (#'<>/tx-data:add-sample db pn "s2")))]
      (let [samples (d/q '[:find  [(pull ?s [*]) ...]
                           :where [?p :project/sample ?s]
                           [?p :project/name "p1"]]
                         db)]
        (is (= [1 2]
               (map :sample/id samples)))
        (is (= (map #(s/join "$" [pn %]) ["s1" "s2"])
               (map :sample/name samples))))))
  (testing "duplicate sample"
    (let [db (as-> (mk-db) db
               (speculate db (#'<>/tx-data:add-project db "p1" ".*")))
          pn (name->obid db :project "p1")]
      (is (thrown-with-msg? java.lang.IllegalStateException
              #"Unique conflict"
            (as-> db db
              (speculate db (#'<>/tx-data:add-sample db pn "s1"))
              (speculate db (#'<>/tx-data:add-sample db pn "s1")))))))
  (testing "two new samples in different projects"
    (let [db (as-> (mk-db) db
               (speculate db (#'<>/tx-data:add-project db "p1" ".*"))
               (speculate db (#'<>/tx-data:add-project db "p2" ".*")))
          p1 (name->obid db :project "p1")
          p2 (name->obid db :project "p2")
          db (as-> db db
               (speculate db (#'<>/tx-data:add-sample db p1 "s1"))
               (speculate db (#'<>/tx-data:add-sample db p2 "s1")))]
      (testing "sample IDs are globally unique (i.e. across all samples)"
        (is (= [1 2]
               (sort (map :sample/id (d/q '[:find  [(pull ?s [*]) ...]
                                            :where [?p :project/sample ?s]]
                                          db))))))
      (testing "samples have been assigned to the correct projects"
        (is (= [p1 p2]
               (->> [[p1 "s1"] [p2 "s1"]]
                    (map #(first
                           (d/q '[:find  [(pull ?p [:project/obfuscated-id])]
                                  :in    $ ?s
                                  :where [?p :project/sample ?s]]
                                db [:sample/name (apply #'<>/mk-sample-name %)])))
                    (map :project/obfuscated-id)))))))
  (testing "project does not exist"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
            #"requested resource Project:none could not be found"
          (let [db (mk-db)]
            (speculate db (#'<>/tx-data:add-sample db "none" "s1"))))))
  (testing "sample name does not match pattern"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
            #"does not conform to .* pattern"
          (let [db (as-> (mk-db) db
                     (speculate db (#'<>/tx-data:add-project db "p1" #"s-\d")))
                pn (name->obid db :project "p1")]
            (speculate db (#'<>/tx-data:add-sample db pn "s-a")))))))

(deftest test:get-sample
  (let [db (as-> (mk-db) db (speculate db (#'<>/tx-data:add-project db "p1" ".*")))
        pi (name->obid db :project "p1")
        db (speculate db (#'<>/tx-data:add-sample db pi "s1"))]
    (testing "one sample"
      (is (= {:name "s1", :id "OQn6Q-s1"}
             (<>/get-sample db pi (name->obid db :sample (#'<>/mk-sample-name pi "s1"))))))
    (testing "no sample"
      (is (nil? (<>/get-sample db pi "..."))))))

(defn- prepare-db:add-stage
  []
  (let [db (as-> (mk-db) db (speculate db (#'<>/tx-data:add-project db "p1" ".*")))
        p1 (name->obid db :project "p1")
        db (speculate db (#'<>/tx-data:add-method db "m1" ""))
        m1 (name->obid db :method "m1")
        db (speculate db (#'<>/tx-data:add-sample db p1 "s1"))
        s1 (name->obid db :sample (#'<>/mk-sample-name p1 "s1"))]
    {:db db :p1 p1 :m1 m1 :s1 s1}))

(deftest test:add-stage
  (testing "no annotations"
    (let [{:keys [db m1 s1]} (prepare-db:add-stage)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {}))]
      (is (= [{:stage/obfuscated-id "Drn1Q"
               :stage/method {:method/obfuscated-id "XZOQ0"}}]
             (d/q '[:find  [(pull ?e [:stage/obfuscated-id
                                      {:stage/method [:method/obfuscated-id]}
                                      :stage/annotation])]
                    :where [?e :stage/obfuscated-id]]
                  db)))))
  (testing "multiple annotations"
    (let [{:keys [db m1 s1]} (prepare-db:add-stage)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {"k1" "v1" "k2" "v2"}))]
      (is (= [{:stage/obfuscated-id "Drn1Q"
               :stage/method {:method/obfuscated-id "XZOQ0"}
               :stage/annotation [{:annotation/k "k1" :annotation/v "v1"}
                                  {:annotation/k "k2" :annotation/v "v2"}]}]
             (d/q '[:find  [(pull ?e [:stage/obfuscated-id
                                      {:stage/method [:method/obfuscated-id]}
                                      {:stage/annotation [:annotation/k :annotation/v]}])]
                    :where [?e :stage/obfuscated-id]]
                  db)))))
  (testing "stage is associated with a sample"
    (let [{:keys [db m1 s1]} (prepare-db:add-stage)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {}))]
      (is (= [{:sample/obfuscated-id "OQn6Q"}]
             (d/q '[:find  [(pull ?sample [:sample/obfuscated-id])]
                    :where [?sample :sample/stage]]
                  db))))))
