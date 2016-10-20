
(ns sagittariidae.ws.liaison-test
  (:require [clojure.string           :as    s]
            [clojure.test             :refer [deftest is testing]]
            [datomic.api              :as    d]
            [sagittariidae.ws.db      :as    db]
            [sagittariidae.ws.db-aux  :refer [mk-db name->obid speculate]]
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

(deftest test:extern-id-in-resource
  (is (= {:id "f00-bar" :name "Bar"}
         (#'<>/extern-id-in-resource {:id 0 :obfuscated-id "f00" :name "Bar"})))
  (is (= {:id "f00"}
         (#'<>/extern-id-in-resource {:id 0 :obfuscated-id "f00"}))))

(deftest test:extern-resource
  (testing "general case"
    (is (= {:id "f00-bar" :name "Bar" :attr0 "nil" :attr1 "one"}
           (#'<>/extern-resource {:res/type            {:db/ident :res.type/thing}
                                  :thing/id            0
                                  :thing/obfuscated-id "f00"
                                  :thing/name          "Bar"
                                  :thing/attr0         "nil"
                                  :other-thing/attr1   "one"}))))
  (testing "no name"
    (#'<>/extern-resource {:res/type            {:db/ident :res.type/thing}
                           :thing/id            0
                           :thing/obfuscated-id "f00"})))

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
           (sort #(compare (:name %1) (:name %2)) (<>/get-methods db))))))

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
               (sort (map :sample/id samples))))
        (is (= (map #(s/join "$" [pn %]) ["s1" "s2"])
               (sort (map :sample/name samples)))))))
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
      (is (nil? (<>/get-sample db pi "..."))))
    (testing "project ID mismatch"
      (is (nil? (<>/get-sample db "..." (name->obid db :sample (#'<>/mk-sample-name pi "s1"))))))
    (testing "sample for project"
      (let [db (speculate db (#'<>/tx-data:add-project db "p2" ".*"))
            p2 (name->obid db :project "p2")
            db (speculate db (#'<>/tx-data:add-sample db p2 "s2"))]
        (is (= {:name "s1", :id "OQn6Q-s1"}
               (<>/get-sample db pi (name->obid db :sample (#'<>/mk-sample-name pi "s1")))))))))

(defn- prepare-db:add-stage
  []
  (as-> {} m
    (assoc m :db (as-> (mk-db) db (speculate db (#'<>/tx-data:add-project db "p1" ".*"))))
    (assoc m :p1 (name->obid (:db m) :project "p1"))
    (assoc m :db (speculate (:db m) (#'<>/tx-data:add-method (:db m) "m1" "")))
    (assoc m :m1 (name->obid (:db m) :method "m1"))
    (assoc m :db (speculate (:db m) (#'<>/tx-data:add-sample (:db m) (:p1 m) "s1")))
    (assoc m :s1 (name->obid (:db m) :sample (#'<>/mk-sample-name (:p1 m) "s1")))))

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

(deftest test:get-sample-with-stage
  (testing "no stage key present when no stages"
    (let [{:keys [db p1 m1 s1]} (prepare-db:add-stage)]
      (is (nil? (:stage (<>/get-sample db p1 s1))))))
  (testing "stage is included when available"
    (let [{:keys [db p1 m1 s1]} (prepare-db:add-stage)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {"k1" "v1"}))
          rs (<>/get-sample db p1 s1)]
      (is (= 1 (count (:stage rs))))
      (is (reduce (fn [x y] (and x y))
                  (map (fn [k] (contains? (first (:stage rs)) k))
                       [:method :annotation]))))))

(defn- prepare-db:get-stages
  []
  (as-> {} m
    (assoc m :db (as-> (mk-db) db (speculate db (#'<>/tx-data:add-project db "p1" ".*"))))
    (assoc m :p1 (name->obid (:db m) :project "p1"))
    (assoc m :db (speculate (:db m) (#'<>/tx-data:add-method (:db m) "m1" "")))
    (assoc m :m1 (name->obid (:db m) :method "m1"))
    (assoc m :db (speculate (:db m) (#'<>/tx-data:add-method (:db m) "m2" "")))
    (assoc m :m2 (name->obid (:db m) :method "m2"))
    (assoc m :db (speculate (:db m) (#'<>/tx-data:add-sample (:db m) (:p1 m) "s1")))
    (assoc m :s1 (name->obid (:db m) :sample (#'<>/mk-sample-name (:p1 m) "s1")))))

(deftest test:get-stages
  (testing "annotations are included when present"
    (let [{:keys [db p1 s1 m1]} (prepare-db:get-stages)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {"k1" "v1"}))]
      (is (= [{:method "XZOQ0-m1" :id "Drn1Q-001" :annotation "k1=v1"}]
             (<>/get-stages db p1 s1)))))
  (testing "annotations are absent when not present"
    (let [{:keys [db p1 s1 m1]} (prepare-db:get-stages)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {}))]
      (is (= [{:method "XZOQ0-m1" :id "Drn1Q-001"}]
             (<>/get-stages db p1 s1)))))
  (testing "multiple stages are returned"
    (let [{:keys [db p1 s1 m1 m2]} (prepare-db:get-stages)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {}))
          db (speculate db (#'<>/tx-data:add-stage db s1 m2 {}))]
      (is (= [{:method "XZOQ0-m1", :id "Drn1Q-001"}
              {:method "Xd9k2-m2", :id "bQ8bm-002"}]
             (<>/get-stages db p1 s1))))))
