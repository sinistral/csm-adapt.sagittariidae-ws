
(ns sagittariidae.ws.core-test
  (:require [clojure.data.json        :as    json]
            [clojure.test             :refer [deftest is testing]]
            [hashids.core             :as    hashid]
            [mantle.io                :refer [fmtstr]]
            [sagittariidae.ws.core    :refer [routes]]
            [sagittariidae.ws.db      :as    db]
            [sagittariidae.ws.liaison :as    <>]
            [sagittariidae.ws.db-aux  :refer [mk-db name->obid speculate]]))

(defn req
  [method resource & params]
  (routes {:request-method method :uri resource :params (first params)}))

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
  (testing "stages are present"
    (let [{:keys [db p1 s1 m1]} (prepare-db:get-stages)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {"k1" "v1"}))]
      (with-redefs [db/db (fn [] db)]
        (let [uri "/projects/~a/samples/~a/stages"
              rsp (json/read-str (:body (req :get (fmtstr uri p1 s1))))]
          (is (= 1 (count (get rsp "stages"))))))))
  (testing "\"next\" token is present and correct"
    (let [{:keys [db p1 s1 m1]} (prepare-db:get-stages)
          db (speculate db (#'<>/tx-data:add-stage db s1 m1 {"k1" "v1"}))]
      (with-redefs [db/db (fn [] db)]
        (let [uri "/projects/~a/samples/~a/stages"
              rsp (json/read-str (:body (req :get (fmtstr uri p1 s1))))]
          (is (= 2 (first
                    (hashid/decode
                     (#'<>/get-stage-hashid-opts db) (get rsp "token"))))))))))

(deftest test:put-stage
  (testing "invalid annotations are a client error"
    (let [{:keys [db p1 s1 m1]} (prepare-db:get-stages)]
      (let [uri "/projects/~a/samples/~a/stages/some-stage"
            rsp (req :put (fmtstr uri p1 s1) {:annotation "foobar"})]
        (is (= 400 (:status rsp)))
        (is (= 0 (count (get rsp "stages")))))))
  (testing "an invalid token is a conflict"
    (let [{:keys [db p1 s1 m1]} (prepare-db:get-stages)]
      (with-redefs [<>/-add-stage (fn [_1 _2 s m a t]
                                    (speculate db (#'<>/tx-data:add-stage db s m a t)))]
        (let [uri "/projects/~a/samples/~a/stages/some-stage"
              rsp (req :put (fmtstr uri p1 s1) {:annotation "foo=bar" :method "???"})]
          (is (= 409 (:status rsp)))
          (is (= 0 (count (get rsp "stages")))))))))
