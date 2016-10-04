
(def project 'sagittariidae-ws)

(def version "0.1.0-SNAPSHOT")

(defn- dependencies
  "Convert a leiningen-like dependency map into a boot dependency vector.  Map
  keys are the build stage, and values are vectors of the standard
  dependency [name version] tuple, e.g.:
  ```
  (set-env! :dependencies
            (dependencies {:build '[[org.clojure/clojure \"1.7.0\"] ...]
                           :test  '[[midje \"1.4.0\" :exclusions [org.clojure/clojure]]]
                           :dev   '[[org.slf4j/slf4j-nop \"1.7.13\"]]}))
  ```
  This example highlights another feature: build stage synonyms.  It can be
  (conceptually, if not practically) useful to distinguish between dependencies
  that provide development infrastrcture (REPL utils), and those that support
  testing (testing frameworks).  Thus `:dev` is a synonym for `:test` and both
  can be used together in the same definition make this distinction.  For
  convenience `:build` is a synonym for `compile`."
  [m]
  (letfn [(scope-dependency [scope spec]
            (let [[p v & opt-lst] spec
                  opts (apply hash-map opt-lst)]
              (vec (concat [p v] (reduce concat (assoc opts :scope scope))))))
          (scope-dependencies [scope specs]
            (vec (map #(scope-dependency scope %) specs)))]
    (vec
     (reduce concat
             (for [[scope specs] m]
               (scope-dependencies (cond (= :build scope) "compile"
                                         (= :dev scope) "test"
                                         :else (if (keyword? scope)
                                                 (name scope)
                                                 (str scope)))
                                   specs))))))

;; Credentials are required and should be configured using a `profile.boot`
;; `configure-repositories!` hook.
;; cf. https://github.com/boot-clj/boot/wiki/Repository-Credentials-and-Deploying#environment
(set-env! :repositories
          #(conj % ["datomic" {:url "https://my.datomic.com/repo"}]))

(set-env! :resource-paths
          #{"resources" "source"}
          :source-paths
          #{"test"}
          :dependencies
          (dependencies {:build '[[org.clojure/clojure     "1.8.0"]
                                  [org.clojure/data.json   "0.2.6"]
                                  [com.datomic/datomic-pro "0.9.5350"]
                                  [compojure               "1.5.1"]
                                  [http-kit                "2.1.18"]
                                  [ring/ring-codec         "1.0.1"]
                                  [ring/ring-core          "1.5.0"]
                                  [sinistral/mantle        "0.3.0"]
                                  [swiss-arrows            "1.0.0"]]
                         :test  '[[adzerk/boot-test        "1.1.2"]
                                  [jstrutz/hashids         "1.0.1"]]
                         :dev   '[[org.clojure/tools.nrepl "0.2.10"]
                                  [org.slf4j/slf4j-nop     "1.7.21"]
                                  [pandeiro/boot-http      "0.7.4-SNAPSHOT"]
                                        ; Patched with https://github.com/pandeiro/boot-http/pull/52
                                        ; FIXIT: Publish own patched version to Clojars.
                                  ]}))

(task-options!
 aot {:namespace   #{'sagittariidae.ws.core}}
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/sagittariidae"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main        'sagittariidae.ws.core
      :file        (str "sagittariidae-ws-" version "-standalone.jar")})

(require '[pandeiro.boot-http :refer [serve]])

(deftask dev
  []
  (comp (serve :httpkit true
               :port    5000
               :init    'sagittariidae.ws.core/initialize
               :handler 'sagittariidae.ws.core/service-handler
               :nrepl   {})
        (wait)))

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[sagittariidae.core :as app])
  (apply (resolve 'app/-main) args))

(require '[adzerk.boot-test :refer [test]])
