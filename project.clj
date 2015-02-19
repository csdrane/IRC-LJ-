(defproject irc-server "0.1.0-SNAPSHOT"
  :description "Pre-alpha IRC server"
  :url "https://github.com/csdrane/IRClj"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [com.taoensso/timbre "3.3.1"]
                 [commons-io/commons-io "2.4"]]
  :main ^:skip-aot irc-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
