(ns irc-server.connection-test
  (:refer-clojure :exclude [send])
  (:require [clojure.test :refer :all]
            [irc-server.connection :refer [add-nick! assign-nick lookup-host parse-command]]
            [irc-server.socket :refer [send]]
            [irc-server.state :refer [->State]])
  (:import [java.net Socket InetAddress ServerSocket Socket SocketImpl]))

;; Eliminates port blockage issue
(def port (+ 1000 (rand-int 10000)))
(def listener (atom nil))
(def sock (atom nil))
(def client (atom nil))

(defn setup [sock]
  (let [new-listener (ServerSocket. port)
        new-client (Socket. "localhost" port)
        new-sock (.accept new-listener)]
    (reset! listener new-listener)
    (reset! client new-client)
    (reset! sock new-sock)))

(defn teardown [sock]
  (.close @sock)
  (.close @client)
  (.close @listener)
  (reset! sock nil)
  (reset! client nil)
  (reset! listener nil))

(deftest new-connect
  (let [state (->State (ref {})
                       (ref {}))
        user {:host nil :nick nil :state :host}]
    (setup sock)
    (is (= (lookup-host @sock) "localhost"))
    (is (= (add-nick! "foo" @sock (state :users))
           "foo"))
    (is (testing "Testing mutable state"
          (not= (add-nick! "foo" @sock (state :users))
                "foo")))
    (teardown sock)))


(deftest parse-commands
  (is (= (parse-command "NICK my-nick")
         ["NICK" "my-nick"])
      (= (parse-command "QUIT A default quit message.")
         ["QUIT" "A default quit message."])))
