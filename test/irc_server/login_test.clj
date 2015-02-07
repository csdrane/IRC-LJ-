(ns irc-server.login-test
  (:refer-clojure :exclude [send])
  (:require [clojure.test :refer :all]
            [irc-server.login :refer [add-nick! init-conn-loop]]
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
    (is (= (init-conn-loop
            nil user
            @sock state)
           {:host "localhost" :nick nil :state :nick}))
    (is (= (init-conn-loop
            "foo" (assoc user :state :nick)
            @sock state)
           {:host nil :nick "foo" :state :motd}))
    (is (not= (add-nick! "foo" (state :users))
              "foo"))
    (is (= (add-nick! "bar" (state :users))
           "bar"))
    (teardown sock)))

