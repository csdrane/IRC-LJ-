(ns irc-server.core-test
  (:require [clojure.test :refer :all]
            [irc-server.core :as c])
  (:import [java.net Socket InetAddress ServerSocket Socket SocketImpl]
           [org.apache.commons.io.output NullOutputStream]))

;; (def sock (NullOutputStream. ))
;; (let [sock (proxy [java.net.ServerSocket] []
;;              (getInetAddress []
;;                (proxy [java.net.InetAddress] []
;;                  (getHostName [] (println "foo")))))]
;;   (.getInetAddress sock))

(def port 6667)
(def listener (atom nil))
(def sock (atom nil))
(def client (atom nil))

(defn setup [sock]
  (let [new-listener (ServerSocket. port)
        new-client (Socket. "localhost" port)
        new-sock (.accept new-listener)]
    (reset! listener new-listener)
    (reset! sock new-sock)
    (reset! client new-client)))

(defn teardown [sock]
  (.close @listener)
  (.close @sock)
  (.close @client)
  (reset! listener nil)
  (reset! sock nil)
  (reset! client nil))

(deftest new-connect
  (let [global-state {:users {"bar" {}}}
        user-state {:host nil :nick nil :state :host}]
    (setup sock)
    (is (= (c/new-connect
            "foo" user-state
            @sock global-state)
           {:host "localhost" :nick nil :state :nick}))
    (is (= (c/new-connect
            "foo" (assoc user-state :state :nick)
            @sock global-state)
           {:host nil :nick "foo" :state :motd}))
    (is (= (c/process-nick-request "foo" global-state)
           "foo"))
    (is (not= (c/process-nick-request "bar" global-state)
              "bar"))
    (teardown sock)))



 ;; check about how `foo` is used in various cases when looking through new-connect.

 

