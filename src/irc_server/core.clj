(ns irc-server.core
  (:refer-clojure :exclude [send])
  (:require [irc-server.login :refer [init-conn-loop]]
            [irc-server.socket :refer [receive send]]
            [irc-server.state :refer [->State]]
            [clojure.java.io :as io])
  (:import [java.net InetAddress ServerSocket]))

(defn wrap-new-connect [sock state]
  (loop [user {:nick nil, :host nil, :state :host}]
    (let [msg (receive sock)
          new-user (init-conn-loop msg user sock state)]
      (recur new-user))))

(defn get-connection [port state]
  (with-open [server-sock (ServerSocket. port)
              sock (.accept server-sock)]
    (wrap-new-connect sock state)))

(defn -main []
  (let [port 6667
        state (->State (ref {})
                       (ref {}))]
    (get-connection port state)))

; (-main)
