(ns irc-server.core
  (:refer-clojure :exclude [send])
  (:require [irc-server.login :refer [init-conn-loop]]
            [irc-server.logging :as logging]
            [irc-server.socket :refer [receive send]]
            [irc-server.state :refer [->State]]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre])
  (:import [java.net InetAddress ServerSocket SocketException]))

(timbre/refer-timbre) ; Provides useful Timbre aliases in this ns
(logging/init)

(defn wrap-new-connect [sock state]
  (loop [user {:nick nil, :host nil, :state :host}]
                                        ; FIX: loops whether new data from sock or not
    (let [msg (spy (receive sock))
          new-user (init-conn-loop msg user sock state)]
      (recur new-user))))

(defn get-connection [port state]
  (with-open [server-sock (ServerSocket. port)
              sock (.accept server-sock)]
    (wrap-new-connect sock state)))

;; We wrap get-connection in try/catch to prevent a SocketException from being thrown
;; when a client unexpectedly disconnects.

;; FIX: Unexpected disconnections may cause a NullPointerException if server was expecting
;; user input.

(defn -main []
  (let [port 6667
        state (->State (ref {})
                       (ref {}))]
    (try (get-connection port state)
         (catch SocketException e))))

; (-main)
