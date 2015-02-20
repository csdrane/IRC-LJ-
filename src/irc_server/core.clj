(ns irc-server.core
  (:refer-clojure :exclude [send])
  (:require [irc-server.connection :refer [server-coordinator new-connect]]
            [irc-server.logging :as logging]
            [irc-server.socket :refer [receive send]]
            [irc-server.state :refer [->State]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [>! <! chan go]]
            [taoensso.timbre :as timbre])
  (:import [java.net InetAddress ServerSocket SocketException]))

(timbre/refer-timbre) ; Provides useful Timbre aliases in this ns
(logging/init)

(defn get-connection [port state server-chan]
  (with-open [server-sock (ServerSocket. port)
              sock (.accept server-sock)]
;; TODO new connection should spawn a new thread
    (new-connect sock state server-chan)))

;; We wrap get-connection in try/catch to prevent a SocketException from being thrown
;; when a client unexpectedly disconnects.

;; FIX: Unexpected disconnections may cause a NullPointerException if server was expecting
;; user input.

(defn -main []
  (let [port 6667
        state (->State (ref {})
                       (ref {}))
        server-chan (chan)]
    (spy (server-coordinator state server-chan))
    (try (get-connection port state server-chan)
         (catch SocketException e))))

; (-main)

;; FYI: may need to buffer received packets if allowed message length is larger than packet size
;; Could lead to weird bugs if unaddressed.
;; TODO add permissions to :users to regulate what functions they have access to
