(ns irc-server.core
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [receive send]]
            [clojure.java.io :as io])
  (:import [java.net InetAddress ServerSocket]))

(defn check-ident
  "May not implement."
  [sock])

(defn lookup-host
  "Attempts lookup of user's host. Returns textual representation of
  IP if hostname not found."
  [conn]
  (.getHostName (.getInetAddress conn)))

;; TODO validation check
;; min/max length, vaild chars
(defn process-nick-request
  "Attempts to secure requested nick."
  [nick global-state]
  (if-not (find (global-state :users) nick)
    nick
    nil))

(defn send-motd
  "Send MOTD to new connection."
   [conn])

(defn new-connect
  [msg user sock global-state]
  (let [state (get user :state)
        res (condp = state
              :host (lookup-host sock)
              :nick (process-nick-request msg global-state))]
    (cond
     (and (= state :host)
          (= res nil))
     (do
       (send sock "Your host could not be found.")
       (assoc user :host res :state :nick))
     (and (= state :host)
          (not= res nil))
     (do
       (send sock (str "Your host has been identified as " res))
       (assoc user :host res :state :nick))
     (and (= state :nick)
          (= res nil))
     (do
       (send sock (str "Your nick " msg " is already taken. Please pick another.")))
     (and (= state :nick)
          (not= res nil))
     (do
       (send sock (str "Your nick has been set to " res))
       (assoc user :nick res :state :motd)))))

(defn wrap-new-connect [msg sock global-state]
  (loop [user {:nick nil :host nil
               :state :host}]
    (let [new-user (new-connect msg user sock global-state)]
      (recur new-user))))

;; TODO move to socket.clj ?
;; Currently dead code.
(defn get-connection [port handler]
  (with-open [server-sock (ServerSocket. port)
              sock (.accept server-sock)]
    (let [msg-in (receive sock)]
      (wrap-new-connect msg-in sock))))



