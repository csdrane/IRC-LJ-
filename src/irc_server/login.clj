(ns irc-server.login
  (:refer-clojure :exclude [send])
  (:require [irc-server.commands :refer [parse-command add-nick!]]
            [irc-server.socket :refer [send]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn check-ident
  "May not implement."
  [sock])

(defn lookup-host
  "Attempts lookup of user's host. Returns textual representation of
  IP if hostname not found."
  [sock]
  (let [host (.getHostName (.getInetAddress sock))]
    (send sock (str "Your host has been identified as " host "\n"))
    host))

(defn send-motd
  "Send MOTD to new connection."
  [sock]
  (send sock "Hello, world!"))

;; FIX: server discards input (e.g. NICK request) made during host lookup step.

(defn init-conn-loop
  [msg user sock state]
  (debug msg user sock state)xb
  (let [state-key (get user :state)
        res (condp = state-key
              :host (lookup-host sock)
              :nick (let [[command args] (parse-command msg)]
                      (if (= command "NICK")
                        (spy (add-nick! args sock (state :users)))))
              nil)]
    (cond
     (= state-key :host)
     (assoc user :host res :state :nick)
     (and (= state-key :nick)
          (not= res nil))
     (do
       (assoc user :nick res :state :motd))
     (= state-key :motd)
     (do
       (send-motd sock))                ; chat loop
     :else (do
             (println "Hit `init-conn-loop :else`")
             user                  ; return user to sustain loop/recur
             ))))
