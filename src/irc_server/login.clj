(ns irc-server.login
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [send]]))

(defn check-ident
  "May not implement."
  [sock])

(defn lookup-host
  "Attempts lookup of user's host. Returns textual representation of
  IP if hostname not found."
  [sock]
  (.getHostName (.getInetAddress sock)))

(defn valid? [nick]
  (re-matches #"[A-Za-z]{1}[A-Za-z0-9_-`\\\[\]\{\}^\|]{0,11}" nick))

(defn add-nick!
  "Attempts to secure requested nick."
  [nick state]
  (if (and (not (find @state nick))
           (valid? nick))
    (dosync
     (alter state assoc nick {})
     nick)
    nil))

(defn send-motd
  "Send MOTD to new connection."
  [sock]
  (send sock "Hello, world!"))

(defn init-conn-loop
  [msg user sock state]
  (let [state-key (get user :state)
        res (condp = state-key
              :host (lookup-host sock)
              :nick (do (add-nick! msg (state :users))
                        msg)
              nil)]
    (println msg) ; for debug purposes
    (cond
     (and (= state-key :host)
          (= res nil))
     (do
       (send sock "Your host could not be found.")
       (assoc user :host res :state :nick))
     (and (= state-key :host)
          (not= res nil))
     (do
       (send sock (str "Your host has been identified as " res))
       (assoc user :host res :state :nick))
     (and (= state-key :nick)
          (= res nil))
     (do
       (send sock (str "Your nick " msg " is already taken and/or invalid. Please pick another.")))
     (and (= state-key :nick)
          (not= res nil))
     (do
       (send sock (str "Your nick has been set to " res))
       (assoc user :nick res :state :motd))
     (= state-key :motd)
     (do
       (send-motd sock)
                                        ;(chat-loop)
       )
     :else (println "Hit `init-conn-loop :else`"))))
