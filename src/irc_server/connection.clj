(ns irc-server.connection
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [send]]
            [clojure.string :refer [split]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(declare add-nick!)

(def command-table
  {:NICK add-nick!})

(defn lookup [command]
  (get command-table (keyword command)))

(defn parse-command [msg]
  (let [[command args & more] (split msg #" " 2)]
    [command args]))

(defn dispatch-command [[command args]]
  [command args]
  (if-let [f (lookup command)]
    (f args)))

(defn valid? [nick]
  (re-matches #"[A-Za-z]{1}[A-Za-z0-9_-`\\\[\]\{\}^\|]{0,11}" nick))

(defn add-nick!
  "Attempts to secure requested nick."
  [nick sock state]
  (if (and (not (find @state nick))
           (valid? nick))
    (dosync
     (alter state assoc nick {})
     (send sock (str "Your nick has been set to " nick "\n"))
     nick)
    (do
      (send sock (str "Your nick " nick " is already taken and/or invalid. "
                      "Please pick another.\n"))
      nil)))

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
  (debug msg user sock state)
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


