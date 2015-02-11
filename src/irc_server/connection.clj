(ns irc-server.connection
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [send receive]]
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

(defn assign-nick [host sock state]
  (loop []
    (let [[command nick] (parse-command (receive sock))]
      (cond
       (not= command "NICK") (do (send sock (str "Invalid command: " command "\n"))
                                 (recur))
       (not (valid? nick)) (do (send sock (str "Invalid nick: " nick "\n"))
                               (recur))
       :else (add-nick! nick sock state)))))

(defn motd
  "Send MOTD to new connection."
  [sock]
  (send sock "Hello, world!"))

;; FIX: server discards input (e.g. NICK request) made during host lookup step.

(defn connect-loop [sock state])

(defn new-connect [sock state]
  (-> (lookup-host sock)
      (assign-nick sock (state :users)))
  (motd sock)
  (connect-loop sock state))
