(ns irc-server.connection
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [send receive]]
            [clojure.core.async :refer [<! >! go go-loop]]
            [clojure.string :refer [split]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(declare add-nick!)

(def command-table
  {:NICK add-nick!})

(defn lookup [command]
  (get command-table (keyword command)))

(defn parse-command [msg user]
  (let [[command args & more] (split msg #" " 2)]
    {:cmd [command args] :user user}))

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
  (try (loop []
         (let [[command nick] (parse-command (receive sock))]
           (cond
            (not= command "NICK") (do (send sock (str "Invalid command: " command "\n"))
                                      (recur))
            (not (valid? nick)) (do (send sock (str "Invalid nick: " nick "\n"))
                                    (recur))
            :else (add-nick! nick sock state))))
       (catch Exception e (str "Exception: " e))))

(defn motd
  "Send MOTD to new connection."
  [sock]
  (send sock "Hello, world!"))

(defn update-clients
  "Receive from Coordinator da message destined for client. Sends to
  client."
  [updates]
  (go-loop []
    (let [msg (<! updates)]
      (println msg))
    (recur)))

(defn update-state
  "Merges updates into global state."
  [state updates])

(defn exec-client-cmds
  "Receives command from client thread and evaluates."
  [state c])

(defn server-coordinator
  "Function takes one argument, representing core.async channel.
  Coordinator receives messages from clients and dispatches."
  [state c]
  (let [updates nil]
   (exec-client-cmds state c)
   (update-clients updates)
   (update-state state updates)))

(defn get-message
  "Receives message from client."
  [sock]
  (receive sock))

(defn exec-client-cmds-wrapper
  "Receives parsed command and sends to server thread."
  [cmd c]
  (>! c cmd))

;; TODO wrap in loop/recur
;; How will we kill loop when socket connection dies?
(defn client-listener
  "Blocks client thread while listening for new messages. New messages
  are parsed and then sent to exec-client-cmds to be executed."
  [user sock state server-chan]
  (let [quit (atom false)]
    (->  (get-message sock)
         (parse-command user)
         (exec-client-cmds-wrapper server-chan))))

(defn new-connect
  "Performs connection actions (e.g. host lookup, nick acquisition,
  etc.) and then places client in connection loop."
  [sock state server-chan]
  (let [host (lookup-host sock)
        nick (assign-nick sock (state :users))]
    (motd sock)
    (client-listener nick sock state server-chan)))

