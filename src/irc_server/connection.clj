(ns irc-server.connection
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [send receive]]
            [clojure.core.async :refer [<! <!! >! >!! alts! chan go go-loop]]
            [clojure.string :refer [split]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(declare add-nick! join-channel!)

(def command-table
  {:NICK add-nick!
   :JOIN join-channel!})

(defn lookup [command]
  (get command-table (keyword command)))

;; FIX not parsing multiple args correctly
(defn parse-command [msg user]
  (let [[command args & more] (split msg #" " 2)]
    {:cmd [command args] :user user}))

(defn dispatch-command 
  ([[command args]]
   (if-let [f (lookup command)]
     (f args)))
  ([[command args] state]
   (if-let [f (lookup command)]
     (f args state))))

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

(defn join-channel!
  "Checks if channel already exists. If so, connects user. If not,
  creates it and then connects user.

  When the channel already exists, we will add user to the list of
  channel's undecorated users. We will have to reflect this in both
  the individual channel's data structure and the user's (the hashset
  joined channels).

  When the channel does not already exist, we must add to the user's
  channel hashset, create a hashmap entry for the channel in the
  channels hashmap, and then give the user operator permissions.

  After these state change activities, we will send the user the
  current topic, if it exists, prefixed by error code 332 RPL_TOPIC.
  If not, we will simply send error code 331 RPL_NOTOPIC."
  
  ;; FIX let's forget about args for now.
  ;; FIX need to add User to args
  [[chan & args] state]
  (info (str "User attempted to join channel " chan))
  (if-not (channel? chan)
    (create-channel )))

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
                                   ; username is nil as not yet assigned
         (let [{[command nick] :cmd} (parse-command (receive sock) nil)]

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
  #_(go-loop []
    (let [msg (<! updates)]
      (println msg))
    (recur)))

(defn update-state
  "Merges updates into global state."
  [state updates])

(defn exec-client-cmds
  "Receives command from client thread and evaluates."
  [state c]
  (let [{[cmd & args] :cmd user :user} (<!! c)]
    (spy (dispatch-command [cmd args] state))))

(defn server-coordinator
  "Function takes one argument, representing core.async channel.
  Coordinator receives messages from clients and dispatches."
  [state c]
  (go-loop []
    (let [updates nil]
      (exec-client-cmds state c)
      (update-clients updates)
      (update-state state updates))
    (recur)))

(defn get-message
  "Receives message from client."
  [sock]
  (receive sock))

(defn exec-client-cmds-wrapper
  "Receives parsed command and sends to server thread."
  [cmd c]
  (go (>! c cmd)))

;; TODO wrap in loop/recur
;; How will we kill loop when socket connection dies?
;; -> Could use async channel. Pass message to set recur to false
;; -> Might make sense to reference channel in user's state; that way won't have to pass as an argument in every function involving user
(defn client-listener
  "Blocks client thread while listening for new messages. New messages
  are parsed and then sent to exec-client-cmds to be executed."
  [user sock state server-chan]
  (loop []

    ;; u is a channel that will be part of user's state. when in other
    ;; parts of the code, a quitting event occurs, a message will be
    ;; sent to `u` setting keep-alive? false
    
    (let [u (chan 1) ; temporary var until move to user state
          keep-alive? (go (alts! [u] :default true))]
      (->  (get-message sock)
           (parse-command user)
           (exec-client-cmds-wrapper server-chan))
      (recur))))

(defn new-connect
  "Performs connection actions (e.g. host lookup, nick acquisition,
  etc.) and then places client in connection loop."
  [sock state server-chan]
  (let [host (lookup-host sock)
        nick (assign-nick host sock (state :users))]
    (motd sock)
    (client-listener nick sock state server-chan)))

