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

(defn coordinator
  "Function takes one argument, representing core.async channel.
  Coordinator receives messages from clients and dispatches."
  [c]
  (go-loop []
    (let [msg (<! c)]
      (println msg))
    (recur)))

(defn get-messages
  "Receives messages from client; sends them to coordinator."
  [sock server-chan]
  (go-loop [] (>! server-chan "a message")))

(defn update-client
  "Receive from Coordinator message destined for client. Sends to
  client."
  [sock])

(defn exec-client-cmds
  "Not sure yet how this is going to work. Either: 
  1) Messages should be parsed within connect-loop scope. 
  2) Coordinator tells exec-client-cmds to perform action. Or, 
  3) function is unnecessary / should be located within Coordinator 
  context."
  [sock])

(defn connect-loop
  "Connect-loop spawns the concurrent processes of listening for
  client input, sending messages to client, and evaluating client
  commands. Connect-loop is not actually a loop, but rather the
  functions it calls are wrapped in `go-loop`. This style is a product
  of using core.async."
  [sock state server-chan]
  (let [quit (atom false)]
    (spy (get-messages sock server-chan))
    (update-client sock)
    (exec-client-cmds sock)))

(defn new-connect
  "Performs connection actions (e.g. host lookup, nick acquisition,
  etc.) and then places client in connection loop."
  [sock state server-chan]
  (-> (lookup-host sock)
      (assign-nick sock (state :users)))
  (motd sock)
  (connect-loop sock state server-chan))
