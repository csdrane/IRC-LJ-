(ns irc-server.connection
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [get-host-name receive send]]
            [clojure.core.async :refer [<! <!! >! >!! alts! chan go go-loop]]
            [clojure.string :as str :refer [split]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(declare add-nick! join-channel)

(defn coll->str [coll] (str/trim (reduce #(str %2 \space %1) "" coll)))

(defn channels 
  "Returns channels hashmap from state."
  [state]
  (get state :channels))

;; TODO write tests
(defn parse-cmd-user [args]
  (let [split-args (split args #" ")
        head (into [] (take 3 split-args))
        rest-args (str/join " " (drop 3 split-args))]
    (conj head rest-args)))

(defn parse-args [[cmd args]]
  (condp = cmd
    "QUIT" (identity args)
    "USER" (parse-cmd-user args)
    (split args #" ")))

;; FIX not parsing multiple args correctly
(defn parse-command [msg user]
  (let [[command args :as all] (split msg #" " 2)]
    {:cmd command :args (parse-args all) :user user}))

(defn state->sock [u s]
  (let [users @(get s :users)]
    (get-in users [u :socket])))

(defn user-on-channel?
  "Returns true if user within state has chan within its channels
  hashset."
  [user chan state]
  (let [users @(get state :users)]
    (contains? (get-in users [user :channels ])
               chan)))

(defn channel-exists?
  "Returns true if chan exists as key within channels hashset in state."
  [chan state]
  (contains? @(get state :channels) chan))

(defn cmd-resp-331
  "RPL_NOTOPIC
  <channel> :No topic is set"
  [{chan :chan user :user}]
  (let [servername :servername
        code (str 331)
        trailing ":No topic is set"]
    (str servername \space code \space user \space chan \space trailing)))

(defn cmd-resp-332
  "RPL_TOPIC
   \"<channel> :<topic>\"

  When sending a TOPIC message to determine the channel
  topic, one of two replies is sent. If the topic is set, RPL_TOPIC is
  sent back else RPL_NOTOPIC."
  [{user :user chan :chan chans :channels}]
  (let [servername :servername
        code (str 332)
        topic (get-in chans [chan :topic])
        trailing (str \: topic)]
    (str servername \space code \space user \space chan \space topic)))

(defn cmd-resp-353
  "RPL_NAMREPLY
  \"( = / * / @ ) <channel>
    :[ @ / + ] <nick> *( \" \" [ @ / + ] <nick> ) 

  @ is used for secret channels, * for private channels, and = for
  others (public channels)."
  [{user :user chan :chan chans :channels}]
  (let [servername :servername
        code (str 353)
        users (-> (get-in chans [chan :users])
                  (keys)
                  (coll->str))
        trailing (str \: users)]
    (str servername \space code \space chan \space users)))

(defn cmd-resp-366
  "RPL_ENDOFNAMES
  \"<channel> :End of NAMES list\"

  To reply to a NAMES message, a reply pair consisting of
  RPL_NAMREPLY and RPL_ENDOFNAMES is sent by the server back to the
  client. If there is no channel found as in the query, then only
  RPL_ENDOFNAMES is returned. The exception to this is when a NAMES
  message is sent with no parameters and all visible channels and
  contents are sent back in a series of RPL_NAMEREPLY messages with a
  RPL_ENDOFNAMES to mark the end."
  [{user :user chan :chan}]
  (let [servername :servername
        code (str 366)
        trailing ":End of /NAMES list."]
    (str servername \space code \space chan \space trailing)))

(defn cmd-resp-443
  "ERR_USERONCHANNEL
  \"<user> <channel> :is already on channel\"

  Returned when a client tries to invite a user to a channel they are
  already on."
  [{user :user chan :chan}]
  (let [servername :servername
        code (str 443)
        trailing ":is already on channel"]
    (str servername \space code \space user \space chan \space trailing)))

(defn cmd-response
  "Send command code and args to dispatcher. Args represents a hashmap
  containing required fields for a particular command code."
  [code args sock]
  (let [f (condp = code
            331 cmd-resp-331
            332 cmd-resp-332
            353 cmd-resp-353
            366 cmd-resp-366
            443 cmd-resp-443
            (throw (Exception. "Command code not recognized!")))
        msg (f args)]
    (spy (send sock msg))))

(defn add-channel-to-user!
  "Updates state by adding user both to users list of channel, and
  channel to user's active channel list. Note that the order of args
  user and chan is flipped from that in create-channel!"
  [user chan state]
  (dosync (alter (get state :users) update-in [user :channels] conj chan)
          (alter (get state :channels) assoc-in [chan :users user] {:v false, :o false})))

(defn create-channel!
  "Updates state by adding chan as key to channels hashmap and adding
  channel to user's active channel list."
  [chan user state]
  (let [chan-template {:attrs {}
                       :users {user {:o true
                                     :v true}}}]
    (dosync (alter (get state :channels) assoc chan chan-template)
            (alter (get state :users) update-in [user :channels] conj chan))))

(defn join-channel
  "First, checks if user is already on channel. If so, does nothing.
  Next, checks if channel already exists. If so, connects user. If
  not, creates it and then connects user.

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
  
  ;; TODO what about restrictions on joining? e.g. keys, invite-only,
  ;; size limit, bans, etc.
  [[chan & args] user state]
  (info (str "User attempted to join channel " chan))
  (let [s (state->sock user state)]
    (cond
      (user-on-channel? user chan state) (do (cmd-response 443 {:user user :chan chan} s)
                                             :user-already-on-channel)
      (channel-exists? chan state) (do (add-channel-to-user! user chan state)
                                       (cmd-response 332 {:user user
                                                          :chan chan
                                                          :channels @(channels state)} s)
                                       (cmd-response 353 {:user user
                                                          :chan chan
                                                          :channels @(channels state)} s)
                                       (cmd-response 366 {:chan chan} s)
                                       :add-channel-to-user!)
      :else (do (create-channel! chan user state)
                ;; (cmd-response 331 {:user user
                ;;                    :chan chan} s)
                ;; (cmd-response 353 {:user user
                ;;                    :chan chan
                ;;                    :channels @(channels state)} s)
                ;;                (cmd-response 366 {:chan chan} s)
                (send s ":csd_!~user@localhost JOIN #j\r\n")
                (send s ":servername MODE #j +ns\r\n")
                (send s ":servername 353 csd_ @ #j :@csd_\r\n")
                (send s ":servername 366 csd_ #j :End of /NAMES list.\r\n")))))

(def command-table
  {:NICK add-nick!
   :JOIN join-channel})

(defn lookup [command]
  (get command-table (keyword command)))

(defn dispatch-command 
  ([command args]
   (if-let [f (lookup command)]
     (spy (f args))))
  ([command args user state]
   (if-let [f (lookup command)]
     (spy (f args user state)))))

(defn valid? [nick]
  (re-matches #"[A-Za-z]{1}[A-Za-z0-9_-`\\\[\]\{\}^\|]{0,11}" nick))

(defn add-nick!
  "Attempts to secure requested nick. Also binds user's socket
  instance to user map."
  [nick sock state]
  (if (and (not (find @state nick))
           (valid? nick))
    (dosync
     (alter state assoc nick {:socket sock
                              :channels #{}})
     (spy (send sock (str "Your nick has been set to " nick "\n")))
     nick)
    (do
      (spy (send sock (str "Your nick " nick " is already taken and/or invalid. "
                       "Please pick another.\n")))
      nil)))

(defn channel?
  "Returns true if chan exists as key within channels hashmap of
  state."
  [chan state]
  (contains? (channels state) chan))

(defn check-ident
  "May not implement."
  [sock])

(defn lookup-host
  "Attempts lookup of user's host. Returns textual representation of
  IP if hostname not found."
  [sock]
  (let [host (get-host-name sock)]
    (spy (send sock (str "Your host has been identified as " host "\n")))
    host))

(defn assign-nick [sock state]
  (try (loop []
         ;; parse-command requires a nick as argument. nick is nil as not yet assigned.
         (let [in (parse-command (receive sock) nil)
               {cmd :cmd [nick & _] :args} in]
           (debug in)
           (cond
             (not= cmd "NICK") (do (spy (send sock (str "Invalid command: " cmd "\n")))
                                   (recur))
             (not (valid? nick)) (do (spy (send sock (str "Invalid nick: " nick "\n")))
                                     (recur))
             :else (spy (add-nick! nick sock state)))))
       (catch Exception e (str "Exception: " e))))

(defn assign-user-details!
  ;; Function assumes that input has been validated by
  ;; assign-user-details. username refers to client's username on
  ;; client's machine. hostname and servername are not used but are
  ;; part of the spec.
  [[username hostname servername realname] nick host state]
  (let [realname' (apply str (drop 1 realname)) ; remove colon
        new {nick {:username username :realname realname' :host host}}]
    (dosync (alter state #(merge-with merge %1 %2) new))))

(defn assign-user-details [nick host sock state]
  (try (loop []
         ;; TODO
         ;; parse-command needs to allow last argument (which must
         ;; begin with a colon (':'), to contain multiple words
         (println "Inside assign-user-details")
         (let [in (parse-command (receive sock) nick)
               {cmd :cmd args :args} in]
           (println (str "In: " in))
           (cond
             (not= cmd "USER") (do (spy (send sock (str "Invalid command: " cmd "\n")))
                                   (recur))
             (or (not= (count args) 4)
                 (not= (first (nth args 3)) ; first character of last
                       \:))
             (do (spy (send sock (str "Invalid arguments supplied to command: " cmd "\n")))
                 (recur))
             :else (assign-user-details! args nick host state))))
       (catch Exception e (str "Exception: " e)))) ; argument must begin with a \:

;; TODO refactor using send-error
(defn motd
  "Send MOTD to new connection."
  [sock]
  (spy (do (send sock ":servername 001 csd_ :Welcome to the server!\r\n")
           (send sock ":servername 002 csd_ :Enjoy your time at the server!\r\n")
           (send sock ":servername 003 csd_ :This server was created XYZ\r\n")
           ; modes stolen from freenode. not sure what they do.
           (send sock ":servername 004 csd_ servername irclj-0.1.0\r\n"))))

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
  (let [{cmd :cmd args :args user :user} (<!! c)]
    (spy (dispatch-command cmd args user state))))

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

;; FIX - I think it is unnecessary for a NICK request to follow a USER
;; request. The below may lead to bugs. The code should be agnostic to
;; which command comes first.
(defn new-connect
  "Performs connection actions (e.g. host lookup, nick acquisition,
  etc.) and then places client in connection loop."
  [sock state server-chan]
  (send sock "NOTICE * :*** Now attempting to look up your host\n")
  (let [host (lookup-host sock)
        nick (spy (assign-nick sock (state :users)))
        user (spy (assign-user-details nick host sock (state :users)))]
    (spy (motd sock))
    (client-listener nick sock state server-chan)))

