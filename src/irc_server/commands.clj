(ns irc-server.commands
  (:refer-clojure :exclude [send])
  (:require [irc-server.socket :refer [send]]
            [clojure.string :refer [split]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre) ; Provides useful Timbre aliases in this ns

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

