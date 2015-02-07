(ns irc-server.commands
  (:require [clojure.string :refer [split]]
            [irc-server.login :refer [add-nick!]]))

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

