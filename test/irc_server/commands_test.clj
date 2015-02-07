(ns irc-server.commands-test
  (:require [clojure.test :refer :all]
            [irc-server.commands :refer [parse-command]]))

(deftest parse-commands
  (is (= (parse-command "NICK my-nick")
         ["NICK" "my-nick"])
      (= (parse-command "QUIT A default quit message.")
         ["QUIT" "A default quit message."])))
