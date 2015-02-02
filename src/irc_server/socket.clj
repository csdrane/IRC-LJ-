(ns irc-server.socket
  (:refer-clojure :exclude [send])
  (:require [clojure.java.io :as io])
  (:import [java.net ServerSocket]))

(defn receive
  "Read a line of textual data from the given socket."
  [socket]
  (.readLine (io/reader socket)))

(defn send
  "Send the given string message out over the given socket."
  [socket msg]
  (let [writer (io/writer socket)]
    (.write writer msg)
    (.flush writer)))

