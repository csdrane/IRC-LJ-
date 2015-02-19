(ns irc-server.socket
  (:refer-clojure :exclude [send])
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre])
  (:import [java.net ServerSocket]))

(timbre/refer-timbre) ; Provides useful Timbre aliases in this ns

(defn receive
  "Read a line of textual data from the given socket."
  [socket]
  (spy (.readLine (io/reader socket))
       ))

(defn send
  "Send the given string message out over the given socket."
  [socket msg]
  (try (let [writer (io/writer socket)]
         (.write writer msg)
         (.flush writer))
       (catch Exception e (str "Exception: " e))))

