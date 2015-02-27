(ns irc-server.socket
  (:refer-clojure :exclude [send])
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre])
  (:import [java.net ServerSocket]))

(timbre/refer-timbre) ; Provides useful Timbre aliases in this ns

;; FYI this may ultimately be a source of memory leaks if the JVM
;; doesn't garbage collect rdr and wtr after a client disconnects.
;; Should GC if allowed to go out of scope... Will need to ensure to
;; delete from shared state
(defn socket-map [sock]
  (let [rdr (io/reader sock)
        wtr (io/writer sock)]
    (hash-map :rdr rdr
              :wtr wtr
              :raw sock)))

(defn get-host-name [{sock :raw}]
  (.getHostName (.getInetAddress sock)))

(defn receive
  "Read a line of textual data from the given socket."
  [{reader :rdr}]
  (spy (.readLine reader)))

(defn send
  "Send the given string message out over the given socket."
  [{writer :wtr} msg]
  (try (spy (do (.write writer msg)
                (.flush writer)
                (println (str "Out: " msg))))
       (catch Exception e (str "Exception: " e))))

