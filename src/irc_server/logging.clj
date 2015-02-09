(ns irc-server.logging
 (:require [taoensso.timbre :as timbre]))

(timbre/refer-timbre) ; Provides useful Timbre aliases in this ns

(defn init []
  (timbre/set-config!
   [:fmt-output-fn]
   (fn [{:keys [level throwable message timestamp hostname ns]}
        ;; Any extra appender-specific opts:
        & [{:keys [nofonts?] :as appender-fmt-output-opts}]]
     ;; <timestamp> <hostname> <LEVEL> [<ns>] - <message> <throwable>
     (format "%s [%s] - %s%s"
             (-> level name clojure.string/upper-case) ns (or message "")
             (or (timbre/stacktrace throwable "\n" (when nofonts? {})) "")))))

