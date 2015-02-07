(ns irc-server.state)

(defprotocol Lifecycle
  (start [component])
  (stop [component]))

(extend-protocol Lifecycle
  java.lang.Object
  (start [this]
    this)
  (stop [this]
    this))

(defrecord State [users channels]
  clojure.lang.IFn
  (invoke [this key] (get this key))
  Lifecycle
  (start [component]
    component)
  (stop [component]
    component))

