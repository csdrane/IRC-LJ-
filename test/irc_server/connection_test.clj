(ns irc-server.connection-test
  (:refer-clojure :exclude [send])
  (:require [clojure.test :refer :all]
            [irc-server.connection :refer [add-nick! add-channel-to-user!
                                           assign-nick assign-user-details!
                                           channel-exists? create-channel!
                                           join-channel
                                           lookup-host parse-command
                                           user-on-channel?]]
            [irc-server.socket :refer [send socket-map]]
            [irc-server.state :refer [->State]])
  (:import [java.net Socket InetAddress ServerSocket Socket SocketImpl]))

;; Eliminates port blockage issue
(def port (+ 1000 (rand-int 10000)))
(def listener (atom nil))
(def sock (atom nil))
(def client (atom nil))

(defn setup [sock]
  (let [new-listener (ServerSocket. port)
        new-client (Socket. "localhost" port)
        new-sock (socket-map (.accept new-listener))]
    (reset! listener new-listener)
    (reset! client new-client)
    (reset! sock new-sock)))

(defn teardown [sock]
  (.close (get @sock :raw))
  (.close @client)
  (.close @listener)
  (reset! sock nil)
  (reset! client nil)
  (reset! listener nil))

(deftest new-connect-test
  (let [state (->State (ref {})
                       (ref {}))
        user {:host nil :nick nil}]
    (setup sock)
    (is (= (lookup-host @sock) "localhost"))
    (is (= (add-nick! "foo" @sock (state :users))
           "foo"))
    (is (not= (add-nick! "foo" @sock (state :users))
              "foo"))
    (is (= (update-in (assign-user-details!
                       ["foo" "bar" "baz" ":qux quux"]
                       "foo"
                       "localhost"
                       (state :users))
                      ["foo"] dissoc  :socket)
           {"foo" {:realname "qux quux"
                   :username "foo"
                   :channels #{}
                   :host "localhost"}}))
    (teardown sock)))

(deftest parse-command-test
  (is (= (parse-command "NICK my-nick" "user")
         {:cmd "NICK"
          :args ["my-nick"]
          :user "user"}))
  (is (= (parse-command "QUIT A default quit message." "user")
         {:cmd "QUIT"
          :args "A default quit message."
          :user "user"})))

(deftest join-channel-test
  (let [state (->State (ref {"user" {:socket nil
                                     :hostname nil
                                     :channels #{}
                                     :kill-chan nil}})
                       (ref {}))
        new-state (->State  (ref {"user" {:socket nil
                                          :hostname nil
                                          :channels #{"foo"}
                                          :kill-chan nil}})
                            (ref {"foo" {:attrs {}
                                         :users {"user" {:v true, :o true}}}}))]
    (create-channel! "foo" "user" state)
    (testing "Testing mutable state re: channel creation"
      (is (= (deref (get state :channels))
             (deref (get new-state :channels)))
          (= (deref (get state :users))
             (deref (get new-state :users))))))
  (let [state (->State (ref {"user" {:socket nil
                                     :hostname nil
                                     :channels #{}
                                     :kill-chan nil}})
                       (ref {"foo" {:attrs {}
                                    :users {"other-user" {:v true, :o true}}}}))
        new-state (->State (ref {"user" {:socket nil
                                         :hostname nil
                                         :channels #{"foo"}
                                         :kill-chan nil}})
                           (ref {"foo" {:attrs {}
                                        :users {"user" {:v false, :o false}
                                                "other-user" {:v true, :o true}}}}))]
    (add-channel-to-user! "user" "foo" state)
    (testing "Testing mutable state re: channel creation"
      (is (= (deref (get state :channels))
             (deref (get new-state :channels)))
          (= (deref (get state :users))
             (deref (get new-state :users)))))
    (testing "channel-exists?"
      (is (= (channel-exists? "foo" new-state)
             true)
          (not= (channel-exists? "bar" new-state))))
    (testing "user-on-channel?"
      (is (true? (user-on-channel? "user" "foo" new-state))
          (false? (user-on-channel? "user" "bar" new-state))))
    (testing "join-channel logic"
      (is (= (join-channel ["foo"] "user" new-state)
             :user-already-on-channel))
      (is (= (join-channel ["foo"] "not-user" new-state)
             :add-channel-to-user!)))))



