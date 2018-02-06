(ns frost-fighters.game
  (:require [clojure.core.async :as a :refer [>! <! >!! <!!
                                                go go-loop chan put! poll!
                                                timeout]]
            [clojure.core.match :refer [match]]))

(defn create-initial-state [client-names]
  (into {} (map (fn [client]
                  [client {:x 0
                           :y 0
                           :left-pressed false
                           :right-pressed false}])
                client-names)))

(defn update-player [player]
  (update player :x + (if :left-pressed -1 0) (if :right-pressed 1 0)))

(defn broadcast-positions [state clients]
  (go
    (doseq [[_ channel] clients]
      (doseq [[username player-state] state]
        (>! channel (format "position:%s %d %d"
                            username
                            (:x player-state)
                            (:y player-state)))))))

(defn run-main-loop [input-channel connected-clients]
  (go-loop [server-state (create-initial-state (keys connected-clients))]
    (let [received-message (poll! input-channel)
          new-state (match received-message
                           [:key-down username key-name]
                           (assoc-in server-state
                                     [username
                                      (if "left" :left-pressed :right-pressed)]
                                     true)

                           [:key-up username key-name]
                           (assoc-in server-state
                                     [username
                                      (if "left" :left-pressed :right-pressed)]
                                     false)

                           ;; TODO: add check for closed channel
                           ;; (nil doesn't work with poll)
                           :else server-state)
          updated-state (into {} (for [[k v] new-state] [k (update-player v)]))]
      (broadcast-positions updated-state connected-clients)
      (<! (timeout 33))
      (recur updated-state))))
