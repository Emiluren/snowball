(ns frost-fighters.core
  (:require [clojure.core.async :as a :refer [>! <! >!! <!! close!
                                              go go-loop chan put! timeout]]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [ring.util.response :refer [file-response]]
            [org.httpkit.server :as httpkit]
            [compojure.core :refer [defroutes GET PUT]]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [frost-fighters.game :as game]))

(def frontend-path "../public")

(defn index []
  (file-response "index.html" {:root frontend-path}))

(defonce server (atom nil))

(defonce lobby-channels (ref {}))

(defn reset-channels []
  (dosync
   (doseq [[_ channel] @lobby-channels]
     (close! channel))
   (ref-set lobby-channels {})))

(defn start-game [input-channel connected-clients]
  (println "Starting game")
  (let [start-game-message (str "start game:"
                                (apply str (interpose " " connected-clients)))]
    (go
      (doseq [[_ client] connected-clients]
        (>! client start-game-message))))
  (game/run-main-loop input-channel connected-clients))

(defn handle-message [received-message input-channel connected-clients]
  (match received-message
         [:new-client username out-channel]
         (assoc connected-clients username out-channel)

         [:chat username message]
         (do
           (doseq [[_ client] (dissoc connected-clients username)]
             (>! client (str "chat:" username ": " message)))
           connected-clients)

         :start-game
         (do
           (start-game input-channel connected-clients)
           connected-clients)

         nil nil

         :else connected-clients))

(defn lobby-handler [input-channel]
  (go-loop [connected-clients {}]
    (let [received-message (<! input-channel)]
      (when-let [new-clients (handle-message received-message
                                             input-channel
                                             connected-clients)]
        (recur new-clients)))))

(defn receive-data [{:keys [lobby username] :as conn} lobby-channel data]
  (let [[message-type message-content] (str/split data #":" 2)]
    (case message-type
      "chat" (put! lobby-channel [:chat username message-content])
      "start game" (put! lobby-channel :start-game)
      "key down" (put! lobby-channel [:key-down username message-content])
      "key up" (put! lobby-channel [:key-up username message-content])
      (println (str \( lobby \) " Received message of unknown type: "
                    data)))))

(defn channel-for-lobby! [lobby-name]
  (dosync (if (contains? @lobby-channels lobby-name)
            (get @lobby-channels lobby-name)
            (let [new-chan (chan)]
              (alter lobby-channels assoc lobby-name new-chan)
              (println "Starting chat handler for" lobby-name)
              (lobby-handler new-chan)
              new-chan))))

(defn socket-handler [lobby-name username request]
  (httpkit/with-channel request channel
    (let [conn {:lobby lobby-name, :username username}
          lobby-channel (channel-for-lobby! lobby-name)
          my-channel (chan)]
      (printf "(%s) %s connected\n" lobby-name username)
      (put! lobby-channel [:new-client username my-channel])
      (go-loop []
        (httpkit/send! channel (<! my-channel))
        (recur))
      (httpkit/on-close channel (fn [_] (println username "diconnected")))
      (httpkit/on-receive channel (partial receive-data conn lobby-channel)))))

(defroutes app
  (GET "/" [] (index))
  (GET "/ws/:lobby/:username" [lobby username]
       (partial socket-handler lobby username))
  (route/files "/" {:root frontend-path})
  (route/not-found "<h1>Page not found</h1>"))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server []
  (reset! server (httpkit/run-server #'app {:port 30000})))

(defn restart-server []
  (@server)
  (start-server))

(defn -main
  "Starts the web server"
  [& args]
  (start-server))

#_(
   (restart-server)
   )
