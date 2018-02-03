(ns frost-fighters.core
  (:require [clojure.core.async :as a :refer [>! <! >!! <!!
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

(defn chat-handler [input-channel]
  (go-loop [connected-clients {}]
    (let [received-message (<! input-channel)]
      (match received-message

             [:new-client username out-channel]
             (recur (assoc connected-clients username out-channel))

             [:chat username message]
             (do
               (doseq [[_ client] (dissoc connected-clients username)]
                 (>! client (str "chat:" username ": " message)))
               (recur connected-clients))

             nil nil

             :else (recur connected-clients)))))

;; (defn start-game [{:keys [lobby socket] :as conn}]
;;   (println "Starting game")
;;   (swap! server-state assoc-in [lobby :running] true)
;;   (let [connected-clients (keys (get @server-state lobby))]
;;     (send-to-all conn
;;                  (str "start game:"
;;                       (apply str (interpose " " connected-clients)))))
;;   (game/run-main-loop server-state conn))

(defn receive-data [{:keys [lobby username] :as conn} lobby-channel data]
  (let [[message-type message-content] (str/split data #":" 2)]
    (case message-type
      "chat" (put! lobby-channel [:chat username message-content])
      (println (str \( lobby \) " Received message of unknown type: "
                    data)))))

(defn channel-for-lobby! [lobby-name]
  (dosync (if (contains? @lobby-channels lobby-name)
            (get @lobby-channels lobby-name)
            (let [new-chan (chan)]
              (alter lobby-channels assoc lobby-name new-chan)
              (println "Starting chat handler for" lobby-name)
              (chat-handler new-chan)
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
