(ns frost-fighters.core
  (:require [clojure.core.async :as a :refer [>! <! >!! <!! go chan timeout]]
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

(defonce server-state (atom {}))

(defn connect-user [{:keys [lobby username socket]}]
  (swap! server-state assoc-in [lobby username] socket)
  (println (str \( lobby \) " User " username " has connected")))

(defn disconnect-user [{:keys [lobby username]} _]
  (swap! server-state update lobby dissoc username)
  (println (str \( lobby \) " User " username " has disconnected")))

(defn send-to-all [{:keys [lobby username]} data]
  (doseq [[_ client] (dissoc (get @server-state lobby) :running)]
    (httpkit/send! client data)))

(def message-handlers
  {"chat"
   (fn [{:keys [lobby username]} message-content]
     (doseq [[_ client] (dissoc (get @server-state lobby) username :running)]
       (httpkit/send! client (str "chat:" username ": " message-content))))
   "start game"
   (fn [{:keys [lobby socket] :as conn} _]
     ;; TODO: Move running inside a swap to get rid of race condition!
     (if (get-in @server-state [lobby :running])
       (httpkit/send! socket
                      (str "chat:(Server) " lobby " is already playing"))
       (do
         (println "Starting game")
         (swap! server-state assoc-in [lobby :running] true)
         (let [connected-clients (keys (get @server-state lobby))]
           (send-to-all conn
                        (str "start game:"
                             (apply str
                                    (interpose " " connected-clients)))))
         (game/run-main-loop server-state conn))))})

(defn receive-data [{:keys [lobby username] :as conn} data]
  (let [[message-type message-content] (str/split data #":" 2)]
    (if-let [handler (get message-handlers message-type)]
      (handler conn message-content)
      (println (str \( lobby \) " Received message of unknown type: "
                    data)))))

(defn socket-handler [lobby username request]
  (httpkit/with-channel request channel
    (let [conn {:lobby lobby, :username username, :socket channel}]
      (connect-user conn)
      (httpkit/on-close channel (partial disconnect-user conn))
      (httpkit/on-receive channel (partial receive-data conn)))))

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
