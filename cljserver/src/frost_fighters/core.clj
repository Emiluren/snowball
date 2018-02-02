(ns frost-fighters.core
  (:require [ring.util.response :refer [file-response]]
            [org.httpkit.server :as httpkit]
            [compojure.core :refer [defroutes GET PUT]]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.string :as str])
  (:gen-class))

(def frontend-path "../public")

(defn index []
  (file-response "index.html" {:root frontend-path}))

(defonce server (atom nil))

(defonce server-state (atom {}))

(defn connect-user [lobby username socket]
  (swap! server-state assoc-in [lobby username] socket)
  (println (str \( lobby \) " User " username " has connected")))

(defn disconnect-user [lobby username _]
  (swap! server-state update lobby dissoc username)
  (println (str \( lobby \) " User " username " has disconnected")))

(def message-handlers
  {"chat"
   (fn [lobby username message-content]
     (doseq [[_ client] (dissoc (get @server-state lobby) username)]
       (httpkit/send! client (str "chat:" username ": " message-content))))})

(defn receive-data [lobby username data]
  (let [[message-type message-content] (str/split data #":" 2)]
    (if-let [handler (get message-handlers message-type)]
      (handler lobby username message-content)
      (println (str \( lobby \) " Received message of unknown type: "
                    data)))))

(defn socket-handler [lobby username request]
  (httpkit/with-channel request channel
    (connect-user lobby username channel)
    (httpkit/on-close channel
                      (partial disconnect-user lobby username))
    (httpkit/on-receive channel
                        (partial receive-data lobby username))))

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
