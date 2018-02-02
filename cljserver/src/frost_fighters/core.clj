(ns frost-fighters.core
  (:require [ring.util.response :refer [file-response]]
            [org.httpkit.server :refer [run-server with-channel on-close on-receive websocket? send!]]
            [compojure.core :refer [defroutes GET PUT]]
            [compojure.route :as route]
            [compojure.handler :as handler])
  (:gen-class))

(def frontend-path "../public")

(defn index []
  (file-response "index.html" {:root frontend-path}))

(defonce server (atom nil))

(defn socket-handler [lobby username request]
  (with-channel request channel
    (on-close channel (fn [status]
                        (println "channel closed")))
    (if (websocket? channel)
      (println "WebSocket channel")
      (println "HTTP channel"))
    (on-receive channel (fn [data]
                          (send! channel data)))))

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
  (reset! server (run-server #'app {:port 30000})))

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
