(ns zen-web.httpkit
  (:require [zen.core :as zen]
            [org.httpkit.server :as httpkit]))

;; :worker-name-prefix {:type zen/string :zen/desc "Worker thread name prefix"}
;; :worker-pool        {:type zen/string :zen/desc "ExecutorService to use for request-handling (:thread, :worker-name-prefix, :queue-size are ignored if set)"}
;; :error-logger       {:type zen/string :zen/desc "Arity-2 fn (args: string text, exception) to log errors"}
;; :warn-logger        {:type zen/string :zen/desc "Arity-2 fn (args: string text, exception) to log warnings"}
;; :event-logger       {:type zen/string :zen/desc "Arity-1 fn (arg: string event name)"}
;; :event-names        {:type zen/string :zen/desc "map of HTTP-Kit event names to respective loggable event names"}
;; :server-header      {:type zen/string :zen/desc "The \"Server\" header. If missing, defaults to \"http-kit\", disabled if nil."}
