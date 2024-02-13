(ns shadow.grove.runtime
  (:require [goog.async.nextTick]))

(defonce known-runtimes-ref (atom {}))

(defn ref? [x]
  (and (atom x)
       (::rt @x)))

(defonce id-seq (volatile! 0))

(defn next-id []
  (vswap! id-seq inc))

(defonce ticker (js/Promise.resolve nil))

(defn next-tick [callback]
  (js/goog.async.nextTick callback))

(defn microtask [callback]
  (.then ticker callback))
