(ns clojure-joust.core-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [clojure-joust.core :as core]))

(deftest multiply-test
  (is (= (* 1 2) (* 1 2))))
