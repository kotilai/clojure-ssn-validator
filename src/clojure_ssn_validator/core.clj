(ns clojure-ssn-validator.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:import (java.text SimpleDateFormat)))

; Validates Finnish Social Security Numbers (SSN)
; The valid form is <date><century><sequence><checksum> e.g. 040784-522R
; <date>     --> date of birth without century, ddMMyy
; <century>  --> date of birth century as a symbol e.g. 1900 = '-'
; <sequence> --> indexes persons born in the same day, between 2 - 899 and odd in males
; <checksum> --> modulo 31 from <date> and <sequence> parts

(def checksum-map {
  "0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "A" 10
  "B" 11 "C" 12 "D" 13 "E" 14 "F" 15 "H" 16 "J" 17 "K" 18 "L" 19 "M" 20
  "N" 21 "P" 22 "R" 23 "S" 24 "T" 25 "U" 26 "V" 27 "W" 28 "X" 29 "Y" 30})

(def centuries-map {
  "+" "18" "-" "19" "A" "20"})

(defn split-to-map [keywords indices string]
  {:pre [(= (* 2 (count keywords)) (count indices))]}
  (zipmap
    keywords
    (map #(subs string (first %) (second %)) (partition 2 indices))))

(defn validate-pattern [ssn]
  (if (re-find #"\d{6}[\+|\-|A]\d{3}\w" ssn)
    true
    false))

(defn parse-ssn [ssn-string]
  (if (validate-pattern ssn-string)
    (->> ssn-string
         (str/upper-case)
         (split-to-map [:date :cent :seq :check] [0 6 6 7 7 10 10 11]))
    nil))

(defn is-date? [date-str]
  (try
    (doto (SimpleDateFormat. "ddMMyyyy")
      (.setLenient false)
      (.parse date-str))
    true
  (catch Exception _
    false)))

(defn str-into [to from pos]
  (str (subs to 0 pos) from (subs to pos)))

(defn validate-date [ssn]
  (is-date? (str-into (:date ssn) (centuries-map (:cent ssn)) 4)))

(defn validate-sequence [ssn]
  (<= 2 (bigint (:seq ssn)) 899))

(defn validate-checksum [ssn]
  (= (mod (bigint (str (:date ssn) (:seq ssn))) 31) (checksum-map (:check ssn))))

(defn -main [ssn-string]
  (if-let [ssn (parse-ssn ssn-string)]
    (and
      (validate-date ssn)
      (validate-sequence ssn)
      (validate-checksum ssn)
      true)
    false))

