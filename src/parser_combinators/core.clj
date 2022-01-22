(ns parser_combinators.core
  (:require [clojure.string :as str]
            [parser_combinators.parse :refer :all]))

(defn context [s]
  (->Context s 0))

(defn optional-m [matcher]
  (fn [ctx]
    (let [result (matcher ctx)]
      (if (error? result)
        (success [] ctx)
        result))))

(defn str-m [string]
  (fn [ctx]
    (if (str/starts-with? (at-pos ctx) string)
      (success string (after ctx (count string)))
      (error   (str "expected " string)))))

(defn regex-m [regex]
  (fn [ctx]
    (let [fst   #(if (coll? %) (first %) %)
          match (-> (str "^" regex) re-pattern (re-find (at-pos ctx)) fst)]
      (if match
        (success match (after ctx (count match)))
        (error   (str "expected " regex))))))

(defn all-m [& matcher]
  (fn [ctx]
    (letfn [(short-circuit [matcher result]
              (if (error? result)
                (reduced result)
                (combine result (matcher (:ctx result)))))]
      (reduce (fn [result matcher] (short-circuit matcher result))
              (success nil ctx)
              matcher))))

(defn any-m [& matcher]
  (fn [ctx]
    (or (first (filter (comp not error?) (map #(% ctx) matcher)))
        (error "no matcher matched"))))

(defn map-m [matcher f]
  (fn [ctx]
    (let [result (matcher ctx)]
      (if (error? result)
        result
        (success (f (:value result)) (:ctx result))))))
