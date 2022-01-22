(ns parser_combinators.parse)

(defprotocol Sliceable
  (at-pos [this]        "the current slice")
  (after  [this offset] "the slice starting at offset"))

(defprotocol ErrorCheck
  (error? [this] "is this an error?"))

(defprotocol Combinable
  (combine [this that] "merge 2 values"))

(defrecord Context [text pos]
  Sliceable
  (at-pos [this]        (subs (:text this) (:pos this)))
  (after  [this offset] (Context. text (+ offset (:pos this)))))

(defn larger-ctx [res-1 res-2]
  (max-key :pos (:ctx res-1) (:ctx res-2)))

(defn join-values [res-1 res-2]
  (let [val-1 (:value res-1)
        val-2 (:value res-2)]
    (cond
      (and (coll? val-1) (not (nil? val-2))) (conj val-1 val-2)
      (and (coll? val-2) (not (nil? val-1))) (conj val-2 val-1)
      :else (into [] (remove nil? [val-1 val-2])))))

(defrecord Result [success value ctx]
  ErrorCheck
  (error? [this] (not (:success this)))

  Combinable
  (combine [this that]
    (cond
      (error? this) this
      (error? that) that
      :else         (Result. true (join-values this that) (larger-ctx this that)))))

(defn success [value new-ctx] (Result. true value new-ctx))
(defn error [value] (Result. false value nil))
