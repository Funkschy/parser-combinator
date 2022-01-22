# parser-combinators

An extremely simple parser combinator library

## Usage

The library provides several matchers for common use cases, which you can combine as you wish.
When you call one of the matcher functions, it will return a function which expects a context
object.

### Simple Example
```clojure
(all-m (str-m "hello") (regex-m #"\s+") (str-m "world")) (context "hello    world"))
; -> {:success true, :value ["hello" "    " "world"], :ctx Context{:text "hello    world", :pos 14}}

(all-m (str-m "hello") (regex-m #"\s+") (str-m "world")) (context "hello"))
; -> {:success false, :value "expected \\s+", :ctx nil}
```

### IRC Example
```clojure
(defmacro defnode [node-name matcher]
  `(def ~node-name
     (parse/map-m ~matcher
                  (fn [value#] [~(keyword node-name) value#]))))

(def space  (parse/regex-m #" +"))
(def crlf  (parse/str-m "\r\n"))
(def colon (parse/str-m ":"))
(def letter (parse/regex-m #"[a-zA-Z]+"))
(def three-numbers (parse/regex-m #"[0-9][0-9][0-9]"))
(def hostname (parse/regex-m #"(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])"))

(defnode trailing (parse/regex-m #"[^\x00\r\n]*"))
(defnode middle (parse/regex-m #"[^:][^\x00\r\n ]*"))
(defnode host hostname)
(defnode servername hostname)
(defnode nick (parse/regex-m #"[a-zA-Z][a-zA-Z0-9\-\[\]\\`\^{}]*"))
(defnode user (parse/regex-m #"[^ \x00\r\n@]+"))

; <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
(defnode params
  (parse/optional-m
    (parse/all-m space
                 (parse/optional-m (parse/any-m (parse/all-m colon trailing)
                                                (parse/all-m middle #'example.irc/params))))))

; <command>  ::= <letter> { <letter> } | <number> <number> <number>
(defnode command (parse/any-m letter (parse/all-m three-numbers)))
; <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
(defnode prefix
  (parse/all-m (parse/any-m servername nick)
               (parse/all-m
                 (parse/optional-m (parse/all-m (parse/str-m "!") user))
                 (parse/optional-m (parse/all-m (parse/str-m "@") host)))))

; <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
(defnode message
  (parse/all-m
    (parse/optional-m
      (parse/all-m (parse/str-m ":")
                   prefix
                   space))
    command
    params
    crlf))

(defn conj-mk-list [old value]
  (cond
    (nil? old) value
    (coll? old) (conj old value)
    :else [old value]))

(defn select-irc-keys
  "make a map with the set of keywords as keys and the value in the coll directly
  after the keyword as value"
  [coll keywords]
  (reduce (fn [m [k v]]
            (if (keywords k)
              (update m k #(conj-mk-list % v))
              m))
          {}
          (partition 2 1 coll)))

(defn parse-message [line]
  (-> (message (->Context line 0))
       :value
       flatten
       (select-irc-keys #{:command :trailing :user :middle})
       (clojure.set/rename-keys {:trailing :content :middle :channel})))
```
