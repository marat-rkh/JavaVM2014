(ns main)

; task 1
(defmulti fact identity)

(defmethod fact 0 [_]  1)
(defmethod fact :default [num]
 (* num (fact (dec num))))

;(println (fact 5))

; task 2
(def history (atom []))

(defn transfer [src-acc dst-acc amount]
 (dosync
  (swap! history conj (str "transfer started, src: " (.toString @src-acc) ", dst: " (.toString @dst-acc)))
  (alter src-acc - amount)
  (alter dst-acc + amount)
  (swap! history conj (str "transfer finished, src: " (.toString @src-acc) ", dst: " (.toString @dst-acc)))))

(defn add-10000 [acc]
 (dosync
  (swap! history conj (str "add-10000 started, acc: " (.toString @acc)))
  (alter acc + 10000)
  (swap! history conj (str "add-10000 finished, acc: " (.toString @acc)))))

(def fst-acc (ref 5000))
(def snd-acc (ref 1000))

(def transfer-task
 (reify Runnable
  (run [this]
   (transfer fst-acc snd-acc 2000))))

(def add-to-fst-task
 (reify Runnable
  (run [this]
   (add-10000 fst-acc))))

;(let [fst-thread (Thread. transfer-task) snd-thread (Thread. add-to-fst-task)]
; (.start fst-thread)
; (.start snd-thread))
;
;(println "-history: ")
;(dorun (map println @history))
;(println "-first acc: " @fst-acc)
;(println "-second acc: " @snd-acc)

; task 3
(defmacro my-or
 ([] nil)
 ([x] x)
 ([x & rest]
  `(let [eval-res# ~x]
    (if eval-res#
     eval-res#
     (my-or ~@rest)))))

;(if (my-or false false false true (println "not printed"))
; (println "true encountered")
; (println "not printed"))

; task 4
(defmacro my-let [bindings & body]
(assert (vector? bindings) "bindings must be vector")
(assert (even? (count bindings)) "bindings vector must have even length")
`((fn [~@(take-nth 2 bindings)] ~@body) ~@(take-nth 2 (rest bindings))))

;(my-let [x 1 y "one"]
;        (println x)
;        (println y))