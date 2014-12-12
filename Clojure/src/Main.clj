(ns javavm.clojure)

; task 1
(defn call-twice [f arg]
 (f arg)
 (f arg)
 )

;(println "# task 1:")
;(call-twice println "hello")

; task 2
(defn read-show [filepath]
 (let [text (slurp filepath)]
  (println text)
  text)
 )

;(println "# task 2:")
;(def rs (read-show "Main.clj"))
;(println rs)

; task 3
(def cube-anonymous #(* % % %))

;(println (cube-anonymous 3))

; task 4
(defn rev-con [seq1 seq2]
 (concat (reverse seq1) (reverse seq2))
 )

;(println (rev-con (seq [1 2 3]) (seq '(7 8 9))))

; task 5
(defn seq-contains? [s elem]
 (if-let [fst (first s)]
  (if (= fst elem)
   true
   (recur (rest s) elem))
  false
  )
 )

;(println (seq-contains? '(1 2 3) 20))

; task 6
; pairs assumed ordered
(defn pairs [seq1 seq2]
 (println (set (for [s1 seq1, s2 seq2] (list s1 s2))))
 )

;(pairs [1 2] [1 1 3])

; task 7
(defn my-repeat
 ([elem n acc]
  (if (= n 0)
   (seq acc)
   (recur elem (- n 1) (conj acc elem)))
  )
 ([elem n] (my-repeat elem n '()))
 )

;(println (my-repeat :p 5))