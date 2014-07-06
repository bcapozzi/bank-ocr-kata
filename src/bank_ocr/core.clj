(ns bank-ocr.core)


(def ZERO '((\space \_ \space) (\| \space \|) (\| \_ \|) (\space \space \space)))
(def ONE '((\space \space \space) (\space \space \|) (\space \space \|) (\space \space \space)))
(def TWO '((\space \_ \space) (\space \_ \|) (\| \_ \space) (\space \space \space)))
(def THREE '((\space \_ \space) (\space \_ \|) (\space \_ \|) (\space \space \space)))
(def FOUR '((\space \space \space) (\| \_ \|) (\space \space \|) (\space \space \space)))
(def FIVE '((\space \_ \space) (\| \_ \space) (\space \_ \|) (\space \space \space)))
(def SIX '((\space \_ \space) (\| \_ \space) (\| \_ \|) (\space \space \space)))
(def SEVEN '((\space \_ \space) (\space \space \|) (\space \space \|) (\space \space \space)))
(def EIGHT '((\space \_ \space) (\| \_ \|) (\| \_ \|) (\space \space \space)))
(def NINE '((\space \_ \space) (\| \_ \|) (\space \_ \|) (\space \space \space)))

(defn is-mark? [s]
  (println s)
  (or (= \| s) (= \_ s))
  )


(defn perturb-line [scan-line]
  (println "line in: ", scan-line)
  (loop [nchanges 0
         data scan-line
         out []]
    (println nchanges, data, out)
    (cond
     (= 1 nchanges) (flatten  (sequence (conj out data)))
     (empty? data) out
     (is-mark? (first data)) (recur (inc nchanges) (drop 1 data) (conj out \space))
     :else (recur nchanges (drop 1 data) (conj out (first data)))
     )
    )
  )


(defn contains-mark? [scan-line]
  (let [n (count (filter #(is-mark? %) scan-line))]
    (> n 0)
    )
  )

(defn perturb [scan-char n]

  ;; iterate over lines
  ;; if contains a '|' or a '_', then replace with a space
  (loop [data scan-char
         out []
         nchanges 0]
    (println data, out, nchanges)
    (cond
     (empty? data) (sequence  out) 
     (= 1 nchanges) (recur (drop 1 data) (conj out (first data)) nchanges)
     (contains-mark? (first data)) (recur (drop 1 data) (conj out (perturb-line (first data))) (inc nchanges))
     :else (recur (drop 1 data) (conj out (first data)) nchanges)
     )
    )
  )

(defn to-matrix [scan-lines n]

  (for [line scan-lines]
    (take 3 (drop (* n 3) line)))
  )


(defn to-digit [s]

  (cond
   (= ZERO s) "0"
   (= ONE s) "1"
   (= TWO s) "2"
   (= THREE s) "3"
   (= FOUR s) "4"
   (= FIVE s) "5"
   (= SIX s) "6"
   (= SEVEN s) "7"
   (= EIGHT s) "8"
   (= NINE s) "9"
   :else "-1"
   )
  )


(defn convert [scan-lines ncols-per-char]
  (let [ncols  (count  (clojure.string/trim-newline (first scan-lines)))
        nchars-per-line (/ ncols ncols-per-char)
        scan-chars (for [i (range 9)](to-matrix scan-lines i)) ]

    (apply str  (for [s scan-chars]
                  (to-digit s)
                  ))
    
    )
  )

