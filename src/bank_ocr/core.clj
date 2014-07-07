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
(def INVALID_CHARACTER \?)

(def char-map
  {:0 ZERO
   :1 ONE
   :2 TWO
   :3 THREE
   :4 FOUR
   :5 FIVE
   :6 SIX
   :7 SEVEN
   :8 EIGHT
   :9 NINE})

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


(defn to-scan-char [digit]
  ((keyword (str digit)) char-map)
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
   :else INVALID_CHARACTER
   )
  )

(defn to-scan-chars [scan-lines]
  (for [i (range 9)]
    (to-matrix scan-lines i))
  )

(defn convert [scan-lines ncols-per-char]
  (let [ncols  (count  (clojure.string/trim-newline (first scan-lines)))
        nchars-per-line (/ ncols ncols-per-char)
        scan-chars (to-scan-chars scan-lines)]

    (apply str  (for [s scan-chars]
                  (to-digit s)
                  ))
    
    )
  )

(defn checksum [acct-number]
  (loop [sum 0
         value acct-number
         pos 1]
    
    (cond
     (empty? value) (mod sum 11)
     :else (recur (+ sum (* (-  (int (last value)) (int \0)) pos)) (drop-last value) (inc pos)) )
    )
  )

(defn is-valid? [acct-number]
  (cond
   (nil? acct-number) false
   (empty? acct-number) false
   :else
     (= 0 (checksum acct-number))
   )
  )

(defn is-illegible? [acct-number]
  (some #(= INVALID_CHARACTER %) acct-number))

(defn create-log-entry-for [acct-number]
  (cond
   (is-illegible? acct-number) (str acct-number " ILL")
   (is-valid? acct-number) acct-number
   :else (str acct-number " ERR")
   )
  )

(defn replace-at-posn-with [acct-number pos c]
  (let [p1 (apply str (take pos acct-number))
        p3 (apply str (drop (inc pos) acct-number))]

    (apply str p1 c p3)
    )
  )

(defn construct-alternates-with [acct-number c]
  (map #(replace-at-posn-with acct-number % c) (range (count acct-number)))
  )


(defn diff [s1 s2]
  (loop [n 0
         s1 (flatten s1)
         s2 (flatten s2)]
    
    (cond
     (empty? s1) n
     (not= (first s1) (first s2)) (recur (inc n) (drop 1 s1) (drop 1 s2))
     :else (recur n (drop 1 s1) (drop 1 s2))
     )
    )
  )

(defn find-chars-close-to [scan-char]
  (filter #(= 1 ( diff scan-char (val %))) char-map )
  )

(defn find-valid-alternates [acct-number]

   (for [n (range 10)]
      (filter #(is-valid? %) (construct-alternates-with acct-number n))
    )
  )

(defn to-digits [scan-chars]
  (apply str 
         (for [s scan-chars]
           (to-digit s)))
  )

(defn to-account-number [p1 c p2]
  (let [a1 (to-digits p1)
        ac (to-digits [c])
        a2 (to-digits p2)]
    (apply str a1 ac a2)
    )
  )

(defn construct-alternate-numbers-at-posn [scan-chars posn]
  (let [r (find-chars-close-to (nth scan-chars posn))
        p1 (vec  (take posn scan-chars))
        p2 (vec  (drop (inc posn) scan-chars))]

    (for [c (vals r)]
      (to-account-number p1 c p2)
      )
    )
  )


(defn convert-and-replace [scan-lines n]
  (let [scan-chars (to-scan-chars scan-lines)
        acct-num (convert scan-lines n)]

    (flatten 
     (for [i (range (count scan-chars))]
       (filter #(is-valid? %) (construct-alternate-numbers-at-posn scan-chars i))
       ))
    
    )
  
  )
