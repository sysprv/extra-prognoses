; vim:set ts=8 sts=2 sw=2 et ai:
; (load "f")
; (in-ns 'extra-prognoses)
; (def free-pattern (vec (first (take 1 (generate-valid-free-cell-indices)))))

(ns extra-prognoses
  (:require clojure.math.combinatorics [clojure.string :as string] [clojure.java.io :as io])
  (:use [clojure.math.combinatorics :only [combinations]]))

; an implementation of part of Python's random.sample
(defn next-unique-int
  "Return a unique int that's less than n and not already in selected"
  [n selected]
  (loop [j (rand-int n)]
    (if (selected j)
      (recur (rand-int n))
      j)))

(defn sample
  "Given a population, return k items from it."
  [population k]
  (let [n (count population)]
    (loop [selected (hash-set)
           result (vector)
           i 0]
      (if (< i k)
        (let [j (next-unique-int n selected)]
          (recur (conj selected j)
                 (conj result (nth population j))
                 (inc i)))
        result))))

; a shortcut - these functions always call sample
; with a population that's a contiguous series of numbers
; starting with 0.
(defn sample-1-to [max k]
  (sample (range max) k))


; the grid as a seq of 25 elements
; unused function - from back when i treated rows first.
; this was not the way to go.
(defn row-old
  "Given a row number (top to bottom), return indices for that row"
  [nr]
  (let [start-idx (* nr 5)
        end-idx (+ start-idx 5)
        indices (range start-idx end-idx)]
    indices))

(defn col-old
  "Given a column number (left to right), return indices for that column"
  [nr]
  (range nr (+ 21 nr) 5))


; the extra grid is columns-first.
; 0 5 10  15  20
; 1 6 11  16  21
; 2 7 12  17  22
; 3 8 13  18  23
; 4 9 14  19  24
;
; rules: there can be only 7 "F"s total in the whole grid.
; there must be 4 Fs in the inner 3x3 grid,
;   the other 3 Fs must be on the remaining 16 cells -- the frame.
; any column (top - bottom), row (left - right) or diagonal
; can only contain up to 2 Fs.

; given a row number, return all the vector indices.
; increases left to right.
(defn row-indices [nr]
  (let [start-idx nr
        end-idx (+ start-idx 20)
        step 5]
    (vec (range start-idx (inc end-idx) step))))


(defn column-indices [nr]
  (let [start-idx (* nr 5)
        end-idx (+ start-idx 4)]
    (vec (range start-idx (inc end-idx)))))


(defn diagonal-lr-indices []
  (vec (range 0 25 6)))

(defn diagonal-rl-indices []
  (vec (range 4 21 4))) ; down -> up, but that's ok

; given some indexes, return the list of values from the values
; object, which will typically be a seq, with extra selections
; (numbers + "F").
(defn structure-values [structure-indices values]
  (vec (map #(nth values %) structure-indices)))

(def inner-block-indices
  [ 6 7 8 11 12 13 16 17 18 ])

(def frame-indices
  [ 0 1 2 3 4 5 9 10 14 15 19 20 21 22 23 24 ])

; define a var with all the paths - rows, columns and the two diagonals.
(def structure-indices
  [ (row-indices 0) (row-indices 1) (row-indices 2) (row-indices 3) (row-indices 4)
    (column-indices 0) (column-indices 1) (column-indices 2) (column-indices 3) (column-indices 4)
    (diagonal-lr-indices) (diagonal-rl-indices) ])


(defn pp-row [selection rowno]
  (let [row-idxs (row-indices rowno)]
    (dorun ; do for side effects
      (map #(print (format "[%1$3s ]" (nth selection %))) row-idxs)) ; no trailing spaces please
    (prn)))


(defn pp-selection [selection]
  ; selections in column order. but print row by row.
  (dorun (map #(pp-row selection %) (range 5)))
  (prn)
  nil)


; free-cell-indices would be the list of indices
; where Fs have been placed.
; structure would be the indices of part of an Extra selection -
; for example, one diagonal row.
; This function checks how many Fs have been places inside the
; area of the given structure (row, column, inner grid, frame etc).
(defn free-cells [free-cell-indices structure-indices]
  (filter (fn [free-cell-index]
    (some #{free-cell-index} structure-indices)) free-cell-indices))

(defn count-free-cells [free-cell-indices structure-indices]
  ; structure could be a row, column,
  ; one of the diagonals, the frame or the
  ; inner block.
  ; a seq of vector indices.
  (count (free-cells free-cell-indices structure-indices)))


; predicates to filter out combinations that are not valid
; F patterns according to the game rules.

(defn free-cell-indices-valid?-prn [free-cell-indices]
  (let [f-count-border (count-free-cells free-cell-indices frame-indices)
        f-count-block (count-free-cells free-cell-indices inner-block-indices)
        f-count-per-path (map #(count-free-cells free-cell-indices %) structure-indices)
        max-f-count-path (apply max f-count-per-path)]
    (prn "f count border" f-count-border)
    (prn "f count block" f-count-block)
    (prn "max f of any path" max-f-count-path)))

(defn free-cell-indices-valid?-old [free-cell-indices]
  (if (not= 3 (count-free-cells free-cell-indices frame-indices))
    false
    (if (not= 4 (count-free-cells free-cell-indices inner-block-indices))
      false
      (if (not= 2 (apply max (map #(count-free-cells free-cell-indices %) structure-indices)))
        false
        true))))

(defn free-cell-indices-valid? [free-cell-indices]
  (if (not= 3 (count-free-cells free-cell-indices frame-indices))
    false
    (if (not= 4 (count-free-cells free-cell-indices inner-block-indices))
      false
      ;; the up-down/left-right/diagonal F counts don't matter any more.
      true)))


(defn all-possible-free-patterns []
  ; frame and inner block together
  (combinations (range 25) 7))

; for testing...
(defn a-few-free-points []
  (take 10 (all-possible-free-patterns)))

; use (take n) on the output of this to limit results
; fun - 14248 such valid (+ unique) combinations exist.
(defn generate-valid-free-cell-indices []
  (filter free-cell-indices-valid? (all-possible-free-patterns)))


;
; these functions are called "wasteful" because they were meant to
; generate more numbers than needed (sample 1..75 25) and then
; replace some with "F"s.
; later i implemented other functions to generate "F"s first
; and fill in numbers.
;

(defn make-one-column-wasteful [column-number]
  ; The range (1-75) of the complete grid
  ; is split between the columns.
  ; each column is restricted to its own range..
  ; col 0 -> 1..15,
  ; col 1 -> 16..30 etc.
  ; This range is common to the frame and the
  ; inner square.
  (let [start (+ 1 (* column-number 15))
        stop (+ start 14)
        column-range (range start (inc stop))
        column-sample (sample column-range 5)]
    ; (prn column-number start stop)
    (sort column-sample)))

(defn make-columns-wasteful []
  (map make-one-column-wasteful (range 5)))

(defn make-selection [columns]
  (flatten columns))

(defn make-rows [columns]
  (apply mapv vector columns))

(defn apply-free-points [number-grid free-points]
  (map-indexed (fn [idx itm]
    (if (some #{idx} free-points)
      "F"
      itm)) number-grid))

(defn print-one-selection []
  (let [selection (make-selection (make-columns-wasteful))
        free-points (first (take 1 (generate-valid-free-cell-indices)))
        selection-with-free (apply-free-points selection free-points)]
    (pp-selection selection-with-free)))

(defn print-some-selections []
  (let [free-points (take 10 (generate-valid-free-cell-indices))]
    (dorun (map (fn [f-p]
                  (let [sel (make-selection (make-columns-wasteful))
                    selection-with-free (apply-free-points sel f-p)]
                    (pp-selection selection-with-free)
                    (prn))) free-points))))



(defn numbers-for-column
  "Given a column number and the number of required numbers (minus any Fs),
  return a sampling from the number range allocated to this column."
  [column-number number-count]
  (let [start (+ 1 (* column-number 15))
        stop (+ start 14)
        column-range (range start (inc stop))
        column-sample (sample column-range number-count)]
    column-sample))


(defn make-column-after-F
  "Given a column number and a (not necessarily valid) set of indices
  giving the locations of the Fs, return a complete column that's
  populated with Fs (if any) and numbers."
  [column-number free-pattern]
  (let [F-count (count-free-cells free-pattern (column-indices column-number))
        number-count (- 5 F-count)
        numbers (numbers-for-column column-number number-count)]
    (loop [result-column (vector)  ; shouldn't be a set; there can be two "F"s.
           remaining-numbers numbers
           i (* column-number 5)] ; i <- start _index_ for this column. see column-rows, f.ex.
      ; (prn i (some #{i} free-pattern) free-pattern)
      (if (= (count result-column) 5)
        result-column
        (if (some #{i} free-pattern)
          (recur (conj result-column "F")                       remaining-numbers        (inc i))
          (recur (conj result-column (first remaining-numbers)) (rest remaining-numbers) (inc i)))))))



(defn make-columns-after-F [free-pattern]
  (map #(make-column-after-F % free-pattern) (range 5)))

; (make-column-after-F 0 free-pattern)
; (make-columns-after-F free-pattern)


; reimplemented flatten
; visiting columns top to bottom, left to right
; (defn join-columns [columns]
;   (loop [result (vector)
;          cols columns]
;     (if (empty? cols)
;       result
;       (recur (concat result (first cols)) (rest cols)))))

(defn generate-unique-selection-seqs-with-random-f-patterns [selection-count f-patterns]
  (let [f-pattern-count (count f-patterns)]
    (loop [sels (hash-set)]
      (if (= (count sels) selection-count)
        sels
        (recur (conj sels (flatten (make-columns-after-F (nth f-patterns (rand-int f-pattern-count))))))))))


(defn generate-ss-selections [selection-count free-pattern-count]
  (let [valid-f-patterns (take free-pattern-count (generate-valid-free-cell-indices))]
    (map #(string/join ";" %) (generate-unique-selection-seqs-with-random-f-patterns selection-count valid-f-patterns))))


; (write-ss-selections-to-file "/dev/tty" 1 1)
(defn write-ss-selections-to-file [filename selection-count free-pattern-count]
  (with-open [cout (io/writer filename)]
    (dorun
      (map #(.write cout (str % "\n")) (generate-ss-selections selection-count free-pattern-count)))))
        

; pretty display
(defn -main []
  (let [f-pattern-count 10
        valid-f-patterns (take f-pattern-count (generate-valid-free-cell-indices))
        selection-count 10
        selections (loop [sels (hash-set)]
                     (if (= (count sels) selection-count)
                       sels
                       (recur (conj sels (flatten (make-columns-after-F (nth valid-f-patterns (rand-int f-pattern-count))))))))]
    (dorun
      (map (fn [sel] (prn (string/join ";" sel)) (pp-selection sel)) selections)))
  0)

