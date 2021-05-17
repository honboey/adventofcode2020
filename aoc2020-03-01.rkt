;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname aoc2020-03-01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==========================================
;; Day 3: Toboggan Trajectory – Part 1
;; ==========================================


;; Data Definitions
;; ==========================================


;; Row
;; ===

;; Row is String
;; interp. a row consisting of open squares (.) and trees (#)
(define R1 "..##.......")

#;
(define (fn-for-row r)
  (... r))
;; Rules used:
;;  - Atomic non-distinct: String


;; ListOfRows
;; ==========

;; ListOfRows is one of:
;;  - empty
;;  - (cons Row ListOfRows)
;; interp a list of rows

(define LOR0 empty)  
(define LOR1 (cons "..##......." empty))                      ; (list "..##.......") 
(define LOR2 (cons "#...#...#.." (cons "..##......." empty))) ; (list "#...#...#.." "..##.......")

#;
(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]                   
        [else (... (first lor)                 
                   (fn-for-lor (rest los)))])) 
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Row ListOfRows)
;;  - self-reference: (rest lor) is ListOfRows


;; Position
;; ========

(define-struct position (x y))
;; Position is (make-position Natural Natural)
;; interp. the position of the tobaggan with x the column number, y the row number
(define P0 (make-position 0 0)) ; start of map
(define P1 (make-position 3 4)) ; column 4, row 3

#;
(define (fn-for-position p)
  (... (position-x)
       (position-y)))
;; Template rules used:
;; - compound: 2 fields


;; ListOfPositions
;; ===============

;; ListOfPositions is one of:
;;  - empty
;;  - (cons Position ListOfPositions)
(define LOP0 empty)
(define LOP1 (cons (make-position 0 0) empty))
(define LOP2 (cons (make-position 3 4) (cons (make-position 0 0) empty)))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else (... (fn-for-position (first lop))
                   (fn-for-lop (rest lop)))]))
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: 2 fields
;;  - reference: first lop
;;  - self-reference: rest lop


;; Data Definitions
;; ==========================================


;; countLoT
;; ========

;; ListOfRows -> Natural
;; Get the ListOfRows and determine the number of trees the tobaggan lands on
(check-expect (countLoT empty) 0)
(check-expect (countLoT (list "..##......."
                              "#...#...#.."
                              ".#....#..#."
                              "..#.#...#.#"
                              ".#...##..#."
                              "..#.##....."
                              ".#.#.#....#"
                              ".#........#"
                              "#.##...#..."
                              "#...##....#"
                              ".#..#...#.#")) 7)

; (define (countLoT lor) 0) ; stub

(define (countLoT lor)
  (length (makeLoT (reverse (makeLoP (length lor))) lor)))


;;; numOfRows
;;; =========
;
;;; ListOfRows -> Natural
;;; interp. determine number of rows in a list
;(check-expect (numOfRows empty) 0)
;(check-expect (numOfRows (list "..##......."
;                              "#...#...#.."
;                              ".#....#..#."
;                              "..#.#...#.#"
;                              ".#...##..#."
;                              "..#.##....."
;                              ".#.#.#....#"
;                              ".#........#"
;                              "#.##...#..."
;                              "#...##....#"
;                              ".#..#...#.#")) 11)
;
;; (define (numOfRows lor) 0) ; stub
;
;(define (numOfRows lor)
;  (length lor))


;; makeLoP
;; =======

;; Natural -> ListOfPositions
;; Consume a natural and create n number of positions in list
(check-expect (makeLoP 0)  empty)
(check-expect (makeLoP 1)  (list (make-position 0  0)))
(check-expect (makeLoP 11) (list (make-position 30 10)
                                 (make-position 27 9)
                                 (make-position 24 8)
                                 (make-position 21 7)
                                 (make-position 18 6)
                                 (make-position 15 5)
                                 (make-position 12 4)
                                 (make-position 9 3)
                                 (make-position 6 2)
                                 (make-position 3 1)
                                 (make-position 0 0)))

; (define (makeLoP n) empty) ; stub

(define (makeLoP n)
  (cond [(zero? n) empty]                   
        [else
         (cons (make-position (- (* n 3) 3)
                              (- n 1))                
               (makeLoP (sub1 n)))]))




;; makeLoT
;; =======

;; ListOfPositions ListOfRows -> ListOfTrees
;; Create a list of trees based on where the tobaggan lands
;; !!!

(define (makeLoT lop lor) empty)




