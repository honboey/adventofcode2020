;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname aoc2020-02-01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==========================================
;; Day 2:Password Philosophy – Part 1
;; ==========================================

;; Data Definitions
;; ==========================================

(define-struct pw (rule password))
;; Password is (make-pw String String)
;; interp rule is the password polit
;;        password is the password

(define PW1 (make-pw "1-3 a: " "abcde"))

#;
(define (fn-for-pw pw)
  (... (pw-rule pw)        ; String
       (pw-password pw)))  ; String


;; ListOfPasswords is one of:
;; - empty
;; - (cons Password ListOfPasswords)
;; interp: a list of Passwords
(define LOP0 empty)
(define LOP1 (cons (make-pw "1-3 a: " "abcde") empty))
(define LOP2 (cons (make-pw "1-3 b: " "cdefg") (cons (make-pw "1-3 a: " "abcde") empty)))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-pw  (first lop))
              (fn-for-lop (rest lop)))]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Password ListOfPasswords)
;; - self-reference: (rest lop) is ListOfPasswords


;; Function Definitions
;; ==========================================

;; ListOfPasswords -> Natural
;; The number of valid passwords in ListOfPasswords

(check-expect (quantity empty) 0)
(check-expect (quantity (list (make-pw "1-3 a: " "abcde")
                              (make-pw "1-3 b: " "cdefg")
                              (make-pw "2-9 c: " "ccccccccc")))
              2)

; (define (quantity lop) 0) ; stub
;; Template taken from Function Composition
(define (quantity lop)
  (countEntries (validList lop)))


;; validList
;; =========
;; ListOfPasswords -> ListOfPasswords
;; A list of valid passwords
(check-expect (validList empty) empty)
(check-expect (validList (list (make-pw "1-3 a: " "abcde")
                               (make-pw "1-3 b: " "cdefg")
                               (make-pw "2-9 c: " "ccccccccc")))
              (list (make-pw "1-3 a: " "abcde")
                    (make-pw "2-9 c: " "ccccccccc")))

; (define (validList lop) empty) ; stub
(define (validList lop)
  (cond [(empty? lop) empty]
        [else
         (if (valid? (first lop))
             (cons (first lop) (validList (rest lop)))
             (validList (rest lop)))]))


;; valid?
;; ==============
;; Password -> Boolean
;; Consume a Password and check if it is valid
;; !!!

(define (valid? lop) false)


;; countEntries
;; ============
;; ListOfPasswords -> Natural
;; Then number of items in the valid ListOfPasswords
;; !!!

(define (countEntries lop) 0)

