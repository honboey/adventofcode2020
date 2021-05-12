;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname aoc2020-02-01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==========================================
;; Day 2:Password Philosophy – Part 1
;; ==========================================

;; Data Definitions
;; ==========================================

;; Password
;;=========

(define-struct pw (rule password))
;; Password is (make-pw String String)
;; interp rule is the password polit
;;        password is the password

(define P1 (make-pw "1-3 a: " "abcde"))

#;
(define (fn-for-pw p)
  (... (pw-rule p)        ; String
       (pw-password p)))  ; String


;; Rule
;; ====

(define-struct rule (min max letter))
;; Rule is (make-rule Natural Natural String)
;; interp. min is allowed minimum occurence of the letter
;;         max is allowed maximium occurence of the letter
;;         letter is compulsory letter

(define R1 (make-rule 1 3 "b"))

(define (fn-for-rule r)
  (... (rule-min r)        ; Natural
       (rule-max r)        ; Natural
       (rule-letter r)))   ; String


;; ListOfPasswords
;; ===============

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
(check-expect (quantity (list (make-pw "1-3 a: " "abcde")      ; valid
                              (make-pw "1-3 b: " "cdefg")      ; not valid
                              (make-pw "2-9 c: " "cccccccc"))) ; valid
              2)

; (define (quantity lop) 0) ; stub

;; Template taken from Function Composition
(define (quantity lop)
  (length (validList lop)))


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
;; ======

;; Password -> Boolean
;; Consume a Password and check if it is valid
;; !!!
(check-expect (valid? (make-pw "1-3 a:  " "abcde"))                true)
(check-expect (valid? (make-pw "1-3 b:  " "cdefg"))                false)
(check-expect (valid? (make-pw "3-12 s: " "sskssssssssss"))        true)
(check-expect (valid? (make-pw "17-18 n: " "jhrnnzpxzngfqrntmnc")) false)
(check-expect (valid? (make-pw "17-18 n: " "nnnnnnnnnnnnnnnnnwn")) true)

; (define (valid? p) false) ; stub

;; Template taken from Password
(define (valid? p)
  (runTest (createRule (pw-rule p))       
           (pw-password p)))


;; createRule
;; ==========

;; String -> Rule
;; Get (pw-rule) and take out the first Number, the second Number and the letter (String)
(check-expect (createRule "1-3 a: ")    (make-rule  1  3 "a")) ;if 2nd = "-" and 4th = " " take out 1st   3rd   5th 
(check-expect (createRule "1-10 a: ")   (make-rule  1 10 "a")) ;if 2nd = "-" and 5th = " " take out 1st   3-4th 6th
(check-expect (createRule "17-18 n: ")  (make-rule 17 18 "n")) ;if 3rd = "-"               take out 1-2nd 4-5th 7th

; (define (createRule s) (make-rule  1  3 "a")) ; stub

(define (createRule s)
  (cond [q a]
        [q a]
        [q a]))


;; runTest
;; =======

;; ListOfRuleElements String -> Boolean
;; Turn ListOfRuleElements to a test to run against String (password)
;; !!!

(define (runTest r s) false)



