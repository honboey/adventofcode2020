;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname aoc2020-02-02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==========================================
;; Day 2:Password Philosophy – Part 1
;; ==========================================

;; Data Definitions
;; ==========================================

;; Password
;; ========

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

#;
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
(define LOP3 (list (make-pw "1-3 b: " "cdefg") (make-pw "2-5 b: " "defg")))

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
(check-expect (quantity (list (make-pw "4-6 b: " "bbbdbtbbbj") 
                              (make-pw "1-6 g: " "ggvggbgggstg")
                              (make-pw "1-4 s: " "lsssss")))
              0)


; (define (quantity lop) 0) ; stub

;; Template taken from Function Composition
(define (quantity lop)
  (length (validList lop)))


;; validList
;; =========

;; ListOfPasswords -> ListOfPasswords
;; Consume ListOfPasswords and produce a list of valid passwords
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
  (runTest (pullApartRule (pw-rule p))        ; String
           (pw-password p)))                  ; String


;; pullApartRule
;; =============

;; String -> Rule
;; Get (pw-rule) and take out the first Number, the second Number and the letter (String)
(check-expect (pullApartRule "1-3 a: ")    (make-rule  1  3 "a")) ;if 2nd = "-" and 4th = " " take out 1st   3rd   5th 
(check-expect (pullApartRule "1-10 a: ")   (make-rule  1 10 "a")) ;if 2nd = "-" and 5th = " " take out 1st   3-4th 6th
(check-expect (pullApartRule "17-18 n: ")  (make-rule 17 18 "n")) ;if 3rd = "-"               take out 1-2nd 4-5th 7th

; (define (pullApartRule s) (make-rule  1  3 "a")) ; stub

(define (pullApartRule s)
  (cond [(and (string-ci=? "-" (string-ith s 1))
              (string-ci=? " " (string-ith s 3)))         ;if 2nd = "-" and 4th = " " take out 1st   3rd   5th
         (make-rule (string->number (string-ith s 0))
                    (string->number (string-ith s 2))
                    (string-ith s 4))]
        [(and (string-ci=? "-" (string-ith s 1))
              (string-ci=? " " (string-ith s 4)))         ;if 2nd = "-" and 5th = " " take out 1st   3-4th 6th
         (make-rule (string->number (string-ith s 0))
                    (string->number (substring s 2 4))
                    (string-ith s 5))]
        [(string-ci=? "-" (string-ith s 2))               ;if 3rd = "-" take out 1-2nd 4-5th 7th
         (make-rule (string->number (substring s 0 2))
                    (string->number (substring s 3 5))
                    (string-ith s 6))]))



;; runTest
;; =======

;; Rule String -> Boolean
;; Consume a Rule and test against String (password)
(check-expect (runTest (make-rule  1  3 "a") "abcde")         true)
(check-expect (runTest (make-rule  1 10 "a") "aaaaaaaaaava") false)

; (define (runTest r s) false) ; stub

;; Template taken from Function Composition

(define (runTest r s)
  (and (>= (charCount (rule-letter r) s) (rule-min r))
       (<= (charCount (rule-letter r) s) (rule-max r))))



;; charCount
;; =========

;; String String -> Number
;; Count the number of occurences of a certain letter in a string
(check-expect (charCount "a" "b")  0)
(check-expect (charCount "a" "a")  1)
(check-expect (charCount "a" "ba") 1)
(check-expect (charCount "a" "bc") 0)

; (define (charCount s1 s2) 1) ;

(define (charCount s1 s2)
  (length (filterList s1 s2)))


;; filterList
;; ==========

;; String String -> ListOfChar
;; Create a ListOfChar of a specific letter in a String
(check-expect (filterList "a" "a") (list #\a))
(check-expect (filterList "a" "b") empty)
(check-expect (filterList "a" "ab") (list #\a))
(check-expect (filterList "a" "aba") (list #\a #\a))

; (define (filter s1 s2) empty) ; stub

(define (filterList s1 s2)
  (filter s1 (string->list s2)))


;; filter
;; ======

;; String ListOfChar -> ListOfChar
;; Remove the instances of String from ListOfChar
;; !!!
(check-expect (filter "a" empty)               empty)
(check-expect (filter "a" (list #\a))          (list #\a))
(check-expect (filter "a" (list #\b))          empty)
(check-expect (filter "a" (list #\a #\b))      (list #\a))
(check-expect (filter "a" (list #\a #\b #\a))  (list #\a #\a))
(check-expect (filter "a" (list #\b #\b #\a))  (list #\a))

; (define (filter s1 loc) empty) ;

(define (filter s loc)
  (cond [(empty? loc) empty]
        [else
         (if (char-ci=? (string-ref s 0) (first loc))
             (cons (first loc) (filter s (rest loc)))
             (filter s (rest loc)))]))


;; Final Data
;; ================================

(define DAY2-DATA
  (list (make-pw "4-6 b: " "bbbdbtbbbj") (make-pw "1-6 g: " "ggvggbgggstg") (make-pw "1-4 s: " "lssss") (make-pw "13-14 v: " "hvvcvvvvvvvvvsvvv") (make-pw "3-5 m: " "lcmmm") (make-pw "3-4 t: " "stht") (make-pw "5-6 b: " "dbkbhb") (make-pw "4-7 p: " "ppfppppq") (make-pw "4-5 j: " "jjjjj") (make-pw "3-12 s: " "sskssssssssss") (make-pw "14-15 z: " "zrndzbmrzzpzzqzj") (make-pw "12-18 l: " "tllllllllllllplllbl") (make-pw "8-10 b: " "bdbvqbtbrb") (make-pw "1-3 c: " "tcqccc") (make-pw "1-2 n: " "nbnj") (make-pw "5-7 c: " "ccccccccc") (make-pw "9-10 l: " "hpmslrlgll") (make-pw "6-9 n: " "nnnnnnnnb") (make-pw "6-10 r: " "rmzjlrsxkbw") (make-pw "6-8 r: " "bzqnnrrrj") (make-pw "4-14 c: " "mfffvcbtchzrqcn") (make-pw "1-6 f: " "ffffffffffffff") (make-pw "2-5 f: " "wxtkf") (make-pw "8-12 b: " "rdcbbjbzbbpb") (make-pw "8-18 d: " "ddtdddddddwvdfdsdd") (make-pw "5-8 s: " "sssmsgpgszms") (make-pw "6-11 x: " "xxxxxvxxxxxt") (make-pw "17-18 n: " "jhrnnzpxzngfqrntmnc") (make-pw "13-14 r: " "rrrmrrrrlrrshvrrr") (make-pw "4-5 h: " "hhhhrhh") (make-pw "8-10 d: " "dgwtdsxnncd") (make-pw "9-13 q: " "qqbpqmqgmqwqbqqqxcgq") (make-pw "2-5 g: " "gjjcpgg") (make-pw "6-12 t: " "ttttvttttttrtt") (make-pw "3-9 h: " "hhhhhhhhsh") (make-pw "15-16 p: " "ppppsppppppppppz") (make-pw "2-5 c: " "csccctcccc") (make-pw "11-14 p: " "pppppppppppppnp") (make-pw "8-9 j: " "jvbjjjjrjbjj") (make-pw "8-16 b: " "qklbmbntmvbhxplbbcb") (make-pw "12-13 j: " "jjjjsjmfjjjkhj") (make-pw "2-13 t: " "llckxhfmtznptndcsx") (make-pw "6-8 x: " "dxxxxvxxtt") (make-pw "3-4 d: " "ddht") (make-pw "3-4 t: " "ttdtt") (make-pw "11-12 r: " "rrrrrrrrrrrd") (make-pw "9-10 h: " "hhhhhhhhgh") (make-pw "3-16 h: " "hhhhhhhhhhhhhhnhhh") (make-pw "10-11 h: " "hxhhhdhhhsh") (make-pw "1-7 n: " "jnfnjnn") (make-pw "3-4 m: " "msmnpmpf") (make-pw "15-17 t: " "tpdtttgltvtttztlv") (make-pw "7-10 v: " "vvpsvpgjzvvvvjs") (make-pw "1-5 p: " "pxppg") (make-pw "2-7 w: " "jwhgkgvxcv") (make-pw "13-14 m: " "pxxmjznmrzdsbbmmfj") (make-pw "1-4 d: " "ddddd") (make-pw "14-16 r: " "rrrrrrrrrrrrrwrr") (make-pw "2-5 l: " "klbtzzlrlslgswhljtq") (make-pw "6-12 c: " "qccrcpccccccdccccc") (make-pw "2-18 l: " "llllllllvllllllllkll") (make-pw "18-20 z: " "zzzzzzzzszqzwzrzzzzn") (make-pw "5-16 c: " "cbccclcfcncvqztqc") (make-pw "7-11 m: " "fnwmtsmgpxncnr") (make-pw "6-7 h: " "hpjrhbhkshnchbhpph") (make-pw "11-12 s: " "ssssssssssps") (make-pw "6-9 s: " "jfsmmssssqz") (make-pw "1-15 l: " "llllllllllllllbll") (make-pw "2-6 g: " "gggggwgggpggggggg") (make-pw "6-9 j: " "jjjpjjjjn") (make-pw "9-13 n: " "nnnnnnnnnnnnnn") (make-pw "10-13 t: " "cgntllxnvpkjwxtght") (make-pw "2-11 f: " "xcftbcdcndkgm") (make-pw "10-13 j: " "jjjjjjjjjjjjvj") (make-pw "9-17 f: " "rfrffnsffxqflbffvv") (make-pw "6-11 k: " "kkkkkrwkqckmk") (make-pw "5-7 q: " "zfqqqqqn") (make-pw "12-13 c: " "cfcccvccccckccccv") (make-pw "5-15 z: " "xhzzzzzzzzfzzknzz") (make-pw "18-19 f: " "jkfksvmfjbdffffffff") (make-pw "8-11 h: " "hhhhmhhkhsfdg") (make-pw "14-16 s: " "ssssssdjssssssssssss") (make-pw "15-16 t: " "jtmjhsxqqmmthmtttm") (make-pw "5-7 h: " "hkbjhjhh") (make-pw "2-6 n: " "knprnfnfhhrcnk") (make-pw "3-4 w: " "snwd") (make-pw "5-11 w: " "wwwwnwwwwwwwwwwww") (make-pw "2-12 k: " "bkqjghpktfsk") (make-pw "14-15 v: " "vvvsvvvvvvvvvxv") (make-pw "8-9 w: " "fxwwwwwscwl") (make-pw "9-15 c: " "sbjvvsmdvqknbccxxx") (make-pw "6-15 t: " "tpwjtdnnldthxvn") (make-pw "13-15 t: " "tttttrzmzttjttt") (make-pw "3-11 m: " "mmvmlmmmwfmjx") (make-pw "13-14 s: " "gskssssssscssssqjssl") (make-pw "5-10 l: " "bfnmqlldllp") (make-pw "17-18 q: " "qkwqqqqqqqqqqqrqqqqq") (make-pw "2-5 r: " "rsvrrq") (make-pw "3-4 j: " "jjjdj") (make-pw "3-4 m: " "mtmk") (make-pw "8-9 k: " "vsvkvkrkc") (make-pw "10-12 t: " "ttttttcttttgttt") (make-pw "3-4 n: " "trzw") (make-pw "5-11 q: " "qqmpsqbxkqq") (make-pw "13-15 s: " "sqsssnmwqszfsmv") (make-pw "5-6 b: " "bbbbzvb") (make-pw "5-13 p: " "pjjhpnqpzpmpfpfp") (make-pw "4-5 l: " "mgnwlrw") (make-pw "1-7 k: " "fkzxwkj") (make-pw "1-10 q: " "qgxqqqqqqg") (make-pw "8-9 s: " "ssqssssfss") (make-pw "7-8 c: " "xxcscclccdvcmqcc") (make-pw "2-6 d: " "xdlmzdzxrpmlnt") (make-pw "3-9 s: " "sssssnssgbs") (make-pw "7-10 h: " "sblrrhqrhh") (make-pw "3-5 n: " "xnndnnnfnw") (make-pw "9-10 l: " "vllllqlllhllljxlp") (make-pw "2-5 d: " "ddxzbxk") (make-pw "10-14 m: " "mmmmmmmmmmdmmmmsmmm") (make-pw "1-2 f: " "ffffw") (make-pw "14-15 g: " "wdjhplhrbcxdgpnt") (make-pw "1-2 l: " "klllllll") (make-pw "10-11 k: " "ckkhkkvkkkmkkjkwkkwk") (make-pw "7-9 f: " "ffrhdvftfpjfqffhnfsf") (make-pw "4-5 c: " "kkjksrmkccg") (make-pw "5-9 r: " "rrrrhrrrrr") (make-pw "5-6 p: " "pppppth") (make-pw "4-10 t: " "kpfwzjtchtbndblrvst") (make-pw "1-5 l: " "mllllllnllll") (make-pw "13-16 r: " "rrrvrrrrrrrrzrrg") (make-pw "17-18 x: " "xxfxxxxxxxxsxxxxxsxx") (make-pw "8-11 w: " "rwbnqrngcvpgwwww") (make-pw "4-8 z: " "zzzzzzzzzz") (make-pw "4-9 b: " "bbvvbbbbr") (make-pw "2-5 x: " "qlfhxkx") (make-pw "3-8 t: " "wvptttttttt") (make-pw "1-3 m: " "hmmmmqmm") (make-pw "17-19 h: " "nhhhdvhnhrhhhhnhmdh") (make-pw "11-12 s: " "psszbdpsgfks") (make-pw "6-7 s: " "sssssshs") (make-pw "10-15 l: " "mnkdvnvmxljjtggwcl") (make-pw "1-13 j: " "qjjjjjjjjjjjdj") (make-pw "4-9 l: " "jxvkwhlmlhdtgwvgsdzz") (make-pw "5-9 c: " "ccfghhccccgc") (make-pw "10-11 v: " "vvvvvvvvvvv") (make-pw "7-8 t: " "cntwzshkzvmrnnkr") (make-pw "1-11 l: " "tllllllltllll") (make-pw "15-17 l: " "lllglvctrvllzkllt") (make-pw "3-5 n: " "ncnnp") (make-pw "2-3 q: " "jsqqh") (make-pw "4-10 h: " "hhhhhhhhhvh") (make-pw "16-18 b: " "bbnbbbbbtbbkktbbzdr") (make-pw "4-6 g: " "kbggdhgggggggggggfc") (make-pw "12-13 p: " "cpvcppqpplwpt") (make-pw "5-10 h: " "fvhhbrhpghchhhhhh") (make-pw "2-5 b: " "bbvzn") (make-pw "14-15 x: " "xxxbxnwxxxxzxxh") (make-pw "13-15 n: " "nnfgdglfnntnjqn") (make-pw "2-4 c: " "cfccc") (make-pw "3-5 v: " "vrvvzdvv") (make-pw "17-19 v: " "vvvvvvvvvvvvvvvvvvsv") (make-pw "1-20 h: " "hhhhhhhhhhqhhhhhhhhh") (make-pw "4-15 q: " "qqqtqqqqqqqqqqqqlq") (make-pw "11-13 h: " "hhkhvhhhhwhgk") (make-pw "8-11 p: " "ppvppppppptspf") (make-pw "8-9 m: " "mmmpmmkmdmpkspmg") (make-pw "1-7 m: " "lcmvggm") (make-pw "6-12 v: " "tvfstvvpvzsvcv") (make-pw "8-9 n: " "nnnqnnwrrdzlmnwlznrn") (make-pw "1-5 s: " "msssms") (make-pw "1-3 v: " "vpdzvdvgv") (make-pw "6-9 g: " "drgrfggcg") (make-pw "6-16 x: " "djpxhxvncxfghsxx") (make-pw "1-3 b: " "sjbwwxbvtvbkt") (make-pw "6-8 c: " "cccccstccjhv") (make-pw "1-11 q: " "qqqpqqqqqqwqqq") (make-pw "9-11 m: " "vsbmmmmmmmqmmsm") (make-pw "2-7 g: " "gqggggggg") (make-pw "2-3 m: " "mrgvm") (make-pw "6-7 c: " "cccpcfcc") (make-pw "6-10 w: " "swbngwswnxnww") (make-pw "13-14 r: " "rrrrrrrrrrwrrgrdr") (make-pw "3-9 v: " "vctxhxtfvq") (make-pw "2-9 r: " "jrrcslgplcprlvgthg") (make-pw "2-3 n: " "hnnnsxclvdj") (make-pw "10-11 h: " "zrhghhqhgzh") (make-pw "15-18 z: " "zzzlzzzzzzzzpzqzpzzz") (make-pw "3-8 f: " "fffffrfl") (make-pw "1-4 l: " "rllllfl") (make-pw "1-2 n: " "nnnnnvtnv") (make-pw "17-19 z: " "zzzzzzzzzzzzzzzzzzqz") (make-pw "13-15 n: " "nnnznnnnnnznnnn") (make-pw "2-5 c: " "gcccncjmsncfcntjc") (make-pw "8-9 h: " "hhhhhhhhbpsfh") (make-pw "7-11 r: " "lzvvlbrgjgrr") (make-pw "5-11 x: " "xxxxqxxxxxxx") (make-pw "4-7 p: " "gqpkmppzpsmtzhfdfpl") (make-pw "3-4 j: " "jjdjdg") (make-pw "14-16 z: " "zzzzzzzgjzzzzpzf") (make-pw "1-2 n: " "nnnvnwnnnnh") (make-pw "4-5 z: " "jhzzz") (make-pw "5-7 k: " "kkkkkkf") (make-pw "8-18 z: " "khzzrzjzmzzvzzpcclm") (make-pw "5-10 m: " "kjrhwkhmsm") (make-pw "10-16 v: " "vvvvvvvvvvvvvvvwv") (make-pw "9-10 l: " "xhvjsmllkcdtldfxlw") (make-pw "8-10 p: " "ppjvppbpqhpwhppgbp") (make-pw "4-6 m: " "jlmkhm") (make-pw "1-3 k: " "gvpklkkkk") (make-pw "15-16 g: " "ggjggggvgmgtpgcg") (make-pw "1-4 j: " "jbjwj") (make-pw "1-3 x: " "xxxpxxdxxhfx") (make-pw "14-16 v: " "vxmhhdvvfjjqwhtv") (make-pw "6-7 l: " "lnkchzlwxlp") (make-pw "3-4 v: " "vvvcv") (make-pw "13-14 p: " "pbqpppppzbmppc") (make-pw "6-12 p: " "glqwzprpqbqf") (make-pw "6-12 l: " "lllllglllllll") (make-pw "8-9 n: " "nnsnnnndcn") (make-pw "6-8 p: " "prwppppp") (make-pw "1-10 q: " "dqqqqqqqqjq") (make-pw "12-16 w: " "kwtbdnjqmwwxhwcwswkl") (make-pw "11-14 r: " "rrnjghfrrrshlrq") (make-pw "2-14 w: " "vwbbvcvgnxdmxl") (make-pw "7-8 g: " "sqmggkgslkwlvggg") (make-pw "1-6 q: " "tqqqqqq") (make-pw "2-3 b: " "bbbr") (make-pw "7-9 b: " "jnwbswfpbn") (make-pw "4-5 n: " "nnlct") (make-pw "3-11 s: " "ssssssssssp") (make-pw "2-6 f: " "wjlpwf") (make-pw "5-10 g: " "gggghqgqgb") (make-pw "1-3 p: " "ppdg") (make-pw "4-7 j: " "pjnkjjljjj") (make-pw "1-2 v: " "ghmjzxmtxjxnv") (make-pw "6-14 k: " "klgdzfmgdwhqdkhcnzm") (make-pw "6-11 z: " "tgzpzzzzztc") (make-pw "2-12 b: " "cxsmjbdgdnrb") (make-pw "4-6 v: " "lxdvvh") (make-pw "3-8 l: " "pnpdnrll") (make-pw "7-8 m: " "mzmswvmmbxmzlmwhdvq") (make-pw "13-14 s: " "khzssssssssszsssss") (make-pw "10-18 d: " "dmfdlgcxdbzznbrlqn") (make-pw "13-14 j: " "jjjjjjjjjjjjdpjj") (make-pw "15-17 j: " "jjjjjjjjjwjsqjwjj") (make-pw "10-15 x: " "xxfxkzxxhxxxxxvxxw") (make-pw "7-14 c: " "wcccwcmmcccccxhcccc") (make-pw "2-7 z: " "zmzvfzlszr") (make-pw "7-8 k: " "jjkrklrkkv") (make-pw "8-9 r: " "rrrrrjrtz") (make-pw "2-3 w: " "tvws") (make-pw "1-5 b: " "bbbjm") (make-pw "1-2 q: " "tqqjf") (make-pw "5-10 j: " "wlgjghjhjljwtpcdkqwk") (make-pw "2-5 c: " "dzpkc") (make-pw "5-6 m: " "mbvmkm") (make-pw "4-15 k: " "stjkjvvxrmwdpkwsjqvc") (make-pw "6-9 h: " "hwkgjplmhxwgvnbhwh") (make-pw "12-13 z: " "zzzzzzzzzzzzz") (make-pw "7-8 q: " "qqqqqqqqq") (make-pw "2-5 c: " "clcwmccczclcccc") (make-pw "2-5 l: " "jlcgfbflklvpfqxtwgg") (make-pw "5-7 n: " "nnnnnnvnnnnnn") (make-pw "3-5 f: " "gfktfffqvgltsbff") (make-pw "10-16 p: " "jppbttppzpqppppp") (make-pw "2-3 m: " "zmdm") (make-pw "4-6 j: " "zsmtjjdnrpp") (make-pw "17-18 j: " "hvvmrkfnnkvrjtjhjj") (make-pw "12-15 d: " "dtddddddddtwxgld") (make-pw "8-12 r: " "rdzrwfgrmxwttknxz") (make-pw "6-7 s: " "rssbktxsgd") (make-pw "11-13 d: " "ddddmwddddxddndc") (make-pw "3-6 p: " "ppcpspfp") (make-pw "12-15 j: " "jjjjjjgjjjjcjlzj") (make-pw "8-12 v: " "tgjkwfbsxzzvvpmfs") (make-pw "6-7 z: " "trbfbdz") (make-pw "4-6 v: " "vvvvvpvv") (make-pw "8-16 p: " "pnvppdpjppppppph") (make-pw "8-9 z: " "lzzzzpdzk") (make-pw "1-4 t: " "qttzz") (make-pw "2-3 d: " "dhdd") (make-pw "5-8 m: " "mmmkmmxmkj") (make-pw "2-12 f: " "hfhzkwdmrlqvfkn") (make-pw "5-6 h: " "hhhhhph") (make-pw "14-15 b: " "bbbbxbbbbbbbbbh") (make-pw "2-3 v: " "vvcj") (make-pw "12-19 d: " "ddvdwwqdddcdtdmwdqp") (make-pw "3-4 s: " "ssjssssssss") (make-pw "2-6 c: " "wzzxqcdcnlgcph") (make-pw "11-12 j: " "jjdrcjzjkjcs") (make-pw "4-9 z: " "bzzzzzzxzz") (make-pw "2-8 b: " "gjbfkxhb") (make-pw "1-3 c: " "cpksst") (make-pw "1-5 h: " "hhjhh") (make-pw "14-15 j: " "jjjjjjjjjjjjjjr") (make-pw "8-9 n: " "pcndxcfknfbnnls") (make-pw "10-13 k: " "kkkqzkwbkkkrtn") (make-pw "4-5 r: " "rzrrrnrj") (make-pw "13-15 p: " "ppppppppppppppt") (make-pw "3-6 j: " "fjqqzzzjm") (make-pw "2-9 m: " "zmjhctkmf") (make-pw "5-6 s: " "ssmjss") (make-pw "3-4 c: " "jlfd") (make-pw "8-12 d: " "qbddfhnddzgvddddd") (make-pw "6-7 p: " "xfppppcppppxgp") (make-pw "8-13 s: " "sssssssfssssssss") (make-pw "13-17 g: " "xskktsjxlvgfxtzzgfj") (make-pw "2-4 q: " "qqtwfqqnkvbvbhzs") (make-pw "1-5 j: " "cjmjs") (make-pw "4-5 c: " "tvccnc") (make-pw "3-14 m: " "kkfhmnkkmztxtmn") (make-pw "11-15 x: " "xxxxxpxmxxvbxxxvx") (make-pw "9-11 l: " "nhgzwmmrkqhblnk") (make-pw "7-10 x: " "xxxxxdmxxxxxxxx") (make-pw "13-14 v: " "vvvvvvpvvvjvvcvv") (make-pw "16-17 n: " "nnnnqnnbnnnnnnnpn") (make-pw "3-4 j: " "jvjj") (make-pw "15-16 q: " "zlqsgvpztknqjqqwqvf") (make-pw "3-4 s: " "sssrsssdss") (make-pw "11-13 g: " "ggqggmggswggdk") (make-pw "1-4 t: " "jmtzttztqt") (make-pw "2-3 v: " "vvvv") (make-pw "11-13 g: " "cgjgxgggkgbggxg") (make-pw "4-6 g: " "ggggqlhgmz") (make-pw "8-15 g: " "prvxwzkvdhgkjlg") (make-pw "6-9 g: " "gggggnsjlg") (make-pw "12-14 p: " "mmvlpzkmpgtpvj") (make-pw "7-10 k: " "kkkxkkjkkkdkkkp") (make-pw "14-16 b: " "zmztqsrgvjjmswzkbnk") (make-pw "1-2 x: " "xxbxxxxhx") (make-pw "2-5 l: " "tlhsx") (make-pw "3-5 x: " "xxxxn") (make-pw "4-12 c: " "vdnmtmqwnxkcldc") (make-pw "4-8 x: " "xxxxjglx") (make-pw "5-13 s: " "vsssspszssssnsss") (make-pw "3-6 k: " "kkhkkkknb") (make-pw "3-5 t: " "pttqtwnprt") (make-pw "10-14 m: " "ttjqvzmgmmjqzkd") (make-pw "1-5 b: " "bbbbpbbb") (make-pw "10-15 d: " "wdjrhvfngdtlkdl") (make-pw "6-7 w: " "trxwdwww") (make-pw "2-4 n: " "snxqlgtsmdnnjgwrgmms") (make-pw "16-18 l: " "klslpljllqlcslqqll") (make-pw "4-7 t: " "tttwzttjt") (make-pw "9-10 f: " "ffffffffwf") (make-pw "8-15 h: " "trhgxjchhxvvhqp") (make-pw "8-12 w: " "wwwwwwwqwwwg") (make-pw "12-13 x: " "xxxxxxxxsffqlxx") (make-pw "5-7 k: " "kkkklkkkktkk") (make-pw "1-7 m: " "wkmmqmmhf") (make-pw "12-13 h: " "mhhchwhhhzhhcvh") (make-pw "7-11 k: " "kkkkkkwkkkgkk") (make-pw "4-6 l: " "hdlbll") (make-pw "8-12 v: " "fvvvvvvvrvvv") (make-pw "2-9 h: " "hzhhhhhhhhhhhhr") (make-pw "6-8 b: " "qbqjpbbbdsshv") (make-pw "2-3 h: " "hwhl") (make-pw "4-7 l: " "mdllxjgdw") (make-pw "3-9 f: " "rwffzfkpwbzp") (make-pw "3-5 t: " "ttnttt") (make-pw "7-10 c: " "mpcccpndqc") (make-pw "6-7 h: " "fhhhljh") (make-pw "2-7 v: " "pnvzcns") (make-pw "1-3 v: " "vvgv") (make-pw "5-9 s: " "ssssshsspsssssss") (make-pw "4-6 j: " "mjjjjq") (make-pw "5-14 h: " "hwbqghmvmmnvhhrqmj") (make-pw "5-6 s: " "wctjsh") (make-pw "7-8 s: " "sssscsjs") (make-pw "14-17 v: " "vvvvvvvvvvvvvvvvv") (make-pw "14-15 w: " "wxwwwwwwfwwwwsw") (make-pw "6-7 v: " "ksvvvlpvv") (make-pw "7-18 s: " "cssstsvsscshsstsss") (make-pw "5-6 b: " "zzwbpm") (make-pw "6-7 r: " "bvtmpkxspskr") (make-pw "6-7 v: " "lvxrvqv") (make-pw "15-17 c: " "ccccccccccccccccccc") (make-pw "8-12 r: " "ghxpwhxcqjrr") (make-pw "6-10 k: " "kzbcdkndqm") (make-pw "5-6 s: " "tpsxss") (make-pw "1-5 p: " "qlrlp") (make-pw "4-8 q: " "qqqkqqqz") (make-pw "7-11 v: " "vgvsxvwvlxv") (make-pw "2-5 b: " "bgkbb") (make-pw "3-18 j: " "jjsplxjxgqjfjrjxjjlx") (make-pw "6-10 k: " "kxkhkkjkkrvkkk") (make-pw "7-16 t: " "ttttttzttttttttbtttt") (make-pw "1-7 l: " "ptzptslrjgwlfgwq") (make-pw "8-10 v: " "vvqvvvvvvvv") (make-pw "3-5 z: " "zzzhz") (make-pw "6-7 c: " "cbrctgc") (make-pw "5-16 l: " "llllxlllllllllllll") (make-pw "6-7 c: " "chxclqcdrh") (make-pw "1-10 c: " "jcmcccccwcccccjbvc") (make-pw "8-14 h: " "zhhvhhhhhhhhhv") (make-pw "4-7 n: " "rnnnnnfnnnnnvn") (make-pw "10-12 w: " "wwwwwwwsgtwww") (make-pw "5-8 c: " "cmbcctzcj") (make-pw "5-7 f: " "fffskrf") (make-pw "5-6 l: " "lllmzl") (make-pw "7-14 m: " "mmmmmmmmmmmmmmmmmmm") (make-pw "1-9 f: " "jfffffffzvffff") (make-pw "2-5 g: " "ggmng") (make-pw "16-17 x: " "xxxxxxxxxxxxxxxlx") (make-pw "4-10 n: " "npkgjcfnnnnn") (make-pw "1-13 d: " "bhkjgsnzxkdgwbdv") (make-pw "6-7 f: " "ncqfzff") (make-pw "7-10 h: " "hqhhhhhjhhh") (make-pw "2-4 v: " "vnvvv") (make-pw "1-6 w: " "thlmdwgwgtswvtx") (make-pw "3-7 d: " "ddlkhvfdnpbdr") (make-pw "1-4 q: " "qbfq") (make-pw "2-6 f: " "rzfmfrjgcfjk") (make-pw "10-16 b: " "bbbbblbbbkbbbbbbwqb") (make-pw "13-14 j: " "jjhcjnkgvrnwjp") (make-pw "5-12 l: " "lgqwvrlwcllllv") (make-pw "4-8 j: " "jjgpdjssspjfdbt") (make-pw "1-9 h: " "hhhhhhhhwhhh") (make-pw "4-6 p: " "psmppt") (make-pw "2-3 h: " "zhhhk") (make-pw "2-6 b: " "bbbbbmbbvb") (make-pw "7-9 z: " "zzzzzzgzdzz") (make-pw "8-9 d: " "ddgdmdwddd") (make-pw "2-6 s: " "nssssv") (make-pw "18-20 x: " "xxxxxxxxxxxxxxxxxvxx") (make-pw "9-10 m: " "dzckmrbhcmwvkcxmlx") (make-pw "10-11 v: " "vwvvvvwvvghvn") (make-pw "3-6 d: " "wcwxddjhnljfntj") (make-pw "13-15 v: " "vvvvvvvvvvvvcvc") (make-pw "1-5 x: " "xxfxfxxkx") (make-pw "7-8 w: " "wqzjzwwwtw") (make-pw "2-4 f: " "flgl") (make-pw "3-6 n: " "ncfngngdnm") (make-pw "5-7 k: " "kdkmbkkkkxk") (make-pw "5-6 x: " "xxvxth") (make-pw "5-6 r: " "rrhhzr") (make-pw "4-6 b: " "bbbrbbbbbmb") (make-pw "12-13 q: " "qqqqqqqqqzqkqp") (make-pw "6-14 n: " "mlbflnrbhlhpdrfln") (make-pw "9-12 q: " "qqqqqqqqqqqwq") (make-pw "3-4 k: " "rkncnbk") (make-pw "1-3 j: " "jjpj") (make-pw "8-9 v: " "vjvczrvvm") (make-pw "9-16 m: " "tmnmmmxmbmmmrtmmr") (make-pw "9-11 q: " "qqqqqzqqsjxq") (make-pw "3-5 h: " "hhhhcs") (make-pw "11-12 k: " "kkkkkkkkkkkvqk") (make-pw "11-12 x: " "xlxxxjjxxxpx") (make-pw "1-12 n: " "nngtnhlnjfnf") (make-pw "5-7 p: " "ppptppppp") (make-pw "13-14 l: " "lllmlflllhllpm") (make-pw "13-14 m: " "vdkmrdfzmkknmp") (make-pw "13-15 s: " "ssssssqssssscssss") (make-pw "5-11 h: " "hhhlxhhhhhhshh") (make-pw "10-11 s: " "ssssssssssgsssss") (make-pw "6-7 q: " "kqqqqmqnqq") (make-pw "8-13 l: " "llllllltlllljll") (make-pw "9-12 j: " "jjjjjjjjjjjlj") (make-pw "7-11 s: " "gstcncsssscssssss") (make-pw "4-6 x: " "lrtjfnhmpmxj") (make-pw "9-20 c: " "cttccccccnccclcccccc") (make-pw "1-8 w: " "cwwwwwwwwww") (make-pw "12-13 n: " "nnnnnnnngnnnnn") (make-pw "1-5 p: " "pfqwcpnppppwwpqppp") (make-pw "7-8 g: " "gggsgglghg") (make-pw "6-7 g: " "gggggggg") (make-pw "4-10 s: " "ssssssssssss") (make-pw "1-2 n: " "njsnnln") (make-pw "7-8 z: " "gzczzwdzkkzz") (make-pw "2-8 b: " "bbbwjfbh") (make-pw "6-8 r: " "rrrrbrrbr") (make-pw "2-5 k: " "lkkkkl") (make-pw "2-3 c: " "swcgjcm") (make-pw "2-10 t: " "ttttttttttt") (make-pw "12-13 l: " "lllllxllllllqhl") (make-pw "2-9 l: " "qtqxdpqqlwhqwlr") (make-pw "1-5 q: " "qqxrn") (make-pw "10-11 k: " "kkkkkkkkkckk") (make-pw "1-2 f: " "ffff") (make-pw "3-4 r: " "rrxr") (make-pw "8-9 p: " "lppppxpsp") (make-pw "2-5 s: " "ssstchlrds") (make-pw "11-15 m: " "mmmmmmmmmmmmmmmmm") (make-pw "14-17 k: " "kkkkkkkkkkgkkkktbkk") (make-pw "1-2 x: " "xlxx") (make-pw "9-10 w: " "wwcwptczwzzd") (make-pw "1-3 c: " "ccgcccccccccccccc") (make-pw "4-5 h: " "nhhhvh") (make-pw "8-16 l: " "hlllfllllmltlhlldl") (make-pw "4-5 w: " "wgwfw") (make-pw "6-15 p: " "pjpbfrmxqgkxkbqhj") (make-pw "12-17 l: " "llmtllnlllllllllnns") (make-pw "8-15 n: " "zjnxzndnznklxzjlx") (make-pw "6-9 r: " "wpsmstnkgtrmng") (make-pw "3-7 s: " "csvhxhsgvrsrn") (make-pw "10-15 h: " "hchhhdhkhghlhgsh") (make-pw "2-9 c: " "mzbmtccktc") (make-pw "3-4 x: " "xlzvxg") (make-pw "12-14 k: " "zmkskknwkkkmkkwkgkkk") (make-pw "4-9 r: " "vqrrrrdzpl") (make-pw "3-4 g: " "ggvbxg") (make-pw "5-8 q: " "xtrqrmqq") (make-pw "16-18 l: " "zllllllllllllllwlnll") (make-pw "6-7 s: " "zsqszss") (make-pw "3-4 g: " "wghgpg") (make-pw "1-6 z: " "hzzzzrz") (make-pw "3-8 h: " "hthfqtccnq") (make-pw "15-18 p: " "ppppppcpppppppnppppp") (make-pw "3-4 l: " "qplkdmjntlghjlpxlq") (make-pw "10-12 q: " "xzqkxdvgrqxqqzzxgjj") (make-pw "6-9 q: " "qqqvsvqqxq") (make-pw "12-13 g: " "shgcnjlgvcgqg") (make-pw "5-10 l: " "llklplllmlsl") (make-pw "3-12 l: " "vllqfzwnsqslpnvrbkh") (make-pw "14-16 g: " "gjggggggggggggbzcggg") (make-pw "5-11 m: " "ssmsmbnspmm") (make-pw "11-13 v: " "mkqvvvvvmcvvz") (make-pw "2-3 k: " "mkkchtzqsvkbclgxn") (make-pw "4-7 r: " "rphrrnrrqwknrktrgsg") (make-pw "6-10 z: " "vpjhzzzkqzjl") (make-pw "16-18 c: " "ccccccccccccccchccc") (make-pw "1-10 v: " "qvvvvzvvvvvv") (make-pw "5-7 j: " "jjrjjjj") (make-pw "14-16 l: " "zlgdrlqllgpllfhh") (make-pw "3-6 l: " "llmllll") (make-pw "3-4 l: " "smdl") (make-pw "4-11 h: " "kgqhcpvrbldrhbq") (make-pw "1-15 j: " "ljjjjjjjjjjjjjjj") (make-pw "7-14 c: " "ccccccccccccccc") (make-pw "2-10 v: " "vzvjvvvvvvvvv") (make-pw "4-9 p: " "flbpmqmhkpt") (make-pw "10-12 q: " "rtdrqmpcsqrhqqchqczw") (make-pw "16-19 w: " "vlwxgtmjwrzvqgdwbdw") (make-pw "8-15 c: " "gkcccslctcmszhc") (make-pw "3-7 f: " "hhffhbbtbwzw") (make-pw "4-9 s: " "msbsxssds") (make-pw "13-15 p: " "pppppppppcpppxldp") (make-pw "6-7 m: " "mmmmmqmm") (make-pw "11-12 m: " "mxmmzwmmmdqpmp") (make-pw "7-12 l: " "gncmgzxlqcllqgt") (make-pw "12-14 t: " "tttttttttttdtt") (make-pw "2-6 f: " "ffffffbrfffp") (make-pw "14-17 h: " "hhdhpphhhhhhkhhxqph") (make-pw "7-9 x: " "xxxxxxxxqxxxxxx") (make-pw "5-18 j: " "vlwgjljtljtrdbxjnjwm") (make-pw "5-7 n: " "jmncnsndnbwx") (make-pw "8-9 r: " "rrrrrrrrr") (make-pw "11-17 j: " "jjjjjjjjjjjjjjpjr") (make-pw "5-6 x: " "xxxxjt") (make-pw "13-15 l: " "nshmnjgzhmjdzvl") (make-pw "9-10 r: " "wmsvzxsrqnnhfr") (make-pw "8-11 k: " "kkkxxrkpktg") (make-pw "1-7 r: " "rrrrrrrfrrrjwrd") (make-pw "5-7 t: " "rztvtvplbrk") (make-pw "2-7 w: " "wwwwwwsw") (make-pw "1-2 g: " "sgggk") (make-pw "12-13 j: " "jjjjjwjjjjvvjjjrjs") (make-pw "1-2 c: " "cccccc") (make-pw "3-5 g: " "khgzr") (make-pw "9-16 b: " "bbbbbbcbsbbbbbbb") (make-pw "12-13 z: " "zzzzzzzzzszkz") (make-pw "4-5 r: " "rkrrrr") (make-pw "1-2 t: " "tgbqtddbmq") (make-pw "1-2 w: " "wkwwwww") (make-pw "14-15 q: " "qqdqqqqqqqqhqnb") (make-pw "6-7 r: " "rrrrrdmrr") (make-pw "8-9 j: " "jjjjjjjqhjjjjj") (make-pw "3-6 t: " "gwmlntffstzllvs") (make-pw "6-8 h: " "hhhhnhhhqh") (make-pw "9-11 w: " "cwcwwwwwgxwwbw") (make-pw "5-9 w: " "wwwwcwwww") (make-pw "8-12 k: " "snjmkkhrgkkzkkpskk") (make-pw "3-6 q: " "lvqjqlq") (make-pw "3-4 z: " "zzzn") (make-pw "9-12 t: " "dvmvhttxtmzhrr") (make-pw "3-7 k: " "kkkkkkskkkkkk") (make-pw "6-15 v: " "vqvvvdvvvvbvvvvvvvvv") (make-pw "3-4 z: " "jzzzzzdk") (make-pw "8-12 b: " "bbbbbbbbbbbs") (make-pw "3-9 m: " "nvhwmwgmmqkbmmmzb") (make-pw "8-10 r: " "npwjcgwrwcrx") (make-pw "1-5 r: " "rxdrr") (make-pw "6-9 p: " "ppppppsbkmppkp") (make-pw "1-4 j: " "jjjqjjjz") (make-pw "6-7 b: " "rblbbbbbbp") (make-pw "5-9 t: " "tttttttttf") (make-pw "8-9 c: " "czcccccccc") (make-pw "13-14 j: " "jjjbjjtbjjjjjj") (make-pw "3-8 p: " "pwppzqvp") (make-pw "3-12 m: " "mmzmmmmmmmmmmm") (make-pw "3-9 d: " "ddjddddddqddd") (make-pw "3-4 q: " "qqfq") (make-pw "6-14 m: " "mmmmmbmmmmwmmlmmmm") (make-pw "3-4 c: " "glgzc") (make-pw "6-7 t: " "ttttttt") (make-pw "1-4 s: " "sssbs") (make-pw "5-8 r: " "drrkrrrzrrrr") (make-pw "10-12 q: " "rvqfqqkllqqqlfrq") (make-pw "11-13 s: " "szsssssssssssss") (make-pw "8-11 t: " "tttttttsttvt") (make-pw "11-15 j: " "zjjjjjjjjjjjjjjjjj") (make-pw "2-4 s: " "dvfs") (make-pw "3-4 q: " "zhpq") (make-pw "10-11 c: " "cccccccccckc") (make-pw "5-11 h: " "pnrjhtdlkzvhh") (make-pw "5-9 l: " "nlllxgnrlllllllmq") (make-pw "3-4 f: " "gfff") (make-pw "8-9 g: " "grgggggbg") (make-pw "3-4 j: " "pfwjhh") (make-pw "2-5 l: " "jlfjr") (make-pw "2-4 l: " "lblv") (make-pw "3-10 q: " "qqvzhnqqhqvqq") (make-pw "11-15 t: " "tfttttttktwttts") (make-pw "12-14 b: " "gqptrzwclbdbfqd") (make-pw "1-4 r: " "crrprr") (make-pw "5-13 v: " "qvdvvvdnrqmrqp") (make-pw "9-12 r: " "rwrrlsrrsrrjgdnrrr") (make-pw "4-6 c: " "ccldccnp") (make-pw "16-17 q: " "qqqqqpqqqqlqqqqmlq") (make-pw "2-8 w: " "wwwwwwwhwmwws") (make-pw "3-6 m: " "dmmbmmdmkxm") (make-pw "6-13 t: " "xflrtblvcvfxnlf") (make-pw "2-6 m: " "pmnsmm") (make-pw "13-15 k: " "kstfvnkkgfvvkbk") (make-pw "4-10 w: " "jqwcwlcwcwvqbfzfzfm") (make-pw "1-5 d: " "ddndbmjxhfqqn") (make-pw "13-16 p: " "pzvbszhqtpklpkpdw") (make-pw "2-4 t: " "thttwmxjsbtp") (make-pw "8-10 s: " "zbsvjcssfmf") (make-pw "3-5 l: " "lltlwlllll") (make-pw "12-14 g: " "hgfvvfpnrvpfggnss") (make-pw "3-13 q: " "cjmbvgxchmqdqcvc") (make-pw "12-16 j: " "cjjjjjjjjjjqjjjjjj") (make-pw "1-5 m: " "kqrgm") (make-pw "11-14 x: " "xcjpwbrrffxkfxh") (make-pw "12-14 c: " "kgfnccxqczkcjkcc") (make-pw "3-4 f: " "sfftf") (make-pw "1-3 f: " "ffdz") (make-pw "12-14 t: " "xftfxmkttdsttg") (make-pw "4-9 f: " "ffhcgfffffff") (make-pw "9-10 b: " "zjlbbsbzbx") (make-pw "9-14 m: " "hhxmlmmmzmmtmm") (make-pw "3-5 t: " "tkcpzjwr") (make-pw "6-9 k: " "wkfdwflgrntrknsr") (make-pw "1-4 l: " "gfml") (make-pw "9-10 z: " "zzzzzzzzzrzzzz") (make-pw "9-11 h: " "hhhhhhhhhhzhh") (make-pw "1-9 k: " "kkkkkkkkzkkkkkkk") (make-pw "2-7 h: " "sswmmkhkvhw") (make-pw "2-5 m: " "pmbnnmzrkk") (make-pw "9-12 f: " "xhpfbfdffkfw") (make-pw "3-4 g: " "gfwg") (make-pw "11-12 n: " "nnnnnnnnnnnpnnnxn") (make-pw "5-6 d: " "dddddgddd") (make-pw "7-9 j: " "cbjgnjqjgj") (make-pw "9-14 r: " "rrrrrrrhrrrrrzbrr") (make-pw "4-5 p: " "prphk") (make-pw "2-4 d: " "dpdd") (make-pw "5-15 p: " "zjsppzhqqgqspcppqpps") (make-pw "2-7 r: " "rrrrrrfwrr") (make-pw "1-3 h: " "hhqh") (make-pw "14-16 g: " "zgvdgspkjrrvcgdlxg") (make-pw "4-6 c: " "psgqccccvc") (make-pw "9-14 q: " "qdqjqmttdtcqggqpqn") (make-pw "13-18 f: " "ffffffffffffgffffff") (make-pw "8-9 g: " "gggggggggggg") (make-pw "11-12 x: " "xxxxxxxxxxxr") (make-pw "2-17 c: " "cxcccfchcccccccbbcm") (make-pw "7-11 b: " "nwbrzndvrfxwt") (make-pw "3-4 x: " "xhwxxxv") (make-pw "16-17 r: " "rzrrnrrrvxrrzrrrrrr") (make-pw "2-3 f: " "zcvgbmxvwp") (make-pw "5-6 b: " "fwbbbf") (make-pw "6-10 m: " "mmmmmpmmmw") (make-pw "2-4 n: " "bnnvndbpvzj") (make-pw "3-4 t: " "dftfhdngqp") (make-pw "1-2 v: " "vtvvvvvvvvv") (make-pw "2-3 v: " "vgvv") (make-pw "9-10 s: " "tsvsshsssgssssmsksss") (make-pw "6-7 t: " "ttttttxt") (make-pw "3-4 c: " "jcrc") (make-pw "5-8 l: " "ztlmjljlb") (make-pw "4-5 w: " "bwwwww") (make-pw "11-14 r: " "rrrbrrrrrrnrrrs") (make-pw "10-16 z: " "zzzzznzzzwzzzzzzzz") (make-pw "7-13 m: " "mmmmmmfmmmmmmmmmmmmm") (make-pw "15-19 w: " "fflzcwftmcswcwwnwts") (make-pw "10-19 b: " "bjbbbbbbmwbbbbbbbbtb") (make-pw "14-16 x: " "xxxxpxxpxxqxdqxxxx") (make-pw "8-13 r: " "rkzrjbxrgwkhnb") (make-pw "5-18 r: " "rrrzfkrrrrrrrrtrrnrr") (make-pw "4-5 b: " "bbbfzb") (make-pw "6-10 q: " "qnscxqqfqb") (make-pw "8-13 w: " "wwwwwwwfwwwwwwwwww") (make-pw "10-11 n: " "scvnsnpgnjnmdpnwct") (make-pw "7-11 p: " "pjpppppdqpjpfppsptp") (make-pw "7-9 n: " "jfsvclhfm") (make-pw "3-8 h: " "whsggqscd") (make-pw "2-12 r: " "mrmpxhrqsdmqpjshvck") (make-pw "2-4 g: " "gdgggg") (make-pw "7-12 s: " "ssssssssgsss") (make-pw "7-11 l: " "dlklllnjlslbl") (make-pw "5-11 k: " "kkkkkkkkkkkkkkk") (make-pw "6-9 g: " "dhtvcgmfrjhk") (make-pw "4-5 m: " "lmmmhmsmmmmmcmmmmzmm") (make-pw "11-13 t: " "zgtnkjzmtkttmtkc") (make-pw "6-7 b: " "bbbbbbv") (make-pw "3-4 q: " "qqcq") (make-pw "12-16 z: " "zzzszjzszzczmxtzzcl") (make-pw "2-5 w: " "wwfhp") (make-pw "1-2 d: " "gddkd") (make-pw "5-12 f: " "fwqgbvrcfmwb") (make-pw "2-5 w: " "bnxcw") (make-pw "3-15 t: " "ncwftppphsxvztttjs") (make-pw "7-10 p: " "ppnsppkcppsp") (make-pw "5-6 v: " "vvrdvv") (make-pw "5-6 z: " "hczwbzz") (make-pw "3-12 p: " "gcphfgmzfkflspmxg") (make-pw "14-19 m: " "jmfmfjpvbmfmmrdkdnzp") (make-pw "6-7 p: " "pdwzppppp") (make-pw "4-7 b: " "bbbnbbqbb") (make-pw "7-8 r: " "frrrrrxxr") (make-pw "8-18 b: " "jbphpzgvnppwhkxfzs") (make-pw "1-5 c: " "pvhcc") (make-pw "4-8 z: " "fzznzjzztstzxrz") (make-pw "5-10 v: " "jvvwvvvlvlvqc") (make-pw "7-14 p: " "svgrzfpxkdpbhph") (make-pw "5-7 n: " "nncmvkn") (make-pw "11-12 r: " "rrlrbrrrvrrr") (make-pw "1-5 r: " "vtngrndhqf") (make-pw "2-4 k: " "knjp") (make-pw "11-14 h: " "hhghhhhhhhxhhwhhh") (make-pw "16-18 r: " "rrrrrrrrrrrrrrrpnb") (make-pw "10-14 d: " "dpddbvdtdmxfdddd") (make-pw "2-4 t: " "cztt") (make-pw "5-6 r: " "rzrprd") (make-pw "2-5 n: " "ngwdngc") (make-pw "1-6 q: " "qqqzzwwqqqkqqq") (make-pw "7-8 l: " "lllzllpxl") (make-pw "1-4 b: " "kmltzzjzbppgwq") (make-pw "1-4 t: " "ttttb") (make-pw "3-4 p: " "dkxpcph") (make-pw "7-15 z: " "zzzzzzhzzzzzzzzzz") (make-pw "4-7 c: " "cdljfccm") (make-pw "3-8 p: " "hbpxhlmc") (make-pw "4-5 w: " "wwwlw") (make-pw "14-15 b: " "lbqbbbkgbbwfbdb") (make-pw "6-10 j: " "jtjjvpwwthwcsj") (make-pw "5-8 t: " "ltvtttbtqxtzq") (make-pw "12-13 t: " "ttttttttjttztxt") (make-pw "16-18 p: " "ppppppppppppppxpxbpp") (make-pw "4-7 n: " "nnnrnnnnt") (make-pw "1-2 r: " "rfrr") (make-pw "5-6 g: " "ggggvggg") (make-pw "3-4 s: " "ssbw") (make-pw "3-4 l: " "ldll") (make-pw "8-10 m: " "mmmmvmmcjkmg") (make-pw "10-18 w: " "wwvwwvwwhwwjwwwlwxcm") (make-pw "1-2 x: " "cxxxxxxxxxxxxx") (make-pw "1-5 b: " "bbbbkbbb") (make-pw "12-14 z: " "zwqzrrzzvqqzzszrx") (make-pw "1-5 z: " "lzzfzzzfz") (make-pw "16-17 t: " "ftgstrgptwmptxrzt") (make-pw "9-10 b: " "bbbbbbbzbbbb") (make-pw "8-14 w: " "wwwwwwwjwwwzwzww") (make-pw "3-4 h: " "qhnh") (make-pw "2-12 d: " "dnddddkddddzdxdddd") (make-pw "5-17 x: " "xxxxxxxxxxxxxxxxx") (make-pw "9-10 h: " "hhhhtqhhhrh") (make-pw "6-7 p: " "pfmppppp") (make-pw "5-8 v: " "vbvvvvvs") (make-pw "4-5 h: " "qhfhqcb") (make-pw "3-12 d: " "dlddlhhwvcrdrxwpt") (make-pw "2-3 n: " "nnwnp") (make-pw "4-8 g: " "ggggjgfgzgdglgg") (make-pw "4-9 h: " "vrghsphxhxzsxw") (make-pw "12-18 z: " "qrzzzfwdcwnzdzkckz") (make-pw "6-8 v: " "zvmlqwwh") (make-pw "10-14 p: " "pppxwpndfpwppdpptmpp") (make-pw "12-15 q: " "qqnqkqkqjgrcqfq") (make-pw "2-4 n: " "pnszjnnn") (make-pw "2-3 c: " "zkctcfc") (make-pw "17-20 l: " "lvjlcclllslzllllwgll") (make-pw "16-17 k: " "dfgskkfkkkfjhfvfks") (make-pw "1-3 t: " "tjttltt") (make-pw "2-3 b: " "bbzbb") (make-pw "8-15 t: " "dtttttsttlttttzlttj") (make-pw "13-14 v: " "vvkvvjvgwvvvkvf") (make-pw "7-8 v: " "tvvvvvvhv") (make-pw "3-10 d: " "bxktdrtddtdtsh") (make-pw "4-12 j: " "npwxjjjjbjkq") (make-pw "1-4 l: " "tlllll") (make-pw "12-13 b: " "lbhpxbbbvbbbqbbbsbb") (make-pw "9-15 v: " "vqhsggmpvmqtbzzlq") (make-pw "12-14 f: " "ffhfjfffqfxqff") (make-pw "15-16 m: " "jtnsjwpggbpxlhqmk") (make-pw "2-4 n: " "xptncjsstcl") (make-pw "5-8 q: " "dmwklqjqnzb") (make-pw "3-4 c: " "vcgl") (make-pw "1-6 h: " "hhhhhh") (make-pw "8-9 x: " "ckblstcdx") (make-pw "5-7 w: " "vwphwwmwwwww") (make-pw "2-4 x: " "kxxxdh") (make-pw "10-11 m: " "kmmvmmmfmksmj") (make-pw "4-7 v: " "zvrvvvdvvv") (make-pw "1-8 p: " "npxbwqpxbjrnrv") (make-pw "5-11 t: " "jfkwttkstrxlgts") (make-pw "1-2 g: " "ggggg") (make-pw "5-7 p: " "tpppppr") (make-pw "15-17 v: " "vvvvvvvvvvvvvvvvvvvv") (make-pw "17-19 w: " "dwjwjznczwgfmkmhdtw") (make-pw "10-13 f: " "fffffbqfffffffffff") (make-pw "4-14 g: " "ggggggggggggggg") (make-pw "2-3 d: " "bdkfd") (make-pw "7-8 x: " "xxxxxxxx") (make-pw "6-9 h: " "hmshdhvvhkhbhcshs") (make-pw "14-15 g: " "gggggggggggggghg") (make-pw "2-16 q: " "qqsxqqqdgqqghqqk") (make-pw "3-17 p: " "ppbppprppppppppphppp") (make-pw "8-18 v: " "vczfvqcvvcspndvxwjdv") (make-pw "1-3 d: " "fdddd") (make-pw "9-11 j: " "gfjjnjsdnhb") (make-pw "8-10 s: " "hkhshttssl") (make-pw "5-10 f: " "ffsffffffqxfff") (make-pw "7-10 w: " "wwlwwckwwf") (make-pw "4-8 x: " "blxxmtbgnblfgnfwz") (make-pw "4-5 p: " "rlpkprppp") (make-pw "13-14 s: " "ssssssssssssswss") (make-pw "4-8 h: " "hhhdhmhhhjhlhh") (make-pw "6-7 h: " "chhhhhfhzqhdhhh") (make-pw "15-17 m: " "msvrmwzkzvmmgrmmpm") (make-pw "4-6 x: " "qgtwwxhgsxxmklgmn") (make-pw "4-7 p: " "rpzkdpp") (make-pw "3-4 v: " "vvvhvjv") (make-pw "16-18 p: " "pppppppppppppppppmp") (make-pw "15-17 k: " "kpkkkkkhqkkkkklktk") (make-pw "5-7 s: " "ssbxxsk") (make-pw "2-4 l: " "cwllll") (make-pw "6-9 v: " "cvvrrkvrvsdvfwcv") (make-pw "1-7 w: " "qwwwwslwwwwwwwwrww") (make-pw "2-12 c: " "clcccccccccccccc") (make-pw "5-10 m: " "ftcmrpmvrzc") (make-pw "7-11 w: " "wwwwwwwwwwtwwwww") (make-pw "2-3 n: " "jngdlvgcvtkmn") (make-pw "1-8 v: " "hvvvvvvvvv") (make-pw "3-17 p: " "jrpvltxlcqgpfxwsj") (make-pw "1-5 k: " "kkkkfkk") (make-pw "1-10 c: " "cccccccccfc") (make-pw "13-19 f: " "flfffffxfvffffprfmcb") (make-pw "3-6 g: " "htglsbvrzcghjmd") (make-pw "9-11 h: " "wrwghhhhnzhxl") (make-pw "5-12 z: " "zzkzzhnjpmkvzzzw") (make-pw "7-11 w: " "qrwcwwstwddw") (make-pw "15-19 m: " "mmmmmmmmmmmmmmmmmmmm") (make-pw "15-16 m: " "mmmmmmmmmmmmmmhmm") (make-pw "12-16 x: " "xxxxxcxxxxxlxxxxxxx") (make-pw "5-10 l: " "qwfqlllgsdjrlspll") (make-pw "3-4 f: " "hffmfffg") (make-pw "6-7 l: " "lwlllbllnl") (make-pw "11-20 t: " "tttkrtlpttwftmwttttt") (make-pw "11-12 d: " "ddbdddddddpktdd") (make-pw "4-11 w: " "sxkmkwdwwnlwxmdvfx") (make-pw "4-5 k: " "kkzkpk") (make-pw "13-20 n: " "nnnnnntnnnnnbnnnbnnn") (make-pw "5-7 c: " "ccccwcc") (make-pw "1-6 h: " "cckkhhdhhwmhhmzchhwx") (make-pw "1-15 b: " "hbbbbbbbbhbbhbbbbnbb") (make-pw "1-2 w: " "mwwww") (make-pw "1-2 f: " "sfzgwtf") (make-pw "3-4 s: " "sssj") (make-pw "1-3 w: " "xwww") (make-pw "1-16 p: " "ppppppppdppppppp") (make-pw "1-6 c: " "ccxmccccc") (make-pw "4-8 b: " "rtpbcfbr") (make-pw "9-10 s: " "sszssstshss") (make-pw "15-16 x: " "xxxtxxxxxxxmxxxzx") (make-pw "9-15 k: " "kkkkkkkkkkgrkkkkk") (make-pw "11-13 x: " "xtxxxxxxxxrxkxx") (make-pw "1-5 f: " "rffflfffnf") (make-pw "14-16 n: " "nnxnnnnnnnnnbnzbnnn") (make-pw "3-6 d: " "ddddddndbdfdhd") (make-pw "14-15 q: " "sbqqhvqqqvqgxfq") (make-pw "5-6 b: " "jglbfjb") (make-pw "1-10 l: " "llllllcllsll") (make-pw "3-5 z: " "lwzzz") (make-pw "5-12 k: " "kkkkmkkkkkkkkkkl") (make-pw "8-12 v: " "vvtsvfvnzvhpm") (make-pw "5-7 l: " "llllllll") (make-pw "4-7 f: " "nqfffkbdf") (make-pw "4-8 j: " "jjjljkhj") (make-pw "9-15 h: " "vgzpgfhfhmwdhbqc") (make-pw "1-5 p: " "ptpsjqpnp") (make-pw "5-8 q: " "qbqqwqnq") (make-pw "5-7 w: " "wwswwxsb") (make-pw "3-6 h: " "nnhrnhkmxqkt") (make-pw "1-3 w: " "jwwww") (make-pw "2-14 x: " "cxxxxdxxxxxxxbxx") (make-pw "12-14 x: " "xxxfxxxxqxxrxt") (make-pw "4-9 f: " "fffkffbsfkxv") (make-pw "12-13 m: " "mmmmmmmmmmmwm") (make-pw "6-10 p: " "ppppphpppxppp") (make-pw "9-12 v: " "vsvvvvkmjvvvn") (make-pw "3-12 r: " "shngvhbmjrpr") (make-pw "12-13 w: " "wqwcwxclwwwfw") (make-pw "4-5 r: " "nnrrdz") (make-pw "4-19 c: " "vbcpwzvxssccqkqgmxvj") (make-pw "1-6 g: " "tglxhggng") (make-pw "9-10 z: " "lvzhvtglzf") (make-pw "10-16 f: " "gddxfftggfbmxwts") (make-pw "4-12 k: " "kkkklkkkkkkpkkk") (make-pw "7-11 m: " "mmmmmmmmmmtm") (make-pw "2-10 z: " "vkfpjrrvlwlbjwk") (make-pw "3-7 w: " "dtdzwjqgxdwjhchwwd") (make-pw "13-14 v: " "vvvvvvvvvvvvvvvv") (make-pw "11-12 c: " "cjccccqccccc") (make-pw "14-15 h: " "hzhrhkhfthrhxht") (make-pw "3-5 v: " "dtwvvvvvcvvvrvsvvv") (make-pw "10-14 q: " "qhqqkqcqqqqqlfqgsqq") (make-pw "3-14 f: " "ffvffffffffzffffff") (make-pw "1-12 x: " "xxtxjxzxxxxxlxxxxxxx") (make-pw "2-4 c: " "pgpc") (make-pw "1-4 r: " "fdrr") (make-pw "5-7 r: " "rrrrrrt") (make-pw "10-11 d: " "drdddddddmddddpd") (make-pw "2-5 f: " "gfrlctftzr") (make-pw "10-13 z: " "nrzjjrzjzzplzmzzbn") (make-pw "8-10 c: " "cccccccgbc") (make-pw "5-8 g: " "fgggcndwgggbjnfgb") (make-pw "6-9 d: " "ddddghdgbddm") (make-pw "3-7 j: " "ftjjjvjqcp") (make-pw "5-6 s: " "sqvfstz") (make-pw "5-18 k: " "kkkwkkkkkkbkkkkkkkbk") (make-pw "3-6 f: " "djffffkff") (make-pw "4-6 p: " "pqppppr") (make-pw "5-9 h: " "hhkhjhmgcqvfhqvhn") (make-pw "8-12 t: " "tttttttvttttt") (make-pw "14-15 z: " "pzrzhjqmtbcnzdzr") (make-pw "1-17 s: " "ssssssssssssswsss") (make-pw "1-4 s: " "ssqh") (make-pw "1-5 r: " "rrrrrrr") (make-pw "17-18 h: " "hhhhhrhcxhhhhhhhhb") (make-pw "7-9 t: " "dttttjmtv") (make-pw "1-3 c: " "mcccp") (make-pw "4-5 x: " "xprbxql") (make-pw "1-5 q: " "cqqqqqq") (make-pw "3-4 g: " "ggggw") (make-pw "1-7 f: " "ffffffnf") (make-pw "7-9 t: " "ttttttttttt") (make-pw "11-15 m: " "mmlmfmmmmmmmmmmqm") (make-pw "4-5 n: " "npnnnn") (make-pw "4-5 b: " "zvbbvbjhlkf") (make-pw "11-16 c: " "gnqmcvtzwpcbvncwcc") (make-pw "12-18 q: " "qbqkwqqvqwqqnljsqpqt") (make-pw "6-10 r: " "wcrtdrlkgjr") (make-pw "16-17 p: " "gppppppppwvlgpptp") (make-pw "1-13 g: " "gqghbwqqzwwdk") (make-pw "1-6 l: " "rlllll") (make-pw "3-7 j: " "pdjjtcqwbqtpfkjbwgq") (make-pw "7-17 z: " "zzhsnjrhrzzfrqszdhdg") (make-pw "6-7 n: " "xnvnhnrn") (make-pw "6-7 r: " "rrrrrrzr") (make-pw "2-3 c: " "ccmcccc") (make-pw "2-3 v: " "vvdv") (make-pw "3-9 f: " "mlfffshbfdff") (make-pw "11-13 v: " "dfvkltvjvvvvx") (make-pw "12-13 c: " "mqrccccbccbgcccccvc") (make-pw "9-13 w: " "wwwwwwwwwwwwcwtww") (make-pw "6-8 d: " "pdvgddtmvwdkvdtzf") (make-pw "1-9 s: " "hsssssssssdsls") (make-pw "1-5 c: " "zbsslcd") (make-pw "1-4 p: " "bpppppppppp") (make-pw "13-16 s: " "sssxslsscssbqsspcs") (make-pw "1-10 p: " "pskwpppzpppppks") (make-pw "3-5 c: " "ccppccmcc") (make-pw "10-11 b: " "rhbbbbbbbzbb") (make-pw "2-3 d: " "ndhjhd") (make-pw "3-4 s: " "sscs") (make-pw "5-6 d: " "dtwwnt") (make-pw "4-5 d: " "dddddddd") (make-pw "5-9 z: " "zztzvzzzz") (make-pw "2-6 v: " "vpqdll") (make-pw "13-14 d: " "nxkmbkkpxkcdld") (make-pw "1-3 s: " "ssss") (make-pw "8-14 z: " "zzzzzzzzzzzzzbz") (make-pw "16-17 f: " "fffnffffffftbzffpkf") (make-pw "3-4 m: " "mtcm") (make-pw "3-4 r: " "mrdrd") (make-pw "11-13 k: " "ljkmhdkkkcpjzlmkkzkk") (make-pw "2-3 d: " "tdqnxpd") (make-pw "3-7 h: " "mrvdlthxchpvwvssqpk") (make-pw "13-17 j: " "jjfjjvjjjjjzjsjjksxr") (make-pw "1-4 n: " "rnnx") (make-pw "7-10 m: " "mmmmzmxfmm") (make-pw "1-6 r: " "lrrvrrrrm") (make-pw "4-18 r: " "rrrdrrrrrrrrrkblrr") (make-pw "6-7 k: " "kkkkkkl") (make-pw "4-6 v: " "vmnfvvvvmcmlh") (make-pw "6-9 g: " "jgcgggkbbmgbs") (make-pw "7-8 t: " "ttcfwtgjtcttv") (make-pw "3-4 j: " "tjjj")))
