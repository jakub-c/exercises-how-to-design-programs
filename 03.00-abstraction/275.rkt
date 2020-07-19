;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |275|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct lettercount [letter count])
; A LetterCount is a structure
; it is used to describe how many times
; a given letter occurs
;   (make-lettercount 1String Number).

; List-of-LetterCounts is a one of:
;  - '()
;  - (cons LetterCount List-of-LetterCounts)
; interpretation a list of letters and
; number of times they are the first letter
; of a word
;
(define LETTER-OCCURRENCES (list
                            (make-lettercount "a" 14537)
                            (make-lettercount "b" 9675)
                            (make-lettercount "c" 17406)
                            (make-lettercount "d" 9946)
                            (make-lettercount "e" 7818)
                            (make-lettercount "f" 6382)
                            (make-lettercount "g" 5843)
                            (make-lettercount "h" 7889)
                            (make-lettercount "i" 8303)
                            (make-lettercount "j" 1158)
                            (make-lettercount "k" 1735)
                            (make-lettercount "l" 5211)
                            (make-lettercount "m" 10709)
                            (make-lettercount "n" 6098)
                            (make-lettercount "o" 7219)
                            (make-lettercount "p" 22171)
                            (make-lettercount "q" 1075)
                            (make-lettercount "r" 8955)
                            (make-lettercount "s" 22759)
                            (make-lettercount "t" 11389)
                            (make-lettercount "u" 16179)
                            (make-lettercount "v" 3079)
                            (make-lettercount "w" 3607)
                            (make-lettercount "x" 293)
                            (make-lettercount "y" 532)
                            (make-lettercount "z" 719)))


; List-of-LetterCounts -> LetterCount
; find the most frequently used letter from the List-of-LetterCounts
(check-expect (most-frequent '()) (make-lettercount "" 0))
(check-expect (most-frequent LETTER-OCCURRENCES) (make-lettercount "s" 22759))

(define (most-frequent l)
  (local (; LetterCount LetterCount -> Boolean
          (define (compare-lc lc1 lc2)
            (> (lettercount-count lc1)
               (lettercount-count lc2))))
    (cond [(empty? l) (make-lettercount "" 0)]
          [else (first (sort l compare-lc))])))



(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
; (define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; List-of-1Strings Dictionary -> List-of-Dictionaries
; The function consumes a Dictionary and List-of-strings and it produces a list of Dictionarys, one per Letter
(check-expect (member (list "a") (words-by-first-letter LETTERS (list "a")))
              #true)
(check-expect
 (and (member (list "a" "ab" "abc") (words-by-first-letter LETTERS (list "a" "ab" "abc" "b" "bc" "bcd" "g")))
      (member (list "b" "bc" "bcd") (words-by-first-letter LETTERS (list "a" "ab" "abc" "b" "bc" "bcd" "g")))
      (member (list "g") (words-by-first-letter LETTERS (list "a" "ab" "abc" "b" "bc" "bcd" "g"))))
 #true)

(define (words-by-first-letter letters dict)
  (local (; 1String -> List-of-String
          (define (add-words-starting-with x)
            (local (; 1String -> String / '()
                    (define (starts-with-x? word list)
                      (if (string=? (first (explode word)) x)
                          (cons word list)
                          list)))
              (foldr starts-with-x? '() dict))))
    (map add-words-starting-with dict)))