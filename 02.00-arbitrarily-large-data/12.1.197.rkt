;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.1.197) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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

; (define (most-frequent lol) (make-lettercount "" 0)) ;stub

(define (most-frequent lol)
  (cond [(empty? lol) (make-lettercount "" 0)]
        [else
         (find-biggest-count (first lol)
                             (most-frequent (rest lol)))]))

; LetterCount LetterCount -> LetterCount
; find bigger letter count given the argument and the first element of the second argument
(check-expect (find-biggest-count (make-lettercount "a" 2) (make-lettercount "" 0))
                (make-lettercount "a" 2))
(check-expect (find-biggest-count (make-lettercount "a" 2) (make-lettercount "b" 1))
                (make-lettercount "a" 2))
(check-expect (find-biggest-count (make-lettercount "a" 2) (make-lettercount "c" 5))
                (make-lettercount "c" 5))

; (define (find-biggest-count l lol) (make-lettercount "" 0)) ;stub

(define (find-biggest-count l1 l2)
  (if (> (lettercount-count l1) (lettercount-count l2))
      l1
      l2))