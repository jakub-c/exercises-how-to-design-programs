;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.1.195) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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

; Letter Dictonary -> Number;
; count how many words in the given Dictionary

(check-expect (starts-with# "a" '()) 0)
(check-expect (starts-with# "a" (list "apple" "albert" "atlanta")) 3)
(check-expect (starts-with# "a" (list "Apple" "albert" "atlanta")) 2)
(check-expect (starts-with# "a" (list "apple" "albert" "banana")) 2)

; (define (starts-with# letter dict) 0) ;stub
(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [else
     (if (word-starts-with letter (first dict))
         (+ 1 (starts-with# letter (rest dict)))
         (+ 0 (starts-with# letter (rest dict))))]))

; Letter String -> Bool
; checks if the word starts with the letter provided
(check-expect (word-starts-with "a" "apple") #true)
(check-expect (word-starts-with "b" "apple") #false)
(check-expect (word-starts-with "a" "Apple") #false)
(check-expect (word-starts-with "A" "Apple") #true)
(check-expect (word-starts-with "" "Apple") #false)

; (define (word-starts-with letter word) #false) ;stub

(define (word-starts-with letter word)
  (string=? letter (first (explode word))))

