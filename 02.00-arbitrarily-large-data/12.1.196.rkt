;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.1.196) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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

; ListOfLetters Dictorinary -> ListOfLetterCounts
; count occurences of a given letter from the list
; in a dictionary
(check-expect (count-by-lol '() '()) '())
(check-expect (member (make-lettercount "a" 2)
                      (count-by-lol LETTERS (list "apple" "albert")))
              #true)
(check-expect (and
               (member
                (make-lettercount "a" 2)
                (count-by-lol LETTERS (list "apple" "albert" "banana")))
               (member
                (make-lettercount "b" 1)
                (count-by-lol LETTERS (list "apple" "albert" "banana"))))
              #true)

;(define (count-by-lol lol d) '()) ;stub
(define (count-by-lol lol d)
  (cond
    [(empty? lol) '()]
    [else
     (cons (count-by-letter (first lol) d)
           (count-by-lol (rest lol) d))]))

; 1String Dictonary -> LetterCount
(check-expect (count-by-letter "" '()) '())
(check-expect (count-by-letter "a" (list "apple" "albert")) (make-lettercount "a" 2))
(check-expect (count-by-letter "b" (list "apple" "albert" "banana")) (make-lettercount "b" 1))

; (define (count-by-letter d) '()) ;stub

(define (count-by-letter s d)
  (cond [(empty? d) '()]
        [else
         (make-lettercount
          s
          (starts-with# s d))]))

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

