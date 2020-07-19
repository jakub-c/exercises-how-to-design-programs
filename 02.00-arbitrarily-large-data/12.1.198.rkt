;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.1.197) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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
(check-expect (member '() (words-by-first-letter LETTERS '()))
              #true)
(check-expect (member (list "a") (words-by-first-letter LETTERS (list "a")))
              #true)
(check-expect
 (and (member (list "a" "ab" "abc") (words-by-first-letter LETTERS (list "a" "ab" "abc" "b" "bc" "bcd" "g")))
      (member (list "b" "bc" "bcd") (words-by-first-letter LETTERS (list "a" "ab" "abc" "b" "bc" "bcd" "g")))
      (member (list "g") (words-by-first-letter LETTERS (list "a" "ab" "abc" "b" "bc" "bcd" "g"))))
 #true)

; (define (words-by-first-letter d) (list (list "a" "aa") (list "b" "bb"))) ;stub

(define (words-by-first-letter los d)
  (cond [(empty? los) '()]
        [else
         (cons (add-to-list-by-letter (first los) d)
               (words-by-first-letter (rest los) d))]))

; 1String Dictionary -> List-of-strings
; create a list of strings that start with a given letter
(check-expect (add-to-list-by-letter "" '()) '())
(check-expect (add-to-list-by-letter "" (list "a" "b")) '())
(check-expect (add-to-list-by-letter "a" (list "a" "ab" "b")) (list "a" "ab"))
(check-expect (add-to-list-by-letter "b" (list "a" "ab" "b")) (list "b"))

;(define (add-to-list-by-letter l d) '()) ;stub

(define (add-to-list-by-letter l d)
  (cond [(empty? d) '()]
        [else
         (if (word-starts-with l (first d))
             (cons (first d) (add-to-list-by-letter l (rest d)))
             (add-to-list-by-letter l (rest d)))]))

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



; ---------
; ---------

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

; List-of-1Strings Dictionary -> List-of-LetterCount
; count how many words start with a given letter from the list of 1Strings

; (define (most-frequent.v2 los d) '()) ;stub

(define (most-frequent.v2 los d)
  (cond
    [(empty? los) '()]
    [(empty? d) '()]
    [else
     (count-by-los-lod LETTERS (words-by-first-letter los d))]))

(check-expect (most-frequent.v2 LETTERS '())
              '())
(check-expect (member (make-lettercount "a" 2) (most-frequent.v2 LETTERS (list "a" "ab")))
              #true)
(check-expect (and
               (member (make-lettercount "a" 2) (most-frequent.v2 LETTERS (list "a" "ab" "b" "bc" "bcd")))
               (member (make-lettercount "b" 3) (most-frequent.v2 LETTERS (list "a" "ab" "b" "bc" "bcd"))))
              #true)

; List-of-1Strings List-of-dictoinaries -> List-ofLetterCount
; count the occurences of a given letter in a list-of-dictionaries
(check-expect (count-by-los-lod LETTERS '())
              '())
(check-expect (member (make-lettercount "a" 2) (count-by-los-lod LETTERS (list (list "a" "ab") '())))
              #true)
(check-expect (and
               (member (make-lettercount "a" 2) (count-by-los-lod LETTERS (list (list "a" "ab")
                                                                                (list "b"))))
               (member (make-lettercount "b" 3) (count-by-los-lod LETTERS (list (list "a" "ab")
                                                                                (list "b" "bc" "bcd")))))
              #true)

;(define (count-by-los-lod los lod) (list (make-lettercount "" 0))) ;stub

(define (count-by-los-lod los lod)
  (cond
    [(empty? los) (list (make-lettercount "" 0))]
    [(empty? lod) '()]
    [else
     (cons
      (if (empty? lod)
          (make-lettercount (first los) 0)
          (make-lettercount (first los) (length (first lod))))
      (count-by-los-lod (rest los) (rest lod)))]))