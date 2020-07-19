;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.1-175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define-struct wordcount [1strings words lines])
; An Wordcount is a structure:
;  (make-wordcount Number Number Number)
; interpretation (make-wordcount 1s w l) combines
; the number of 1Strings, words and lines in a given file

; An LLS is one of: 
  ; – '()
  ; – (cons Los LLS)
; interpretation a list of lines, each is a list of Strings

#; (define (fn-for-lls lls)
  (cond
    [(empty? lls) ...]
    [else
     (.. (first lls)
         (fn-for-lls (rest lls)))]))
 
(define line0 (cons "   hello" (cons " world" '())))
(define line1 '())

(define line2 (cons "hello" (cons "from" '())))
(define line3 (cons "Albert" '()))
(define line4 (cons "Maupstein" '()))
(define line5 (cons "    " (cons "!!!!" (cons " " (cons ":)" (cons "  " (cons "!!!!" (cons "  " (cons ":)" '())))))))))
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(define lls3 (cons line2 (cons line3 (cons line4 (cons line5 '())))))

; LLS -> Number
; count number of 1Strings in a LLS
(check-expect (count-1s-lls lls0) 0)
(check-expect (count-1s-lls lls1) 10)

; (define (count-1s-lls lls) 0) ;stub

(define (count-1s-lls lls)
  (cond
    [(empty? lls) 0]
    [else
     (+ (count-1s-los (first lls))
        (count-1s-lls (rest lls)))]))

; Los -> Number
; count number of 1Strings in a ListOfStrings
(check-expect (count-1s-los line1) 0)
(check-expect (count-1s-los line0) 10)

; (define (count-1s-los los) 0) ;stub

(define (count-1s-los los)
  (cond
    [(empty? los) 0]
    [else
     (+ (count-non-empty-chars (first los))
        (count-1s-los (rest los)))]))

; String -> Number
; count non empty characters in a given string

; (define (count-non-empty-chars s) 0) ;stub

(define (count-non-empty-chars s)
  (count-non-empty-chars-lo1s (explode s)))

; ListOf1Strings -> Number
; counts non empty characters in a given list
(check-expect (count-non-empty-chars-lo1s (cons "" '())) 0)
(check-expect (count-non-empty-chars-lo1s (cons "a" '())) 1)
(check-expect (count-non-empty-chars-lo1s (cons "a" (cons "b" (cons "\n" '())))) 2)

; (define (count-non-empty-chars-lo1s lo1s) 0) ;stub

(define (count-non-empty-chars-lo1s lo1s)
  (cond
    [(empty? lo1s) 0]
    [else
     (if (or
          (string=? (first lo1s) "")
          (string=? (first lo1s) " ")
          (string=? (first lo1s) "\n"))
         (count-non-empty-chars-lo1s (rest lo1s))
         (+ 1 (count-non-empty-chars-lo1s (rest lo1s))))]))

; LLS -> Number
; count number of words in LLS
(check-expect (count-words-lls lls0) 0)
(check-expect (count-words-lls lls1) 2)

; (define (count-words-lls lls) 0) ;stub

(define (count-words-lls lls)
  (cond
    [(empty? lls) 0]
    [else
     (+ (length (first lls))
         (count-words-lls (rest lls)))]))

; LLS -> Number
; count number of words in LLS
(check-expect (count-lines-lls lls0) 0)
(check-expect (count-lines-lls lls1) 2)

; (define (count-words-lls lls) 0) ;stub

(define (count-lines-lls lls)
  (cond
    [(empty? lls) 0]
    [else
     (length lls)]))


; String -> Wordcount
; count the number of 1Strings, words and lines
; in a file provided by the argument (path)

; (define (wc p)
;  (make-wordcount 0 0 0)) ; stub

(define (wc f)
  (make-wordcount
   (count-lines-lls (read-words/line f))
   (count-words-lls (read-words/line f))
   (count-1s-lls (read-words/line f))))


(wc "10.1-175.txt")