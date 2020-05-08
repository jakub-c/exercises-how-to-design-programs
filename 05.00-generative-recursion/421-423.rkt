;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |421|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
;(define (bundle s n) '()) ;stub

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if posssible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

; ==================== Exercise 421 ====================

; (bundle '("a" "b" "c") 0) ;- produces infinite loop
; because of the argument 0 it never takes nor drops
; anything, the list is never exhausted so the loop
; never stops

; =================== End of exercise ==================

; ==================== Exercise 422 ====================

; [X] [List-of X] Number -> [List-of [List-of X]]
; produce a list of list chunks of size n
; (define (list->chunks l n) '(())) ;stub

(check-expect (list->chunks (explode "abcdefg") 2)
              '(("a" "b")
                ("c" "d")
                ("e" "f")
                ("g")))

(define (list->chunks l n)
  (cond [(empty? l) '()]
        [else
         (cons (take l n)
               (list->chunks (drop l n) n))]))

(check-expect (bundle.v2 (explode "abcdefg") 3)
              (list "abc" "def" "g"))

(define (bundle.v2 l n)
  (map implode (list->chunks l n)))

; =================== End of exercise ==================

; ==================== Exercise 423 ====================

; String Number -> [List-of [List-of String]]
; (define (partition s n) '(())) ;stub

(check-expect (equal? (partition "abcdef" 3) (bundle.v2 (explode "abcdef") 3))
              #t)

(define (partition s n)
  (local ((define string-length (length (explode s)))
          (define substr-max-length (if (> n string-length)
                                        string-length
                                        n)))
    (cond [(string=? s "") '()]
          [else
           (cons (substring s 0 substr-max-length)
                 (partition (substring s substr-max-length) n))])))

; =================== End of exercise ==================
