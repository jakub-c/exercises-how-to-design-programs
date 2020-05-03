;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |400|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require racket/list)

; ==================== Exercise 400 ====================

; DNAdescription is one of:
;  - 'a
;  - 'c
;  - 'g
;  - 't

; [List-of DNAdescription] [List-of DNAdescription] -> Boolean
; function returns #true if the pattern is identical to the
; initial part of the search string; otherwise it returns #false
; (define (DNAprefix pattern search) #f) ;stub

(check-expect (DNAprefix '() '(a c g)) #true)
(check-expect (DNAprefix '(a c) '(a c g)) #true)
(check-expect (DNAprefix '(a c a) '(a c g)) #false)
(check-expect (DNAprefix '(a c) '(a)) #false)

; version using abstraction
#;(define (DNAprefix pattern search)
    (if (> (length search) (length pattern))
        #false
        (for/and ([s search] [p pattern])
          (equal? s p))))

(define (DNAprefix pattern search)
  (cond [(empty? pattern) #true]
        [(and (cons? pattern) (empty? search)) #false]
        [else
         (and
          (equal? (first pattern) (first search))
          (DNAprefix (rest pattern) (rest search)))]))

; [List-of DNAdescription] [List-of DNAdescription] -> DNAdescription
; returns the first item in the search string beyond the pattern
; (define (DNAdelta pattern search) 'a) ;stub

(check-expect (DNAdelta '() '(a c g)) 'a)
(check-expect (DNAdelta '(a c) '(a c g)) 'g)
(check-error (DNAdelta '(a c) '(a c)))
(check-expect (DNAdelta '(a g) '(a c c)) #false)

; first implementation
#;(define (DNAdelta pattern search)
  (cond [(and (empty? pattern) (cons? search)) (first search)]
        [(and (empty? pattern) (empty? search)) (error "lists are the same size")]
        [else
         (if (equal? (first pattern) (first search))
             (DNAdelta (rest pattern) (rest search))
             #false)]))

; simplified cond
(define (DNAdelta pattern search)
  (cond [(empty? pattern) (first search)]
        [(empty? search) (error "lists are the same size")]
        [else
         (if (equal? (first pattern) (first search))
             (DNAdelta (rest pattern) (rest search))
             #false)]))



; =================== End of exercise ==================
