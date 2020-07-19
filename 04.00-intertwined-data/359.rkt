;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |359|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 358

(define-struct add [left right])
(define-struct mul [left right])

; Fun is a structure:
;  - (make-fun Symbol Symbol BSL-var-exp
(define-struct fun [name parameter body])

; (define (f x) (+ 3 x))
(make-fun 'f 'x (make-add 3 'x))

; (define (g y) (f (* 2 y)))
(make-fun 'g 'x (make-add 3 (make-mul 2 'y)))

;(define (h v) (+ (f v) (g v)))
(make-fun 'h 'v (make-add
                 (make-add 3 'v)
                 (make-add 3 (make-mul 2 'v))))


; BSL-fun-def* is:
;  - '()
;  - (cons BSL-fun-dev '())

(define da-fgh
  (list (make-fun 'f 'x (make-add 3 'x))
        (make-fun 'g 'x (make-add 3 (make-mul 2 'y)))
        (make-fun 'h 'v (make-add
                         (make-add 3 'v)
                         (make-add 3 (make-mul 2 'v))))))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g)
              (make-fun 'g 'x (make-add 3 (make-mul 2 'y))))

(define (lookup-def da f)
  (local ((define result (filter (lambda (el)
            (equal? (fun-name el) f))
          da)))
    (first result)))

(eval-function* expr 