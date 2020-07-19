;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11.3.187) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

(define GP1 (make-gp "Albert" 10))
(define GP2 (make-gp "Klara" 20))
(define GP3 (make-gp "Anders" 5))

; List-of-gp is one of:
;  - '()
;  - (cons GamePlayer List-of-gp)

; List-of-gp -> List-of-gp
; sort the list in the descending order

(check-expect (sort-log '()) '())
(check-expect (sort-log (list GP1 GP2 GP3))
              (list GP2 GP1 GP3))
(check-expect (sort-log (list GP3 GP2 GP1))
              (list GP2 GP1 GP3))

; (define (sort-log log) log) ;stub

(define (sort-log log)
  (cond
    [(empty? log) '()]
    [else
     (insert (first log)
          (sort-log (rest log)))]))

; GamePlayer List-of-gp -> List-of-gp
; instert game player into the ordered list list,
; at the beggining of the list if it's bigger than the first item on the list
; as the second item of the list if it's lower than the fist item on the list
(check-expect (insert GP1 '()) (list GP1))
(check-expect (insert GP2 (list GP1 GP3)) (list GP2 GP1 GP3))
(check-expect (insert GP1 (list GP2 GP3)) (list GP2 GP1 GP3))

; (define (insert gp log) log) ;stub

(define (insert gp log)
  (cond
    [(empty? log) (list gp)]
    [else
     (if (gp>? gp (first log))
         (cons gp log)
         (cons (first log) (insert gp (rest log))))]))

; GamePlayer -> GamePlayer
; compare two GamePlayer entities
(check-expect (gp>? GP1 GP2) false)
(check-expect (gp>? GP2 GP3) true)

; (define (gp>? gp1 gp2) true) ;stub

(define (gp>? gp1 gp2)
  (> (gp-score gp1) (gp-score gp2)))