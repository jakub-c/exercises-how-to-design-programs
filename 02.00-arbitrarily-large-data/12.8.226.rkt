;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.8.226) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))


(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; Transition Transition -> Boolean
; compare two states
(check-expect (state=? (make-transition "green" "yellow")
                       (make-transition "green" "yellow"))
              #true)
(check-expect (state=? (make-transition "yellow" "red")
                       (make-transition "green" "yellow"))
              #false)
(check-expect (state=? (make-transition "yellow" "red")
                       (make-transition "yellow" "yellow"))
              #false)

; (define (state=? s1 s2) #false) ;stub
(define (state=? s1 s2)
  (and (string=? (transition-current s1)
          (transition-current s2))
       (string=? (transition-next s1)
          (transition-next s2))))