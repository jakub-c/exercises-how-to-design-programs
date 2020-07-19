;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.8.229) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

; FSM-State is a String.

(define fsm-letters
  (list (make-ktransition "AA" "a" "BB")
        (make-ktransition "BB" "b" "BB")
        (make-ktransition "BB" "c" "BB")))

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; Transition.v2 FM-State -> Boolean
; compare two states
(check-expect (state=? (make-ktransition "AA" "a" "BB")
                       "AA")
              #true)
(check-expect (state=? (make-ktransition "AA" "a" "BB")
                       "BB")
              #false)
(check-expect (state=? (make-ktransition "BB" "b" "BB")
                       "AA")
              #false)

; (define (state=? s1 s2) #false) ;stub
(define (state=? ktransition state)
  (string=? (ktransition-current ktransition) state))


(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)


; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect
 (find-next-state.v2 (make-fs fsm-letters (make-ktransition "AA" "a" "BB")) "a")
 (make-fs fsm-traffic (make-ktransition "BB" "b" "CC")))
(check-expect
 (find-next-state.v2 (make-fs fsm-letters (make-ktransition "BB" "b" "BB")) "b")
 (make-fs fsm-traffic (make-ktransition "BB" "b" "BB")))
(check-expect
 (find-next-state.v2 (make-fs fsm-letters (make-ktransition "BB" "c" "CC")) "c")
 (make-fs fsm-traffic (make-ktransition "BB" "c" "BB")))

#;(define (find-next-state.v2 cs ke)
    cs) ;stub

(define (find-next-state.v2 an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-letters (make-ktransition "AA" "a" "BB")) (make-ktransition "BB" "b" "BB"))
(check-expect (find fsm-letters (make-ktransition "BB" "b" "BB")) (make-ktransition "BB" "b" "BB"))
(check-expect (find fsm-letters (make-ktransition "BB" "c" "BB")) (make-ktransition "BB" "b" "BB"))
#;(check-error (find fsm-traffic "black")
             "not found: black")

(define (find transitions current)
  (cond [(empty? transitions) (error (string-append "not found: " current))]
        [else
         (if (state=? (first transitions) current)
             (transition-next (first transitions))
             (find (rest transitions) current))]))


; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state.v2]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))