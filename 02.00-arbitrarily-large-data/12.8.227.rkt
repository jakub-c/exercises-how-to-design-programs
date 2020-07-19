;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.8.227) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define fsm-bw
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; Transition Transition -> Boolean
; compare two states
(check-expect (state=? (make-transition "green" "yellow")
                       (make-transition "green" "yellow" ))
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

; A SimulationState.v1 is an FSM-State.

; A SimulationState.v1  -> ???
; match the keys pressed with the given FSM 
#;(define (simulate.v1 fsm0)
  (big-bang initial-state
    [to-draw render-state.v1]
    [on-key find-next-state.v1]))

; SimulationState.v1 -> Image
; renders a world state as an image 
#;(define (render-state.v1 s)
  empty-image)

; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
#;(define (find-next-state.v1 cs ke)
   cs)


(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image 
(define (render-state.v2 s)
  empty-image)
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect
  (find-next-state.v2 (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state.v2 (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state.v2 (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))

#;(define (find-next-state.v2 cs ke)
   cs) ;stub

(define (find-next-state.v2 an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black")
             "not found: black")

(define (find transitions current)
  (cond [(empty? transitions) (error (string-append "not found: " current))]
        [else
         (if (string=? (transition-current (first transitions)) current)
             (transition-next (first transitions))
             (find (rest transitions) current))]))


; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
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