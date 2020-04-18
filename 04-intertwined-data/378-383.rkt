;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |378|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color
 
; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (square 100 "solid" current))]
    [on-key
     (lambda (current key-event)
       (find transitions current))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; (simulate "green" fsm-traffic)

; ==================== Exercise 378 ====================

(define (simulate-378 state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (overlay/align "center" "center"
                      (text current 24 "black")
                      (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions current))]))

; (simulate-378 "green" fsm-traffic)

; =================== End of exercise ==================

; ==================== Exercise 379 ====================

(check-expect (find '(("input1" "expect1")
                      ("input2" "expect2"))
                    "input1")
              "expect1")
(check-error (find '("input" "output") "input"))

; =================== End of exercise ==================

; ==================== Exercise 380 ====================

; A 1Transition is a list of two items:
;   (cons Key-Event (cons FSM-State '()))

(define fsm-traffic-keys
  '(("1" "green") ("2" "yellow") ("3" "red")))

(define (simulate-380 state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (overlay/align "center" "center"
                      (text current 24 "black")
                      (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions key-event))]))

; (simulate-380 "green" fsm-traffic-keys)

; =================== End of exercise ==================

; <machine initial="red">
;   <action state="red"    next="green" />
;   <action state="green"  next="yellow" />
;   <action state="yellow" next="red" />
; </machine>

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

; An XMachine is a nested list of this shape:
;   `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

; ==================== Exercise 381 ====================

; An XMachine-list is a nested list of this shape:
;   (list 'machine (list (list initial FSM-State)) (list X1))
; An X1T-list is a nested list of this shape:
;   (list action (list (list state FSM-State) (list next FSM-State)))

; An XMachine-cons is a nested list of this shape:
;   (cons 'machine
;         (cons (cons (cons 'initial (cons FSM-State '())) '())
;               (cons (cons X1T '()) '())))
; An X1T-cons is a nested list of this shape:
;   (cons 'action
;         (cons (cons (cons 'state (cons 'FSM-State '())) '())
;               (cons (cons 'next (cons 'FSM-State '())) '())))

; =================== End of exercise ==================

; ==================== Exercise 382 ====================

; <machine initial="white">
;   <action state="white"    next="black" />
;   <action state="black"    next="white" />
; </machine>

(define bw-machine '(machine ((initial "white"))
                             (action ((state "white") (next "black")))
                             (action ((state "black") (next "white")))))

; =================== End of exercise ==================

; ==================== Exercise 383 ====================

; ---------------------------------
; functions from excercises 363-377
; ---------------------------------

; [List-of Attributes] Symbol -> [Maybe String]
; if the attributes list associates the symbol with a string,
; the function retrieves this string
(define (find-attr loa sym)
  (local ((define search-result (assq sym loa)))
    (cond [(false? search-result) #false]
          [else (second search-result)])))

; [List-of Attribute] or Xexpr.v2.1 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; [List-of Attribute] or Xexpr.v2.1 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2.1 -> [List-of [List-of Xexpr.v2.1]]
; retrieves the content (body) of xe
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             (rest optional-loa+content)
             optional-loa+content))])))

; ----------------------------------------
; end of functions from excercises 363-377
; ----------------------------------------

; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))
 
; XMachine -> FSM-State 
; extracts and translates the transition table from xm0
 
(check-expect (xm-state0 xm0) "red")
 
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))
 
; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
 
(check-expect (xm->transitions xm0) fsm-traffic)
 
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

; bw-machine simulation
; (simulate-xmachine bw-machine)

; =================== End of exercise ==================
