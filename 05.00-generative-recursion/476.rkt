;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |476|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 476 ====================

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
; (define (fsm-match? an-fsm a-string) #false) ;stub

(check-expect (fsm-match? fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "da") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)

(define (fsm-match? an-fsm a-string)
  (local ((define initial (fsm-initial an-fsm))
          (define transitions (fsm-transitions an-fsm))
          (define current-available-transitions
            (filter
             (lambda (el) (string=? (transition-current el)
                                    initial))
             transitions))
          (define selected-transition
            (filter
             (lambda (el) (string=? (transition-key el)
                                    (first (explode a-string))))
             current-available-transitions)))
    (cond [(empty? selected-transition) #false]
          [(string=? (transition-next (first selected-transition))
                     (fsm-final an-fsm)) #true]
          [else
           (local ((define rest-of-a-string
                     (implode (rest (explode a-string)))))
             (fsm-match?
              (make-fsm
               (transition-next (first selected-transition))
               transitions
               (fsm-final an-fsm))
              rest-of-a-string))])))

; =================== End of exercise ==================
