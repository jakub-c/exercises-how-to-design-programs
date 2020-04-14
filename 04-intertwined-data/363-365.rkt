;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |363|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; example: <machine />
(define xexpr.v0 '(machine))

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; example: <machine><action /><action /><action /></machine>
(define xepr.v1 '(machine (action) (action) (action)))

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; example: <machine initial="red"></machine>
(define expr.v2-1 '(machine ((initial "red"))))

; example:  <machine initial="red">
;             <action state="red" next="green" />
;           </machine>
(define expr.v2-2 '(machine ((initial "red"))
                            (action ((state "red") (next "green")))))

; ==================== Exercise 363 ====================
; An Xepr.v2.1 is a list:
; - (cons Symbol Xexpr.v2.1)
; - (cons Symbol (cons [List-of Attribute] Xexpr.v2.1))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; =================== End of exercise ==================

; ==================== Exercise 364 ====================

; <transition from="seen-e" to="seen-f" />
; it could be represented with Xexpr.v1;
'(transition ((from "seen-e") ('to "seen-f")))

; <ul><li><word /><word /></li><li><word /></li></ul>
; it could be represented with Xexpr.v0;
'(ul (li (word) (word))
     (li (word)))

; =================== End of exercise ==================

; ==================== Exercise 365 ====================

; Xexpr.v1 (with no body)
;(cons Symbol
;  (cons (cons Symbol (cons String '()) '())))
'(server ((name "example.org")))
; <server name="example.org">

; Xexpr.v1
; (cons Symbol
;   (cons Symbol (cons Symbol '()))
;   (cons Symbol (cons (cons Symbol (cons Strnig '())))))
'(carcas (board (grass)) (player ((name "sam"))))
; <carcas>
;  <board><grass /><board>
;  <player name="sam" />
; <carcas>

; Xexpr.v0
; (cons Symbol '())
'(start)
; <start />

; =================== End of exercise ==================

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
; (define (xexpr-attr xe) '()) ;stub
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

#;(define (xexpr-attr xe)
    (local ((define optional-loa+content (rest xe)))
      (cond
        [(empty? optional-loa+content) ...]
        [else ...]))) ; template-v1

#;(define (xexpr-attr xe)
    (local ((define optional-loa+content (rest xe)))
      (cond
        [(empty? optional-loa+content) ...]
        [else (... (first optional-loa+content)
                   ... (rest optional-loa+content) ...)]))) ; template-v2

; [List-of Attribute] or Xexpr.v2 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
#;(define (list-of-attributes? x)
    #false) ; a wish

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))]))) ; template-v3

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; ==================== Exercise 366 ====================

; Xexpr.v2 -> Symbol
; retrieves the name of xe
; (define (xexpr-name xe) '()) ;stub

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xe) (first xe))

; Xexpr.v2 -> [List-of [List-of Xexpr.v2.1]]
; retrieves the content (body) of xe
; (define (xexpr-content xe) '()) ;stub

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

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