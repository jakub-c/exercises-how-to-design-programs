;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 363-370) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

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

; Xexpr.v2.1 -> [List-of Attribute]
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

; [List-of Attribute] or Xexpr.v2.1 -> ???
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

; [List-of Attribute] or Xexpr.v2.1 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; ==================== Exercise 366 ====================

; Xexpr.v2.1 -> Symbol
; retrieves the name of xe
; (define (xexpr-name xe) '()) ;stub

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xe) (first xe))

; Xexpr.v2.1 -> [List-of [List-of Xexpr.v2.1]]
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

; =================== End of exercise ==================

; ==================== Exercise 367 ====================

#; (define (xexpr-attr xe)
     (local ((define optional-loa+content (rest xe)))
       (cond
         [(empty? optional-loa+content) ...]
         [else (... (first optional-loa+content)
                    ... (rest optional-loa+content) ...
                    ... (xexpr-attr optional-loa+content) ...)])))

; it doesn't make sense to make a self-reference here becase
; the items in the list have different functions (name, body, attributes)
; and are also of a different type
; - we wouldn't get too much value out of self reference in this case

; =================== End of exercise ==================

; ==================== Exercise 368 ====================

; XexprContent is one of:
;  - (cons [List-of Attribute] '())
;  - (cons '() Xexpr.v2.1)
;  - (cons [List-of Attribute] Xexpr.v.2.1)

; ==================== Exercise 369 ====================

; [List-of Attributes] Symbol -> [Maybe String]
; if the attributes list associates the symbol with a string,
; the function retrieves this string
(define input-369 '((attr1 "a") (attr2 "b")))

(check-expect (find-attr input-369 'attr1) "a")
(check-expect (find-attr input-369 'attr2) "b")
(check-expect (find-attr input-369 'attr3) #false)

; (define (find-attr loa sym) "") ;stub

(define (find-attr loa sym)
  (local ((define search-result (assq sym loa)))
    (cond [(false? search-result) #false]
          [else (second search-result)])))

; =================== End of exercise ==================

; An XWord is '(word ((text String)))

; ==================== Exercise 370 ====================

(define XWord-1 '(word ((text "hello"))))
(define XWord-2 '(word ((text "world"))))
(define XWord-3 '(word ((text "!"))))

; Any -> Boolean
; check whether some ISL+ value is in XWord
(check-expect (word? XWord-1) #true)
(check-expect (word? '(word "abc")) #false)
(check-expect (word? "2") #false)
(check-expect (word? 'symbol) #false)

; (define (word? w) #false) ;stub

#;(define (word? w)
    (match w
      [(list word attr)
       (and (list? attr)
            (equal? 'text (first (first attr)))
            (string? (second (first attr))))]
      [_ #false]))

; we're sure that text is the only attribute of XWord
; so we can refactor the function above to this
; specific pattern matching
(define (word? w)
  (match w
    [(list 'word (list (list 'text (? string?))))
     #true]
    [_ #false]))

; XWord -> [Maybe String]
; extract the value of the only attribute of an instance of XWord
(check-expect (word-text XWord-1) "hello")
(check-expect (word-text '(word)) #false)
(check-expect (word-text "abc") #false)

; (define (word-text w) "abc") ;stub

(define (word-text w)
  (match w
    [(list 'word (list (list 'text text-value)))
     (if (word? w)
         text-value
         #false)]
    [_ false]))

; =================== End of exercise ==================

; ==================== Exercise 371 ====================

; An Xepr.v2.3 is a list:
; - (cons Symbol Xexpr.v2.3)
; - (cons word Xexpr.v2.3)
; - (cons Symbol (cons [List-of Attribute] Xexpr.v2.2))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XWord is '(word ((text String))).

; =================== End of exercise ==================

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

; – (cons 'ul [List-of XItem.v1]) is also an XExpression because
; it matches a pattern of: Symbol + list or Words defined in 371

; – (cons 'ul (cons Attributes [List-of XItem.v1])) is also an XExpression
; becauce it matches the pattern
; (cons Symbol (cons [List-of Attribute] Xexpr.v2.2)) in 371

