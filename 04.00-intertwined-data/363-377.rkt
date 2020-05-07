;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 363-376) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require 2htdp/image)

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

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

; example of XEnum.v1
(define e0-enum
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))


; ==================== Exercise 372 ====================

(define BT-372 (circle 2 100 "grey"))

; XItem.v1 -> Image 
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 '(li (word ((text "hello")))))
              (beside/align 'center BT-372 (text "hello" 12 'grey)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'grey)))
    (beside/align 'center BT-372 item)))

; explanation of what does (render-item i) do:
; 1. find the content of a given expression (a word inside <li>)
; 2. find the first occurence of the <word>
; 3. get the actual text from the <word>
; 4. create a image based representation of the found text
; 5. put the text next to the bullet defined by BT

; =================== End of exercise ==================

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; ==================== Exercise 373 ====================

(define SIZE 12) ; font size 
(define COLOR "grey") ; font color 
(define BT ; a graphical constant 
  (beside (circle 3 'solid 'grey) (text " " SIZE COLOR)))
 
; Image -> Image
; marks item with bullet
(check-expect (bulletize (text "Hello" SIZE COLOR))
              (beside/align 'center BT (text "Hello" SIZE COLOR)))

(define (bulletize item)
  (beside/align 'center BT item))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(define input-render-enum-1 '(ul
                              (li (word ((text "hello"))))
                              (li ((display "block"))
                                  (word ((text "world"))))))
(check-expect (render-enum input-render-enum-1)
              (above/align 'left
                           (bulletize (text "hello"  SIZE 'black))
                           (bulletize (text "world"  SIZE 'black))
                           empty-image))

(define input-render-enum-2 '(ul
                              (li (word ((text "hello"))))
                              (li
                               (ul
                                (li ((display "block"))
                                    (word ((text "!"))))))))
(check-expect (render-enum input-render-enum-2)
              (above/align 'left
                           (bulletize (text "hello"  SIZE 'black))
                           (above/align 'left
                                        (bulletize
                                         (bulletize (text "!"  SIZE 'black)))
                                        empty-image)
                           empty-image))


(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item '(li (word ((text "hello")))))
              (bulletize (text "hello"  SIZE 'black)))
(check-expect (render-item '(li
                             (ul
                              (li (word ((text "hello")))))))
              (above/align 'left
                           (bulletize
                            (above/align 'left
                                         (bulletize (text "hello"  SIZE 'black))
                                         empty-image))
                           empty-image))

(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))

; =================== End of exercise ==================

; ==================== Exercise 374 ====================

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; An XItem.cons is one of:
; - (cons 'li (cons Xword '()))
; - (cons 'li (cons (cons Attribute '())
;                   (cons Word '())))
; - (cons 'li (cons XEnum.cons '()))
; - (cons 'li (cons (cons Attribute '())
;                   (cons XEnum.cons '()))

; An XEnum.cons is one of:
; - (cons 'ul (cons XItem.cons '()) '())
; - (cons 'ui (cons (cons Attribute '())
;                   (cons XItem.cons '())))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum-cons input-render-enum-1)
              (above/align 'left
                           (bulletize (text "hello"  SIZE 'black))
                           (bulletize (text "world"  SIZE 'black))
                           empty-image))

(check-expect (render-enum-cons input-render-enum-2)
              (above/align 'left
                           (bulletize (text "hello"  SIZE 'black))
                           (above/align 'left
                                        (bulletize
                                         (bulletize (text "!"  SIZE 'black)))
                                        empty-image)
                           empty-image))


(define (render-enum-cons xe)
  (local ((define content (xexpr-content xe))
          (define (render-content-enum cnt)
            (cond [(empty? cnt) empty-image]
                  [else
                   (above/align 'left
                                (render-item-cons (first cnt))
                                (render-content-enum (rest cnt)))])))
    (render-content-enum content)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item-cons '(li
                                  (ul
                                   (li (word ((text "hello")))))))
              (above/align 'left
                           (bulletize
                            (above/align 'left
                                         (bulletize (text "hello"  SIZE 'black))
                                         empty-image))
                           empty-image))
(check-expect (render-item-cons '(li (word ((text "hello!")))))
              (bulletize (text "hello!"  SIZE 'black)))

(define (render-item-cons an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum-cons content)]))))

; =================== End of exercise ==================

; ==================== Exercise 375 ====================

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item-v.2 '(li (word ((text "hello")))))
              (bulletize (text "hello"  SIZE 'black)))
(check-expect (render-item-v.2 '(li
                                 (ul
                                  (li (word ((text "hello")))))))
              (above/align 'left
                           (bulletize
                            (above/align 'left
                                         (bulletize (text "hello"  SIZE 'black))
                                         empty-image))
                           empty-image))

(define (render-item-v.2 an-item)
  (local ((define content (first (xexpr-content an-item))))
    (cond [(word? content) (bulletize
                            (text (word-text content) SIZE 'black))]
          [else (bulletize (render-enum content))])))

; I like both versions, the one with render-item-v.2 might
; be a bit easier to comprehend though
; It's covered by the same tests as the original function
; so I'm confident it works

; =================== End of exercise ==================

; ==================== Exercise 376 ====================

; XEnum.v2 -> Number
; count all "hello"s in an instance of XEnum.v2
(define input-render-enum-3 '(ul
                              (li (word ((text "hello"))))
                              (li (word ((text "world"))))
                              (li ((display "block"))
                                  (word ((text "hello"))))))
(check-expect (count-hello input-render-enum-3) 2)

; (define (count-hello enum) 0) ;stub

(define (count-hello enum)
  (local ((define content (xexpr-content  input-render-enum-3)))
    (for/sum ([xitem content]) (count-hello-xitem xitem))))

(define (count-hello-xitem i)
  (local ((define xitem-content (first (xexpr-content i))))
    (cond [(word? xitem-content)
           (if (string=? "hello" (word-text xitem-content))
               1
               0)]
          [else (count-hello i)])))

; =================== End of exercise ==================

; ==================== Exercise 377 ====================

; XEnum.v2 -> XEnum.v2
; replace all "hello"s with "bye" in an enumeration
(define input-render-enum-4 '(ul
                              (li (word ((text "hello"))))                              
                              (li (word ((text "world"))))
                              (li ((display "block"))
                                  (word ((text "hello"))))))
(define expect-render-enum-4 '(ul
                               (li (word ((text "bye"))))
                               (li (word ((text "world"))))
                               (li ((display "block"))
                                   (word ((text "bye"))))))
(check-expect (replace-hello input-render-enum-4) expect-render-enum-4)

; (define (replace-hello enum) enum) ;stub
(define (replace-hello enum)
  (local ((define content (xexpr-content enum)))
    (cons (xexpr-name enum)
          (map (lambda (xitem) (replace-in-item xitem)) content))))

; XItem.v2 -> XItem.v2
; (define (replace-in-item i) i) ;stub
(define (replace-in-item i)
  (local ((define xitem-content (first (xexpr-content i))))
    (cond 
      [(word? xitem-content)
         (map (lambda (el)
                (cond [(word? el)
                       (if (string=? "hello" (word-text el))
                           '(word ((text "bye")))
                           el)]
                      [else el]))
              i)])))

; =================== End of exercise ==================
