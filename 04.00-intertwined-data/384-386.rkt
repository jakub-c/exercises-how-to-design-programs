;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 384-385) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/abstraction)

; ==================== Exercise 384 ====================

;(read-xexpr/web "http://info.cern.ch/hypertext/WWW/TheProject.html")
; result:
(list
 'header
 '()
 "\n"
 (list 'title '() "The World Wide Web project")
 "\n"
 (list 'nextid (list (list 'n "55")) "\n"))

; (read-xexpr f)
; this function seems to do the same as above but with the local file

; (read-plain-xexpr/web "http://info.cern.ch/hypertext/WWW/TheProject.html")
; throws an arror - looks like it requires a properly formatted XML


(define PREFIX "https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
(define example-data (make-data 100 10))
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; create a data struct from a price returned by the webpage
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image 
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    ; compose an image
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; (define (get x) ...)

; =================== End of exercise ==================

; ==================== Exercise 385 ====================

; mock a HTTP response for the purpose of an exercise
(read-xexpr "sample.html")

; =================== End of exercise ==================

; ==================== Exercise 386 ====================


; ---------------------------------
; functions from excercises 363-377
; ---------------------------------

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

; [List-of Attributes] Symbol -> [Maybe String]
; if the attributes list associates the symbol with a string,
; the function retrieves this string
(define (find-attr loa sym)
  (local ((define search-result (assq sym loa)))
    (cond [(false? search-result) #false]
          [else (second search-result)])))

; Xexpr.v2.1 -> [List-of Attribute]
; retrieves the list of attributes of xe
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


; ----------------------------------------
; end of functions from excercises 363-377
; ----------------------------------------

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
#;(check-expect
   (get '(meta ((content "+1") (itemprop "F"))) "F")
   "+1")
#;(check-expect
   (get '(meta ((content "+2") (itemprop "A"))) "A")
   "+2")
#;(check-error
   (get '(meta ((content "+2") (itemprop "A"))) "Z"))

 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

; Xexpr.v3 String -> String
; get the content of the given prop from an Xexpr
(define input-386 (list
                   'html
                   '()
                   (list 'meta (list (list 'content "17.09") (list 'itemprop "price")))
                   (list 'meta (list (list 'content "+0.07") (list 'itemprop "priceChange")))))
(check-expect (get-xexpr
               (list 'meta (list (list 'content "17.09") (list 'itemprop "price")))
               "price")
              "17.09")
(check-expect (get-xexpr input-386 "price") "17.09")
(check-expect (get-xexpr input-386 "priceChange") "+0.07")

; (define (get-xexpr x s) "") ; stub

(define (get-xexpr x s)
  (local ((define content (xexpr-content x))
          (define attributes (second x))
          (define itemprop-found? (string? (find-attr attributes 'itemprop))))
    (if itemprop-found?
        (local ((define itemprop-val (second (assq 'itemprop attributes)))
                (define contentprop-val (second (assq 'content attributes)))
                (define itemprop-value-found? (string=? itemprop-val s)))
          (if itemprop-value-found?
              contentprop-val
              #false))
        (for/or ([el content])
          (get-xexpr el s)))))

; =================== End of exercise ==================
