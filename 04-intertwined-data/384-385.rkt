;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |384|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

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

(define (get x) ...)

; =================== End of exercise ==================

; ==================== Exercise 385 ====================

; mock a HTTP response for the purpose of an exercise
(read-xexpr "sample.html")

; =================== End of exercise ==================
