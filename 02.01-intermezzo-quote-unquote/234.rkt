;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |234|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))
(define two-list
  '((1 "Asia: Heat of the Moment")
    (2 "U2: One")))

; ListOfStrings -> String
(define (generate-page los)
  (html (ranking los)))

; LoLOR -> String
(check-expect (html two-list)
              "<table><tr><td>1</td><td>Asia: Heat of the Moment</td></tr><tr><td>2</td><td>U2: One</td></tr></table>")
;(define (html lolor) "") ;stub

(define (html lolor)
  (cond
    [(empty? lolor) ""]
    [else
     (string-append
      "<table>"
      (table-body lolor)
      "</table>")]))

; LoLOR -> String
(check-expect (table-body two-list)
              "<tr><td>1</td><td>Asia: Heat of the Moment</td></tr><tr><td>2</td><td>U2: One</td></tr>")
;(define (table-body lolor) "") ;stub
(define (table-body lolor)
  (cond [(empty? lolor) ""]
        [else
         (string-append (table-row (first lolor))
                        (table-body (rest lolor)))]))

; ListOfRanks -> String
(check-expect (table-row LOR1) "<tr><td>1</td><td>U2: One</td></tr>")
; (define (table-row lor) "") ;stub
(define (table-row lor)
  (string-append
   "<tr>"
   "<td>" (number->string (first lor)) "</td>"
   "<td>" (second lor) "</td>"
   "</tr>"))

; ListOfStrings -> LoLOR
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; ListOfStrings -> LoLOR;
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; LoLOR is a List of ListOfRanks
; LoLOR is one of:
;  - '()
;  - (cons ListOfRanks LoLOR)
(define LoLOR1 '(LOR1))

; ListOfRanks is a list:
;   (list Number String)
(define LOR1 '(1 "U2: One"))