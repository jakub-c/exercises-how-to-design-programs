;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |283|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir (name price))
(define th 11)

(map (lambda (x) (* 10 x))
     '(1 2 3))
 
(foldl (lambda (name rst)
         (string-append name ", " rst))
       "etc."
       '("Matthew" "Robby"))
 
(filter (lambda (ir) (<= (ir-price ir) th))
        (list (make-ir "bear" 10)
              (make-ir "doll" 33)))