;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 10.1-163) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Number -> Number
; convert Farenheit to Celcus temperature

; check-range used for the simplicity of rounding the results
(check-within (f2c -50) -45.6 0.1)
(check-within (f2c 0) -17.7 0.1)
(check-within (f2c 50) 9.9 0.1)

; (define (f2c t) 0) ;stub

(define (f2c t)
   (* (- t 32) (/ 5 9)))

; ListOfMesurements is one of:
;  - '()
;  - (cons Number '())

(define LOM1 '())
(define LOM2 (cons 50 '()))
(define LOM3 (cons 0 (cons 50 '())))

#; (define (fn-for-lom lom)
  (cond
   [(empty? lom) ...]
   [else
    (...
     (first lom)
     (fn-for-lom (rest lom)))]))

; ListOfMesurements -> ListOfMesurements
; convert the list of the Farenheit values to Celcius
(check-expect (convertFC LOM1) '())
(check-within (convertFC LOM2) (cons 9.9 '()) 0.1)
(check-within (convertFC LOM3) (cons -17.7 (cons 9.9 '())) 0.1)

; (define (convertFC lom) '()) ;stub

(define (convertFC lom)
  (cond
   [(empty? lom) '()]
   [else
    (cons
     (f2c (first lom))
     (convertFC (rest lom)))]))