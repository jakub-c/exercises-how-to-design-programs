;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |388|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 388 ====================

(define-struct employee (name ssn pay-rate))
; Employe is a structure:
; - (make-employee String Number Number)

(define employee1 (make-employee "Albert" 999 10))
(define employee2 (make-employee "Anders" 222 30))

(define-struct wr (name hours-worked))
; WorkRecord is a structure:
; - (make-wr String Number)

(define wr1 (make-wr "Albert" 10))
(define wr2 (make-wr "Anders" 30))

; [List-of Employee] [List-of WorkRecord] -> [List-of [String Number]]
; produce is a list of structures that contain the name of the employee
; and the weekly wage
; (define (wages*.v2 hours wages/h) '()) ;stub

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list employee1) (list wr1))
              (list (list "Albert" 100)))
(check-expect (wages*.v2 `(,employee1 ,employee2)
                         `(,wr1 ,wr2))
              '(("Albert" 100) ("Anders" 900)))

(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
      (weekly-wage (first hours) (first wages/h))
      (wages*.v2 (rest hours) (rest wages/h)))]))

; Employee WorkRecord -> (cons String (cons Number '()))
; computes the weekly wage from pay-rate and hours
(define (weekly-wage empl wr)
  (local ((define salary (*
                          (employee-pay-rate empl)
                          (wr-hours-worked wr)))
          (define name (employee-name empl)))
    (list name salary)))

; =================== End of exercise ==================

; ==================== Exercise 389 ====================

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; consumes a list of names and a list of phone numbers,
; combines those equally long lists into a list of phone records
; (define (zip lon lop) '()) ;stub

(check-expect (zip '() '()) '())
(check-expect (zip '("Albert") '("223344"))
              `(,(make-phone-record "Albert" "223344")))
(check-expect (zip '("Albert" "Anders") '("223344" "12345"))
              `(,(make-phone-record "Albert" "223344")
                ,(make-phone-record "Anders" "12345")))

(define (zip lon lop)
  (cond [(empty? lon) '()]
        [else
         (cons (make-phone-record (first lon)
                                  (first lop))
               (zip (rest lon) (rest lop)))]))

; =================== End of exercise ==================
