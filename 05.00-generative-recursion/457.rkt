;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |457|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 457 ====================

(define-struct amount [base months])
; An Amount is a structure:
;   (make-table Number [List-of Number])

; Interest Number [List-of-Number] -> Number
; computes how many months it takes to double a given amount of money
; when a savings account pays interest at a fixed rate on a monthly basis
; (define (double-amount amount interest) 1) ;stub

(check-expect (double-amount (make-amount 100 '()) 100) 1)
(check-expect (double-amount (make-amount 100 '()) 50) 2)
(check-expect (double-amount (make-amount 100 '()) 10) 8)

(define (double-amount amount interest)
  (local ((define base (amount-base amount))
          (define months (amount-months amount))
          (define (calc-interest val) (* val (/ interest 100)))
          (define savings-so-far (foldr + base months))
          (define amount-months+new-month (cons (calc-interest savings-so-far)
                                                months))
          (define new-month-savings (foldr + base amount-months+new-month )))
    (if (>= new-month-savings (* 2 base))
        (length amount-months+new-month)
        (double-amount
         (make-amount base amount-months+new-month)
         interest))))

; =================== End of exercise ==================
