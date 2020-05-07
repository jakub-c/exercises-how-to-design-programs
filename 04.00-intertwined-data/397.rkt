;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |397|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ==================== Exercise 397 ====================

(define-struct card [em-num hours])
; Card is a structure:
;  - (make-card Number Number)

(define CARD1 (make-card 001 10))
(define CARD2 (make-card 002 50))
(define CARD3 (make-card 003 1))

(define-struct record [name em-num rate])
; Record is a structure:
;  - (make-record String Number Number)

(define REC1 (make-record "Anders" 002 5))
(define REC2 (make-record "Albert" 001 10))
(define REC3 (make-record "Rabbit" 003 10))


(define-struct wage [name amount])
; Wage is a structure:
;  - (make-wage String Number)

(define WAGE (make-wage "Albert" 100))



; [List-of Record] [List-of Card] -> [List-of Wage]
; produce a list of wage records,
; which contain the name and weekly wage of an employee
; Assumption: There is at most one time card per employee number.
; (define (wages*.v3 lor loc) '("" 0)) ;stub

(check-expect (wages*.v3 (list REC1 REC2) (list CARD1 CARD2))
              `(,(make-wage "Albert" 100)
                ,(make-wage "Anders" 250)))

(check-error (wages*.v3 (list REC1 REC2) (list CARD1)))
(check-error (wages*.v3 (list REC1 REC3) (list CARD1 CARD2)))
(check-error (wages*.v3 (list REC1 REC2) (list CARD1 CARD3)))

(define (wages*.v3 lor loc)
  (local ((define sorted-loc
            (sort loc (lambda (x y) (< (card-em-num x) (card-em-num y)))))
          (define sorted-lor
            (sort lor (lambda (x y) (< (record-em-num x) (record-em-num y)))))
          (define generate-wages
            (for/list ([r sorted-lor] [c sorted-loc])
              (local ((define card-record-em-num-the-same?
                        (= (record-em-num r) (card-em-num c))))
                (if card-record-em-num-the-same?
                    (make-wage (record-name r)
                               (* (card-hours c)
                                  (record-rate r)))
                    (error "employee numbers don't match")))))
          (define lists-equal-lenghts? (= (length lor) (length loc))))
    (if lists-equal-lenghts?
        generate-wages
        (error "Lists have different lengths"))))

; =================== End of exercise ==================
