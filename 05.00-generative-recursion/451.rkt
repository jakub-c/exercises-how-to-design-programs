;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |451|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 3 (lambda (i) i)))
 
; N -> Number
(define (a2 i)
  (if (= i 0)
      pi
      (error "table2 is not defined for i =!= 0")))
 
(define table2 (make-table 1 a2))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))


; A table t is monotonically increasing
; if (table-ref t 0) is less than (table-ref t 1),
; (table-ref t 1) is less than (table-ref t 2), and so on

; (table-ref t 0) < (table-ref t 1) < (table-ref t 2)

(define table-1024 (make-table 1024 (lambda (i) (- i 1023))))

; ==================== Exercise 451 ====================

; Q: How many calls to find are needed in
;    find-linear and find-binary, respectively?

; A: find-linear need 1023 steps
;    find-binary needs 10 steps (2^10 = 1024)

(define EPSILON 0.1)

; Table -> Number
; find the smallest index for a root of the table
; ; assume an input is a monotonically increasing table
; (define (find-linear t) 1) ;stub

(check-error (find-linear table2))
(check-expect (find-linear table-1024) 1023)

(define (find-linear t)
  (local ((define (find t search-index)
            (cond [(>= search-index (table-length t))
                   (error "root not found")]
                  [(= (table-ref t search-index) 0)
                   search-index]
                  [else (find t (add1 search-index))])))
    (find t 0)))

; Table -> Number
; find the smallest index for a root of the table
; ; assume an input is a monotonically increasing table

; (define (find-binary t) 0) ;stub

(check-error (find-binary table2))
(check-expect (find-binary table-1024) 1023)
(check-expect (find-binary (make-table 1 (lambda (x) 0)))
              0)


(define (find-binary t)
  (local ((define (find t l-index r-index)
            (local ((define half-of-left-right-distance (floor (/ (- r-index l-index) 2)))
                    (define mid-index (+ l-index half-of-left-right-distance))
                    (define mid-val (table-ref t mid-index)))
              (cond [(= l-index r-index) (error "root not found")]
                    [(and (>= mid-val 0) (<= mid-val EPSILON)) mid-index]
                    [else (if (> mid-val 0)
                              (find t l-index mid-index)
                              (find t mid-index r-index))]))))
    (find t 0 (table-length t))))

; =================== End of exercise =================
