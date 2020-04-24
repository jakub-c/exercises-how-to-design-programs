;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |391|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 391 ====================

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
; (define (replace-eol-with front end) front) ;stub

(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with (cons 1 '()) '())
              (cons 1 '()))
(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))


;               | (empty? end)         |  (cons? end)
;------------------------------------------------------
;(empty? front) | (and (empty? front)  | (and (empty? front)
;                      (empty? end))   |      (cons? end))
;               |                      |
;(cons? front)  | (and (cons? front)   |  (and (cons? front)
;                      (empty? end))   |       (cons? end))

; implementation based on the table
#;(define (replace-eol-with front end)
    (cond [(and (empty? front)
                (empty? end)) '()]
          [(and (cons? front)
                (empty? end)) front]
          [(and (empty? front)
                (cons? end)) end]
          [(and (cons? front)
                (cons? end)) (append front end)]))

; add or statements
#;(define (replace-eol-with front end)
    (cond [(or (and (empty? front)
                    (empty? end))
               (and (cons? front)
                    (empty? end)))
           front]
          [(and (empty? front)
                (cons? end)) end]
          [(and (cons? front)
                (cons? end)) (append front end)]))

; use de Morgan’s law on the first clause
#;(define (replace-eol-with front end)
    (cond [(and (or (empty? front)
                    (cons? front))
                (empty? end))
           front]
          [(and (empty? front)
                (cons? end)) end]
          [(and (cons? front)
                (cons? end)) (append front end)]))

; (or (empty? front) (cons? front)) is always true so we can skip it
#;(define (replace-eol-with front end)
  (cond [(empty? end) front]
        [(and (empty? front)
              (cons? end)) end]
        [(and (cons? front)
              (cons? end)) (append front end)]))

; first clause filters out all empty 'end's, therefore we can skip
; the check if 'end' is a list: (cons? end)
; it always is because of the first clause
(define (replace-eol-with front end)
  (cond [(empty? end) front]
        [(empty? front) end]
        [(cons? front) (append front end)]))

; the final function is significantly simplified,
; yet it might not be so ebvious as the initial one

; =================== End of exercise ==================
