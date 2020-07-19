;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |231|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;evaluate to list
(check-expect '(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))
(check-expect '()
              '())
(check-expect
 '(("alan" 1000)
  ("barb" 2000)
  ("carl" 1500))
 (list
  (list "alan" 1000)
  (list "barb" 2000)
  (list "carl" 1500)))

; evaluate to cons
(check-expect '(1 "a" 2 #false 3 "c")
              (cons 1 (cons "a" (cons 2 (cons #false (cons 3 (cons "c" '())))))))