;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |340|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)
(require 2htdp/abstraction)

(define test-dir (create-dir "20.3"))

(define input (make-dir
               '|20.3|
               (list
                (make-dir
                 '20.3/dir1
                 '()
                 (list
                  (make-file ".DS_Store" 6148 (make-date 2020 3 31 23 25 33) "")
                  (make-file "f1.rtf" 397 (make-date 2020 3 31 14 34 2) "")
                  (make-file "f2.rtf" 397 (make-date 2020 3 31 14 34 2) "")
                  (make-file "f3.rtf" 397 (make-date 2020 3 31 14 34 2) "")))
                (make-dir
                 '20.3/dir2
                 (list
                  (make-dir
                   '20.3/dir2/dir2.2
                   '()
                   (list (make-file "d1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))
                 (list
                  (make-file ".DS_Store" 6148 (make-date 2020 3 31 23 25 25) "")
                  (make-file "c1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))
               (list
                (make-file ".DS_Store" 6148 (make-date 2020 3 31 23 25 18) "")
                (make-file "a1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))

(define expect (list
                (list '20.3/dir1 ".DS_Store")
                (list '20.3/dir1 "f1.rtf")
                (list '20.3/dir1 "f2.rtf")
                (list '20.3/dir1 "f3.rtf")
                (list '20.3/dir2 ".DS_Store")
                (list '20.3/dir2 "c1.rtf")
                (list '20.3/dir2/dir2.2 "d1.rtf")))

(check-expect (ls input) expect)


; Dir -> String
; (define (ls dir) "") ;stub

(define (ls input-dir)
  (local ((define (list-files dir)
            (foldr (lambda (v l)
                     (cons
                      (cons (dir-name dir) (list (file-name v)))
                      l))
                   '()
                   (dir-files dir)))
          (define (list-dirs dir)
            (foldr (lambda (v l)
                     (append
                      (list-files v)
                      (list-dirs v)
                      l))
                   '()
                   (dir-dirs dir))))
    (list-dirs input-dir)))