;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |343|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)
(require racket/string)	
(require 2htdp/abstraction)

(define test-dir (create-dir "20.3"))

(define input-short (make-dir
                     '|20.3|
                     (list
                      (make-dir
                       '20.3/dir1
                       '()
                       (list
                        (make-file "f1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))
                     '()))

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
                   (list
                    (make-dir
                     '20.3/dir2/dir2.2/dir2.3
                     '()
                     (list (make-file "d2.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))
                   (list
                    (make-file ".DS_Store" 6148 (make-date 2020 4 3 14 1 20) "")
                    (make-file "d1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))
                 (list
                  (make-file ".DS_Store" 6148 (make-date 2020 4 3 14 1 13) "")
                  (make-file "f1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))
               (list
                (make-file ".DS_Store" 6148 (make-date 2020 3 31 23 25 18) "")
                (make-file "a1.rtf" 397 (make-date 2020 3 31 14 34 2) ""))))

; Dir String -> Boolean
; (define (find? dir fname) #f) ;stub

(define (find? dir fname)
  (local (; Dir -> Boolean
          (define (find-files dir)
            (for/or ([f (dir-files dir)])
              (string=? (file-name f) fname))))
    (or (find-files dir)
        (for/or ([d (dir-dirs dir)])
          (find? d fname)))))

; Dir String -> String
; produces a path to a file with name fname; otherwise produce #false

;(define (find d fname) "") ;stub

(define (find dir fname)
  (local ((define found-in-children? (for/or ([d (dir-dirs dir)])
                                       (find d fname))))
    (if (find? dir fname)
        (if (equal? found-in-children? #false)
            (string-append
             (symbol->string (dir-name dir))
             "/"
             fname)
            found-in-children?)
        #false)))

; list the paths to all files contained in a given Dir
; Dir -> [List-of String]

; (define (ls-R dir) '()) ;stub

(define (ls-R dir)
  (local ((define (list-dir dir)
            (for/list ([f (dir-files dir)])
              (find dir (file-name f)))))
    (append (list-dir dir)
          (for/list ([d (dir-dirs dir)])
            (ls-R d)))))