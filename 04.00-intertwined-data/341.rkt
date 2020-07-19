;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |341|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)
(require 2htdp/abstraction)

;(define test-dir (create-dir "20.3"))

(define input (make-dir
               '|20.3|
               (list
                (make-dir
                 '20.3/dir1
                 '()
                 (list
                  (make-file ".DS_Store" 6148 (make-date 2020 3 31 23 25 33) ""))))
               (list
                (make-file ".DS_Store" 6148 (make-date 2020 3 31 23 25 18) ""))))

(check-expect (du input) (+ 6148 6148 1 1))

; Dir -> String
; (define (ls dir) "") ;stub

#;(define (du dir)
    (local ((define (count-files dir)
              (for/sum ([f (dir-files dir)])
                (+ (file-size f))))
            (define (count-dirs dir)
              (for/sum [(d (dir-dirs dir))]
                (+ 1
                   (count-files d)
                   (count-dirs d)))))
      (+ 1
         (count-dirs dir)
         (count-files dir))))

(define (du dir)
  (local ((define dir-size 1)
          (define (count-files dir)
            (for/sum ([f (dir-files dir)])
              (+ (file-size f))))
          (define (count-in-dirs dir)
            (+ dir-size
               (count-files dir)
               (for/sum [(d (dir-dirs dir))]
                 (count-in-dirs d)))))
       (count-in-dirs dir)))