;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |338|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)
(require 2htdp/abstraction)

(define test-dir (create-dir "20.3"))

(define (how-many dir)
  (local ((define (count-in-dirs lod)
            (cond [(empty? lod) 0]
                  [else (+ (length (dir-files (first lod)))
                           (count-in-dirs (dir-dirs (first lod)))
                           (count-in-dirs (rest lod)))])))
    (+ (count-in-dirs (dir-dirs dir))
       (length (dir-files dir)))))

(how-many test-dir)