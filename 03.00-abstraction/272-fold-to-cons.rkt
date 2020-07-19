;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 272-fold-to-cons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; clarification of the fold application

(foldl cons '() '(1 2 3))
(cons 3 (cons 2 (cons 1 '())))

(foldr cons '() '(1 2 3))
(cons 1 (cons 2 (cons 3 '())))