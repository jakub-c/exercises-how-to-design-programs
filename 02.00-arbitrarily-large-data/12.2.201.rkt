;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.2.201) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "12.iTunes Music Library.xml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the 2htdp/itunes library documentation, part 1: 


(define-struct dateit [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; it suffix stands for ITunes - I couldn't reference the
; date structure for some reason
;
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

(define DATE1 (make-dateit 2019 1 1 0  0 0))
(define DATE2 (make-dateit 2019 2 1 10 5 22))
(define DATE3 (make-dateit 2019 2 21 10 7 0))
(define DATE4 (make-dateit 2019 4 1  20 08 32))

(define-struct trackit
  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; it suffix stands for ITunes - I couldn't reference the
; track structure for some reason
;
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played

(define TRACK1 (make-trackit "The Meaning of Life"
                             "Nebula"
                             "Scientific Wax 027"
                             3000
                             2
                             DATE1
                             10
                             DATE2))

(define TRACK2 (make-trackit "Cubic Dub"
                             "B Toriyama"
                             "Slow Living vol.3"
                             5000
                             1
                             DATE3
                             10
                             DATE4))

; An LTracks is one of:
; – '()
; – (cons Track LTracks)

(define LTRACKS (list TRACK1 TRACK2))
(define LTRACKS2 (list TRACK1 TRACK2 TRACK2 TRACK1))

; LTracks -> Number
(check-expect (total-time '()) 0)
(check-expect (total-time LTRACKS) 8000)

; (define (total-time ltracks) 0) ;stub

(define (total-time ltracks)
  (cond
    [(empty? ltracks) 0]
    [else
     (+ (trackit-time (first ltracks))
        (total-time (rest ltracks)))]))

; LTracks -> List-of-Strings
(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles LTRACKS) (list "Scientific Wax 027" "Slow Living vol.3"))

; (define (select-all-album-titles lot) '()) ;stub

(define (select-all-album-titles lot)
  (cond
    [(empty? lot) '()]
    [else
     (cons (trackit-album (first lot))
           (select-all-album-titles (rest lot)))]))

; List-of-strings -> List-of-strings
(check-expect (create-set '()) '())
(check-expect (create-set (list "a")) (list "a"))
(check-expect (create-set (list "a" "a")) (list "a"))
(check-expect (create-set (list "a" "b" "a" "b")) (list "a" "b"))
(check-expect (create-set (list "c" "a" "b" "a" "b")) (list "c" "a" "b"))

; (define (create-set los) '()) ;stub

(define (create-set los)
  (cond
    [(empty? los) '()]
    [else
     (cons
      (first los)
      (create-set (remove-value (first los) (rest los))))]))

; String -> List-of-strings
(check-expect (remove-value "a" '()) '())
(check-expect (remove-value "a" (list "a")) '())
(check-expect (remove-value "a" (list "a" "a")) '())
(check-expect (remove-value "a" (list "a" "b" "a")) (list "b"))
(check-expect (remove-value "a" (list "a" "b" "a" "b")) (list "b" "b"))
(check-expect (remove-value "c" (list "c" "b" "a" "b")) (list "b" "a" "b"))

; (define (remove-value s los) '()) ;stub
(define (remove-value s los)
  (cond
    [(empty? los) '()]
    [else
     (if (string=? s (first los))
         (remove-value s (rest los))
         (cons (first los) (remove-value s (rest los))))]))

; LTracks -> List-of-strings
(check-expect (select-album-titles/unique '()) '())
(check-expect (select-album-titles/unique LTRACKS2)
              (list "Scientific Wax 027" "Slow Living vol.3"))

; (define (select-album-titles/unique lot) '()) ;stub

(define (select-album-titles/unique lot)
  (cond
    [(empty? lot) '()]
    [else
     (create-set
      (select-all-album-titles lot))]))

; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track for legitimate inputs
; otherwise it produces #false
#;(define (create-track name artist album time
                        track# added play# played)
    ...)
 
; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date for legitimate inputs 
; otherwise it produces #false
#;(define (create-date y mo day h m s)
    ...)
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define (get-track-index i)
  (track-name (list-ref itunes-tracks i)))