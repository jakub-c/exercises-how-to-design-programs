;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.2.200) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "12.iTunes Music Library.xml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the 2htdp/itunes library documentation, part 1: 


; (define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

;(define DATE1 (make-date 2019 1 1 0  0 0))
;(define DATE2 (make-date 2019 2 1 10 5 22))
;(define DATE3 (make-date 2019 2 21 10 7 0))
;(define DATE4 (make-date 2019 4 1  20 08 32))

#;(define-struct track
  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played

#;(define TRACK1 (make-track "The Meaning of Life"
                           "Nebula"
                           "Scientific Wax 027"
                           3000
                           2
                           DATE1
                           10
                           DATE2))

#;(define TRACK2 (make-track "Cubic Dub"
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

#;(define LTRACKS (list TRACK1 TRACK2))

; LTracks -> Number
(check-expect (total-time '()) 0)
(check-expect (total-time LTRACKS) 8000)

; (define (total-time ltracks) 0) ;stub

(define (total-time ltracks)
  (cond
    [(empty? ltracks) 0]
    [else
     (+ (track-time (first ltracks))
        (total-time (rest ltracks)))]))

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

; LTracks -> Number
(define (total-time lot) 0) ;stub