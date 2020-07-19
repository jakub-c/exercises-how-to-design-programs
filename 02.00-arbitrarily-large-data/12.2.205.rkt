;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.2.205) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "12.iTunes Music Library.xml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the 2htdp/itunes library documentation, part 1: 


;(define-struct date [year month day hour minute second])
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

(define DATE1 (create-date 2019 1 1 0  0 0))
(define DATE2 (create-date 2019 2 1 10 5 22))
(define DATE3 (create-date 2019 2 21 10 7 0))
(define DATE4 (create-date 2019 4 1  20 08 32))
(define DATE5 (create-date 2009 4 1  20 08 32)) ;1238616512
(define DATE6 (create-date 2019 10 10  20 08 32)) ;1570738112000

#;(define-struct track
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

(define TRACK1 (create-track "The Meaning of Life"
                             "Nebula"
                             "Scientific Wax 027"
                             3000
                             2
                             DATE1
                             10
                             DATE2))

(define TRACK2 (create-track "Cubic Dub"
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
; produces the total amount of play time of tracks
(check-expect (total-time '()) 0)
(check-expect (total-time LTRACKS) 8000)

; (define (total-time ltracks) 0) ;stub

(define (total-time ltracks)
  (cond
    [(empty? ltracks) 0]
    [else
     (+ (track-time (first ltracks))
        (total-time (rest ltracks)))]))

; LTracks -> List-of-Strings
; produces the list of album titles as a List-of-strings
(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles LTRACKS) (list "Scientific Wax 027" "Slow Living vol.3"))

; (define (select-all-album-titles lot) '()) ;stub

(define (select-all-album-titles lot)
  (cond
    [(empty? lot) '()]
    [else
     (cons (track-album (first lot))
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
; produces a list of unique album titles
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

; String LTracks -> LTracks
; extracts from LTracks the list of tracks that belong to the given album
(check-expect (select-album "" '()) '())
(check-expect (select-album "Scientific Wax 027" LTRACKS) (list TRACK1))

;(define (select-album s lot) '()) ;stub

(define (select-album title lot)
  (cond
    [(empty? lot) '()]
    [else
     (if (string=? title (track-album (first lot)))
          (cons (first lot) (select-album title (rest lot)))
          (select-album title (rest lot)))]))

; String Date LTracks -> LTracks
; extracts from the latter the list of tracks that belong to the given album
; and have been played after the given date
(check-expect (select-album-date "" DATE5 '()) '())
(check-expect (select-album-date "Scientific Wax 027" DATE5 LTRACKS) (list TRACK1))
(check-expect (select-album-date "Scientific Wax 027" DATE6 LTRACKS) '())


;(define (select-album-date title date lot) '()) ;stub

(define (select-album-date title date lot)
  (select-tracks-played-after date
                              (select-album title lot)))

; Date LTracks -> LTracks
(check-expect (select-tracks-played-after DATE5 '()) '())
(check-expect (select-tracks-played-after DATE5 LTRACKS) LTRACKS)
(check-expect (select-tracks-played-after DATE6 LTRACKS) '())

; (define (select-stracks-played-after date lot) '()) ;stub
; select all tracks played after a given year
; year was chosen to make the date comparison faster and simpler
(define (select-tracks-played-after date lot)
  (cond
    [(empty? lot) '()]
    [else
     (if (date-year<? date (track-played (first lot)))
         (cons (first lot) (select-tracks-played-after date (rest lot)))
         (select-tracks-played-after date (rest lot)))]))

; Date Date -> Boolean
(check-expect (date-year<? DATE5 DATE6) true)
(check-expect (date-year<? DATE6 DATE5) false)

; (define (date<? date1 date2) false) ;stub
(define (date-year<? date1 date2)
  (< (date-year date1) (date-year date2)))

; LTracks -> List-of-LTracks
; produces a list of LTracks, one per album
; Each album is uniquely identified by its title and shows up in the result only once
(check-expect (select-albums '()) '())
(check-expect (select-albums LTRACKS) (list (list TRACK1) (list TRACK2)))
(check-expect (select-albums LTRACKS2) (list (list TRACK1 TRACK1) (list TRACK2 TRACK2)))

; (define (select-albums lot) (list '())) ;stub
(define (select-albums lot)
  (cond
    [(empty? lot) '()]
    [else
     (select-by-lofalbums (select-album-titles/unique lot) lot)]))

; List-of-strings LTracks -> List-of-LTracks
(check-expect (select-by-lofalbums '() '()) '())
(check-expect (select-by-lofalbums (list "Scientific Wax 027" "Slow Living vol.3") LTRACKS2)
              (list (list TRACK1 TRACK1) (list TRACK2 TRACK2)))

; (define (select-by-lofalbums los lot) '()) ;stub
; produce the list of tracks that belog to the given album
; based on the provided list of albums
(define (select-by-lofalbums los lot)
  (cond
    [(empty? los) '()]
    [(empty? lot) '()]
    [else
     (cons
      (select-album (first los) lot)
      (select-by-lofalbums (rest los) lot))]))

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
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)

(define LASSOC1 (list "Track ID" 86))
(define LASSOC2 (list "Genre" "Rock"))
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)

(define LLISTS1 (list LASSOC1 LASSOC2))
 
; String -> LLists
; creates a list of lists representation for all tracks in 
; file-name, which must be an XML export from iTunes 
#;(define (read-itunes-as-lists file-name)
  ...)