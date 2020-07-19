;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |276|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


; String Date LTracks -> LTracks
; extract from LTracks the list of tracks from the given album that have been played after the date
(check-expect (select-album-date "" DATE5 '()) '())
(check-expect (select-album-date "Scientific Wax 027" DATE5 LTRACKS) (list TRACK1))
(check-expect (select-album-date "Scientific Wax 027" DATE6 LTRACKS) '())

(define (select-album-date album date ltracks)
  (local (; Track -> Boolean
          (define (matches-album-name? track)
            (string=? (track-album track)
                      album))
          ; Track -> Boolean
          (define (track-played-after? track)
            (date-year<? date
                         (track-played track)))
          (define albums-played-after
            (filter track-played-after? ltracks)))
    (filter matches-album-name? albums-played-after)))
                      


; Date Date -> Boolean
(check-expect (date-year<? DATE5 DATE6) true)
(check-expect (date-year<? DATE6 DATE5) false)

; (define (date<? date1 date2) false) ;stub
(define (date-year<? date1 date2)
  (< (date-year date1) (date-year date2)))

; LTracks -> List-of-LTracks
; produces a list of LTracks, one per album.
; Each album is uniquely identified by its title and shows up in the result only once.
(check-expect (select-albums '()) '())
(check-expect (select-albums LTRACKS) (list (list TRACK1) (list TRACK2)))
(check-expect (select-albums LTRACKS2) (list (list TRACK1 TRACK1) (list TRACK2 TRACK2)))

(define (select-albums ltracks)
  (local (; LTrack List-of String -> List-of String
          (define (add-unique track loa)
            (if (member? (track-album track) loa)
                loa
                (append loa (list (track-album track)))))
          ;-----
          (define unique-album-names (foldl add-unique '() ltracks))
          ; String LTracks -> LTracks
          (define (select-by-album albumname)
            (local (; Track -> Boolean
                    (define (is-album-name? track)
                      (string=? (track-album track)
                                albumname)))
              (filter is-album-name? ltracks))))
    (map select-by-album unique-album-names)))
