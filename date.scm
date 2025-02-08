#!/usr/bin/env guile
!#
;; date.scm - © Sarthak Shah (matchcase), 2025
;; Licensed under GPLv3

(use-modules (ice-9 match)
             (ice-9 format))

(define dark-color "#0d0e1c")
(define bright-color "#61647a")

(define (blend-colors color-dark color-bright minutes)
  (define (hex->rgb hex)
    (list (string->number (substring hex 1 3) 16)
          (string->number (substring hex 3 5) 16)
          (string->number (substring hex 5 7) 16)))
  (define (rgb->hex rgb)
    (match rgb
      [(r g b)
       (format #f "#~2,'0d~2,'0d~2,'0d"
               (inexact->exact (round r))
               (inexact->exact (round g))
               (inexact->exact (round b)))]
      [_ (error "Wrong format for RGB values!")]))
  (define (interpolate a b t)
    (+ (* (- 1 t) a) (* t b)))
  (let [(rgb-dark (hex->rgb color-dark))
        (rgb-bright (hex->rgb color-bright))
        (t (/ (- 1 (cos (* (/ minutes 1440.0) (acos -1)))) 2))]
    (match `(,rgb-dark ,rgb-bright)
      [((rd gd bd) (rb gb bb))
       (let [(r (interpolate rd rb t))
             (g (interpolate gd gb t))
             (b (interpolate bd bb t))]
         (rgb->hex (list (round r) (round g) (round b))))]
      [_ (error "Wrong format for rgb-dark or rgb-bright!")])))

(define (what-day dow)
  (list-ref '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") dow))

(define (time->emoji minutes)
  (define (random-emoji lst)
    (list-ref lst (random (length lst))))
  (let [(night-emoji '("🌉" "🌃" "🌖" "🌙" "🌝" "🌛" "🌠"))]
    (cond [(< minutes 360) (random-emoji night-emoji)]
          [(< minutes 720) (random-emoji '("🌅" "🌄"))]
          [(< minutes 1080) (random-emoji '("☀️" "🌞"))]
          [(< minutes 1320) (random-emoji '("🌆" "🌇"))]
          [else (random-emoji night-emoji)])))

(match (array->list (localtime (current-time)))
  [(sec min hr day month year dow . _)
   (let [(minutes (+ (* hr 60) min))]
     ;; same emoji every hour :)
     (set! *random-state* (seed->random-state hr))
     (format #t " ~2,'0d ~a ~2,'0d:~2,'0d ~a ~%" day (what-day dow) hr min (time->emoji minutes))
     (newline)
     (newline)
     (format #t "~a~%" (blend-colors dark-color bright-color minutes)))]
  [_ (error "Wrong format for localtime!")])
