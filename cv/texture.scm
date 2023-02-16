;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2023
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Guile-CV.

;;;; GNU Guile-CV is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; GNU Guile-CV is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Guile-CV.  If not, see
;;;; <https://www.gnu.org/licenses/gpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (cv texture)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv transform)
  #:use-module (cv process)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-texture
            im-glcp
            im-glcm))


;;;
;;; Texture
;;;

(define* (im-texture image n-gl #:key
                     (dist 1) (p-max 255) (use-log2 #f) (no-px-y0 #f))
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (let* ((glcp (im-glcp image n-gl #:dist dist #:p-max p-max))
               (pxys (pxy glcp))
               (sum-avgs (sum-average pxys n-gl)))
          (list
           ;; 1. uniformity
           (/ (reduce + 0 (im-reduce (im-times glcp glcp) + 0)) 4)
           ;; 2. contrast
           (/ (reduce + 0 (contrast glcp)) 4)
           ;; 3. correlaton
           (/ (reduce + 0 (correlation glcp)) 4)
           ;; 4. variance
           (/ (reduce + 0 (variance glcp)) 4)
           ;; 5. homogeneity
           (/ (reduce + 0 (homogeneity glcp)) 4)
           ;; 6. sum average
           (/ (reduce + 0 sum-avgs) 4)
           ;; 7. sum variance
           (/ (reduce + 0 (sum-variance pxys n-gl sum-avgs)) 4)
           ;; 8. sum entropy
           (/ (reduce + 0 (sum-entropy pxys n-gl #:use-log2 use-log2)) 4)
           ;; 9. entropy
           (/ (reduce + 0 (entropy glcp #:use-log2 use-log2)) 4)
           ;; 10. diff variance
           (/ (reduce + 0 (diff-variance pxys n-gl #:no-px-y0 no-px-y0)) 4)
           ;; 11. diff entropy
           (/ (reduce + 0 (diff-entropy pxys n-gl #:use-log2 use-log2)) 4))))
       (else
        (error "Not a GRAY image."))))))

(define* (im-glcp image n-gl #:key (dist 1) (p-max 255))
  (let* ((glcm (im-glcm image n-gl #:dist dist #:p-max p-max))
         (glcp (im-add glcm (im-transpose glcm))))
    (match glcp
      ((width height n-chan idata)
       (list width height n-chan
             (map (lambda (pr)
                    (match pr
                      ((p r)
                       (im-divide-channel p width height r))))
               (zip idata
                    (im-reduce glcp + 0))))))))

(define* (im-glcm image n-gl #:key (dist 1) (p-max 255))
  (if (im-gray? image)
      (let* ((g-n-cell (* n-gl n-gl))
             (g0 (make-s32vector g-n-cell 0))
             (g45 (make-s32vector g-n-cell 0))
             (g90 (make-s32vector g-n-cell 0))
             (g135 (make-s32vector g-n-cell 0))
             (image (if (<= p-max n-gl)
                        image
                        (im-scale image (- n-gl 1) #:p-max p-max))))
        (match image
          ((width height n-chan idata)
           (match idata
             ((c)
              (glcm (f32vector->s32vector c)
                    width height g0 g45 g90 g135 n-gl dist)
              (list n-gl n-gl 4
                    (list (s32vector->f32vector g0)
                          (s32vector->f32vector g45)
                          (s32vector->f32vector g90)
                          (s32vector->f32vector g135))))))))
      (error "Not a GRAY image, n-chan is: " (im-n-channel image))))


;;;
;;; None API and support proc for the above
;;;

(define (contrast glcp)
  (match glcp
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (channel)
                   (contrast-channel channel width height))
           idata)))))

(define (contrast-channel channel width height)
  ;; Sum P(i,j) (i - j) ^2
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i height) result)
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (set! result
              (+ result
                 (* (im-fast-channel-ref channel i j width)
                    (expt (- i j) 2))))))))

(define (correlation glcp)
  (let* ((mu (mu glcp))
         (sigma (sigma glcp mu)))
    (match glcp
      ((width height n-chan idata)
       (let ((map-proc (if (and (> n-chan 1)
                                (%use-par-map)) par-map map)))
         (map-proc (lambda (chamusi)
                     (match chamusi
                       ((channel mu sigma)
                        (correlation-channel channel width height mu sigma))))
             (zip idata mu sigma)))))))

(define (correlation-channel channel width height mu sigma)
  (let ((n (* mu mu))
        (d (sqrt (* sigma sigma)))
        (result 0))
    (do ((i 0
            (+ i 1)))
        ((= i height)
         (/ (- result n) d))
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (set! result
              (+ result
                 (* (+ i 1)
                    (+ j 1)
                    (im-fast-channel-ref channel i j width))))))))

(define (variance glcp)
  (let* ((mu (mu glcp)))
    (match glcp
      ((width height n-chan idata)
       (let ((map-proc (if (and (> n-chan 1)
                                (%use-par-map)) par-map map)))
         (map-proc (lambda (chamu)
                     (match chamu
                       ((channel mu)
                        (variance-channel channel width height mu))))
             (zip idata mu)))))))

(define (variance-channel channel width height mu)
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i height) result)
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (set! result
              (+ result
                 (* (im-fast-channel-ref channel i j width)
                    (expt (- (+ i 1) mu) 2))))))))

(define (homogeneity glcp)
  (match glcp
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (channel)
                   (homogeneity-channel channel width height))
           idata)))))

(define (homogeneity-channel channel width height)
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i height) result)
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (set! result
              (+ result
                 (/ (im-fast-channel-ref channel i j width)
                    (+ 1 (expt (- (+ i 1) (+ j 1)) 2)))))))))

(define (sum-average pxys n-gl)
  (let ((n-cell (- (* 2 n-gl) 1))
        (map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (pxy)
                (match pxy
                  ((px+y px-y)
                   (sum-average-channel px+y n-cell))))
        pxys)))

(define (sum-average-channel px+y n-cell)
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell) result)
      (set! result
            (+ result
               (* (+ i 2) (f32vector-ref px+y i)))))))

(define (sum-variance pxys n-gl sum-avgs)
  (let ((n-cell (- (* 2 n-gl) 1))
        (map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (pxysavg)
                (match pxysavg
                  ((pxy sum-avg)
                   (match pxy
                     ((px+y px-y)
                      (sum-variance-channel px+y n-cell sum-avg))))))
        (zip pxys sum-avgs))))

(define (sum-variance-channel px+y n-cell sum-avg)
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell) result)
      (set! result
            (+ result
               (* (expt (- (+ i 2) sum-avg) 2) (f32vector-ref px+y i)))))))

(define* (sum-entropy pxys n-gl #:key (use-log2 #f))
  (let ((n-cell (- (* 2 n-gl) 1))
        (map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (pxy)
                (match pxy
                  ((px+y px-y)
                   (sum-entropy-channel px+y n-cell #:use-log2 use-log2))))
        pxys)))

(define* (sum-entropy-channel px+y n-cell #:key (use-log2 #f))
  (let ((result 0)
        (e 1.0e-4)
        (log_ (if use-log2 log2 log)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell) (- result))
      (let ((px+yi (f32vector-ref px+y i))) 
        (set! result
              (+ result (* px+yi (log_ (+ px+yi e)))))))))

(define* (entropy glcp #:key (use-log2 #f))
  (match glcp
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (channel)
                   (entropy-channel channel width height #:use-log2 use-log2))
           idata)))))

(define* (entropy-channel channel width height #:key (use-log2 #f))
  (let ((result 0)
        (e 1.0e-4)
         (log_ (if use-log2 log2 log)))
    (do ((i 0
            (+ i 1)))
        ((= i height) (- result))
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (let ((pij (im-fast-channel-ref channel i j width)))
          (set! result
                (+ result
                   (* pij (log_ (+ pij e))))))))))

(define* (diff-average pxys n-gl #:key (no-px-y0 #f))
  (let ((n-cell n-gl)
        (map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (pxy)
                (match pxy
                  ((px+y px-y)
                   (diff-average-channel px-y n-cell #:no-px-y0 no-px-y0))))
        pxys)))

(define* (diff-average-channel px-y n-cell #:key (no-px-y0 #f))
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell) result)
      (set! result
            (+ result
               (* (if no-px-y0 (+ i 1) i)
                  (f32vector-ref px-y i)))))))

(define* (diff-variance pxys n-gl #:key (no-px-y0 #f))
  (let ((n-cell n-gl)
        (map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (pxysavg)
                (match pxysavg
                  ((pxy diff-avg)
                   (match pxy
                     ((px+y px-y)
                      (diff-variance-channel px-y n-cell diff-avg))))))
        (zip pxys
             (diff-average pxys n-gl #:no-px-y0 no-px-y0)))))

(define (diff-variance-channel px-y n-cell diff-avg)
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell) result)
      (set! result
            (+ result
               (* (expt (- (+ i 1) diff-avg) 2) (f32vector-ref px-y i)))))))

(define* (diff-entropy pxys n-gl #:key (use-log2 #f))
  (let ((n-cell n-gl)
        (map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (pxy)
                (match pxy
                  ((px+y px-y)
                   (diff-entropy-channel px-y n-cell #:use-log2 use-log2))))
        pxys)))

(define* (diff-entropy-channel px-y n-cell #:key (use-log2 #f))
  (let ((result 0)
        (e 1.0e-4)
        (log_ (if use-log2 log2 log)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell) (- result))
      (let ((px-yi (f32vector-ref px-y i))) 
        (set! result
              (+ result (* px-yi (log_ (+ px-yi e)))))))))

(define* (mu glcp #:key (dir 'row))
  (match glcp
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (channel)
                   (mu-channel channel width height #:dir dir))
           idata)))))

(define* (mu-channel channel width height #:key (dir 'row))
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i height) result)
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (set! result
              (+ result
                 (* (case dir
                      ((row) (+ i 1))
                      ((col) (+ j 1))
                      (else
                       (error "No such direction: " dir)))
                    (im-fast-channel-ref channel i j width))))))))

(define* (sigma glcp mu #:key (dir 'row))
  (match glcp
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (chamu)
                   (match chamu
                     ((channel mu)
                      (sigma-channel channel width height mu #:dir dir))))
           (zip idata mu))))))

(define* (sigma-channel channel width height mu #:key (dir 'row))
  (let ((result 0))
    (do ((i 0
            (+ i 1)))
        ((= i height) result)
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (set! result
              (+ result
                 (* (case dir
                      ((row) (expt (- (+ i 1) mu) 2))
                      ((col) (expt (- (+ j 1) mu) 2))
                      (else
                       (error "No such direction: " dir)))
                    (im-fast-channel-ref channel i j width))))))))

(define (pxy glcp)
  (match glcp
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (channel)
                   (pxy-channel channel width height))
           idata)))))

(define (pxy-channel channel width height)
  (let ((px+y (make-f32vector (- ( * 2 width) 1) 0.0))
        (px-y (make-f32vector width 0.0)))
    (do ((i 0
            (+ i 1)))
        ((= i height) (list px+y px-y))
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (let ((k (+ i j))
              (l (abs (- i j)))
              (pij (im-fast-channel-ref channel i j width)))
          (f32vector-set! px+y k (+ (f32vector-ref px+y k) pij))
          (f32vector-set! px-y l (+ (f32vector-ref px-y l) pij)))))))

(define* (sum image #:key (dim 'row))
  (match image
    ((width height n-chan idata)
     (let ((map-proc (if (and (> n-chan 1)
                              (%use-par-map)) par-map map)))
       (map-proc (lambda (channel)
                   (sum-channel channel width height #:dim dim))
           idata)))))

(define* (sum-channel channel width height #:key (dim 'row))
  (case dim
    ((row)
     (sum-channel-row channel width height))
    ((col)
     (sum-channel-col channel width height))
    (else
     (error "No such dimension:" dim))))

(define (sum-channel-row channel width height)
  (let ((to (make-f32vector width 0.0)))
    (do ((j 0
            (+ j 1)))
        ((= j width))
      (do ((i 0
              (+ i 1)))
          ((= i height))
        (f32vector-set! to j
                        (+ (f32vector-ref to j)
                           (im-fast-channel-ref channel i j width)))))
    to))

(define (sum-channel-col channel width height)
  (let ((to (make-f32vector height 0.0)))
    (do ((i 0
            (+ i 1)))
        ((= i height))
      (do ((j 0
              (+ j 1)))
          ((= j width))
        (f32vector-set! to i
                        (+ (f32vector-ref to i)
                           (im-fast-channel-ref channel i j width)))))
    to))

;; later -> add.scm
;; once I'm sure of the api, neither if I will keep the name
(define* (im-scale image n-max #:key (p-max 255))
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (list width height n-chan
              (list (im-scale-channel c width height n-max #:p-max p-max))))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (list width height n-chan
                (map-proc (lambda (channel)
                            (im-scale-channel channel width height n-max #:p-max p-max))
                    idata))))))))

(define* (im-scale-channel channel width height n-max #:key (p-max 255))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (f32vector-scale channel n-max to #:n-cell n-cell #:p-max p-max)))

(define (log2 x)
  #;(/ (log x) (log 2))
  (* (log x) 1.4426950408889634))


#!

;; pure scheme
scheme@(guile-user)> ,time (im-glcp $5 16 #:p-max 255)
$7 = (16 16 4 (#f32(1.5557915321551263e-4 9.873292583506554e-5 # # # 0.0 0.0 …) …))
;; 0.913465s real time, 0.934684s run time.  0.126345s spent in GC.

;; with glcm in C
scheme@(guile-user)> ,time (im-glcp $2 16 #:p-max 255)
$3 = (16 16 4 (#f32(1.5557915321551263e-4 9.873292583506554e-5 # # # 0.0 0.0 …) …))
;; 0.197151s real time, 0.265552s run time.  0.103699s spent in GC.

;; with both glcm and im-scale in C
scheme@(guile-user)> ,time (im-glcp $2 16 #:p-max 255)
$4 = (16 16 4 (#f32(0.004733196925371885 0.0015842147404327989 # # # # 0.0 # …) …))
;; 0.104758s real time, 0.121116s run time.  0.024648s spent in GC.

;; with the above and f32vector->s32vector in C
scheme@(guile-user)> ,time (im-glcp $2 16 #:p-max 255)
$6 = (16 16 4 (#f32(1.5557915321551263e-4 9.873292583506554e-5 # # # 0.0 0.0 …) …))
;; 0.039153s real time, 0.040082s run time.  0.000000s spent in GC.

!#

;; Below, the 'original functinality, pure scheme. I moved som of the
;; code below to libguile-cv, which makes im-glcm 100x faster. I keep
;; the pure scheme code though, so when we have an AOT compiler I can
;; try, cmpare and maybe reactive it.

#;(define (s32-ref channel i j width)
  (s32vector-ref channel
                 (im-fast-channel-offset i j width)))

#;(define (s32-set! channel i j width val)
  (s32vector-set! channel
                  (im-fast-channel-offset i j width)
                  val))

#;(define* (im-glcm image n-gl #:key (dist 1) (p-max #f))
  (if (im-gray? image)
      (let* ((g-n-cell (* n-gl n-gl))
             (g0 (make-s32vector g-n-cell 0))
             (g45 (make-s32vector g-n-cell 0))
             (g90 (make-s32vector g-n-cell 0))
             (g135 (make-s32vector g-n-cell 0))
             (p-max (or p-max (im-max image)))
             (image (if (< p-max n-gl)
                        image
                        (im-map (lambda (p-val)
                                  (round (* (/ p-val p-max) (- n-gl 1))))
                            image))))
        (match image
          ((width height n-chan idata)
           (match idata
             ((c)
              (let ((s32c (f32vector->s32vector c)))
                (do ((i 0
                        (+ i 1)))
                    ((= i height))
                  (do ((j 0
                          (+ j 1)))
                      ((= j width))
                    (let ((row (s32-ref s32c i j width)))
                      (if (< j (- width dist))
                          ;; g0
                          (let ((col-g0 (s32-ref s32c i (+ j dist) width)))
                            (s32-set! g0 row col-g0 n-gl
                                      (+ (s32-ref g0 row col-g0 n-gl) 1))
                            (if (> i (- dist 1))
                                ;; g45
                                (let ((col-g45 (s32-ref s32c (- i dist) (+ j dist) width)))
                                  (s32-set! g45 row col-g45 n-gl
                                            (+ (s32-ref g45 row col-g45 n-gl) 1))))))
                      (if (> i (- dist 1))
                          ;; g90
                          (let ((col-g90 (s32-ref s32c (- i dist) j width)))
                            (s32-set! g90 row col-g90 n-gl
                                      (+ (s32-ref g90 row col-g90 n-gl) 1))
                            (if (> j (- dist 1))
                                ;; g135
                                (let ((col-g135 (s32-ref s32c (- i dist) (- j dist) width)))
                                  (s32-set! g135 row col-g135 n-gl
                                            (+ (s32-ref g135 row col-g135 n-gl) 1)))))))))
                (list n-gl n-gl 4
                      (list (s32vector->f32vector g0)
                            (s32vector->f32vector g45)
                            (s32vector->f32vector g90)
                            (s32vector->f32vector g135)))))))))
      (error "Not a GRAY image, n-chan is: " (im-n-channel image))))
