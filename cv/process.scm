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


(define-module (cv process)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv features)
  #:use-module (cv utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-and
            im-and-channel
	    im-or
            im-or-channel
            im-xor
            im-range
            im-mtimes
            im-mtimes-channel
            im-mdivide
            im-mdivide-channel
	    im-min
	    im-max
            im-map
            im-map-channel
            im-reduce
            im-reduce-channel
            im-normalize
            im-normalize-channel
            im-composite->rgb))


(g-export im-add
          im-add-channel
          im-subtract
          im-subtract-channel
          im-times
          im-times-channel
          im-divide
          im-divide-channel)


;;;
;;; Guile-CV additional API
;;;

(define (im-matrix-op image img-2 op)
  (match image
    ((width height n-chan idata)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (if (and (= width width-2)
                 (= height height-2)
                 (= n-chan n-chan-2))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (match channels
                                  ((c1 c2)
                                   (im-matrix-channel-op c1 width height c2 op))))
                              (zip idata idata-2))))
            (error "Size missmatch.")))))))

(define (im-matrix-channel-op channel width height channel-2 op)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
    (f32vector-set! to i (op (f32vector-ref channel i)
                             (f32vector-ref channel-2 i))))))

(define (mtimes c1 width-1 height-1 c2 width-2)
  (let ((to (im-make-channel width-2 height-1)))
    (f32vector-mtimes c1 width-1 height-1 c2 width-2 to)
    to))

(define (mdivide c1 width-1 height-1 c2 width-2)
  (let* ((height-2 width-1)
         (i-c2 (im-make-channel width-2 height-2))
         (n-cell-2 (* width-2 height-2))
         (to (im-make-channel width-2 height-1)))
    (f32vector-mtimes c1 width-1 height-1
                      (f32vector-invert c2 i-c2 #:n-cell n-cell-2)
                      width-2
                      to)
    to))

(define (im-matrix-multdiv-op img-1 img-2 op)
  ;; The product is defined only if the number of columns in img-1 is
  ;; equal to the number of rows in img-2.
  ;; The division is a multiplication by the inverse (of img-2)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (if (= width-1 height-2)
            (list width-2 height-1 n-chan-1
                  (let ((map-proc (if (and (> n-chan-1 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (match channels
                                  ((c1 c2)
                                   (op c1 width-1 height-1 c2 width-2))))
                              (zip idata-1 idata-2))))
            (error "Size missmatch.")))))))

#;(define-method (im-add image (val <number>))
  (im-map (lambda (p-val) (+ p-val val)) image))

#;(define-method (im-add-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (+ p-val val)) width height channel))

(define-method (im-add image (val <number>))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (map-proc (lambda (channel)
                         (im-add-channel channel width height val))
                 idata))))))

(define-method (im-add-channel channel width height (val <number>))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (f32vector-add-value channel val to #:n-cell n-cell)
    to))

#;(define-method (im-add . images)
  (apply im-map + images))

#;(define-method (im-add-channel width height . channels)
  (apply im-map-channel + width height channels))

(define-method (im-add . images)
  (im-map-la im-add-channel images))

(define-method (im-add-channel width height . channels)
  (let ((n-cell (* width height))
        (to (im-make-channel width height)))
    (f32vector-add-vectors to n-cell channels)))


#;(define-method (im-subtract image (val <number>))
  (im-map (lambda (p-val) (- p-val val)) image))

#;(define-method (im-subtract-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (- p-val val)) width height channel))

(define-method (im-subtract image (val <number>))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (map-proc (lambda (channel)
                         (im-subtract-channel channel width height val))
                 idata))))))

(define-method (im-subtract-channel channel width height (val <number>))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (f32vector-subtract-value channel val to #:n-cell n-cell)
    to))

#;(define-method (im-subtract . images)
  (apply im-map - images))

#;(define-method (im-subtract-channel width height . channels)
  (apply im-map-channel - width height channels))

(define-method (im-subtract . images)
  (im-map-la im-subtract-channel images))

(define-method (im-subtract-channel width height . channels)
  (let ((n-cell (* width height))
        (to (im-make-channel width height)))
    (f32vector-subtract-vectors to n-cell channels)))


#;(define-method (im-times image (val <number>))
  (im-map (lambda (p-val) (* p-val val)) image))

#;(define-method (im-times-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (* p-val val)) width height channel))

(define-method (im-times image (val <number>))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (map-proc (lambda (channel)
                         (im-times-channel channel width height val))
                 idata))))))

(define-method (im-times-channel channel width height (val <number>))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (f32vector-times-value channel val to #:n-cell n-cell)
    to))

#;(define-method (im-times . images)
  (apply im-map * images))

#;(define-method (im-times-channel width height . channels)
  (apply im-map-channel * width height channels))

(define-method (im-times . images)
  (im-map-la im-times-channel images))

(define-method (im-times-channel width height . channels)
  (let ((n-cell (* width height))
        (to (im-make-channel width height)))
    (f32vector-times-vectors to n-cell channels)))

(define (im-mtimes-1 prev images)
  (if (null? images)
      prev
      (match images
        ((ii . rest)
         (im-mtimes-1 (im-matrix-multdiv-op prev ii mtimes)
                      rest)))))

(define (im-mtimes . images)
  (match images
    ((i1 . rest)
     (im-mtimes-1 i1 rest))
    (else
     (error "Wrong arguments:" images))))

(define (im-mtimes-channel-1 channel width height rest)
  (if (null? rest)
      (values channel width height)
      (match rest
        ((channel-i width-i height-i . rest)
         (let ((to (im-make-channel width-i height)))
           (im-mtimes-channel-1 (f32vector-mtimes channel width height
                                                  channel-i width-i to)
                                width-i height rest))))))

(define (im-mtimes-channel . rest)
  (match rest
    ((channel width height . rest)
     (im-mtimes-channel-1 channel width height rest))
    (else
     (error "Wrong arguments:" rest))))


#;(define-method (im-divide image (val <number>))
  (im-map (lambda (p-val) (/ p-val val)) image))

#;(define-method (im-divide-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (/ p-val val)) width height channel))

(define-method (im-divide image (val <number>))
  (if (= val 0.0)
      (error "Attempt to divide by 0")
      (match image
        ((width height n-chan idata)
         (list width height n-chan
               (let ((map-proc (if (and (> n-chan 1)
                                        (%use-par-map)) par-map map)))
                 (map-proc (lambda (channel)
                             (im-divide-channel channel width height val))
                     idata)))))))

(define-method (im-divide-channel channel width height (val <number>))
  (if (= val 0.0)
      (error "Attempt to divide by 0")
      (let ((to (im-make-channel width height))
            (n-cell (* width height)))
        (f32vector-divide-value channel val to #:n-cell n-cell)
        to)))

#;(define-method (im-divide . images)
  (apply im-map / images))

#;(define-method (im-divide-channel width height . channels)
  (apply im-map-channel / width height channels))

(define-method (im-divide . images)
  (im-map-la im-divide-channel images))

(define-method (im-divide-channel width height . channels)
  (let ((n-cell (* width height))
        (to (im-make-channel width height)))
    (f32vector-divide-vectors to n-cell channels)))

(define (im-mdivide-1 prev images)
  (if (null? images)
      prev
      (match images
        ((ii . rest)
         (im-mdivide-1 (im-matrix-multdiv-op prev ii mdivide)
                       rest)))))

(define (im-mdivide . images)
  (match images
    ((i1 . rest)
     (im-mdivide-1 i1 rest))
    (else
     (error "Wrong arguments:" images))))

(define (im-mdivide-channel-1 channel width height rest)
  (if (null? rest)
      (values channel width height)
      (match rest
        ((channel-i width-i height-i . rest)
         (let ((i-ci (im-make-channel width-i height-i))
               (n-cell-i (* width-i height-i))
               (to (im-make-channel width-i height)))
           (im-mdivide-channel-1 (f32vector-mtimes channel width height
                                                   (f32vector-invert channel-i i-ci
                                                                     #:n-cell n-cell-i)
                                                   width-i
                                                   to)
                                 width-i height rest))))))

(define (im-mdivide-channel . rest)
  (match rest
    ((channel width height . rest)
     (im-mdivide-channel-1 channel width height rest))
    (else
     (error "Wrong arguments:" rest))))


(define-method (im-map-la im-map-la-channel image (val <number>))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (map-proc (lambda (channel)
                         (apply im-map-la-channel width height channel val))
                 idata))))))

(define-method (im-map-la im-map-la-channel images)
  (match images
    ((image) image)
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (apply im-map-la-channel width height channels))
                        (apply zip (apply im-collect 'channels images)))))
	    (error "Size mismatch.")))))
    (()
     (error "Invalid argument: " images))))


(define (im-range image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-range-channel c width))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (im-range-channel channel width))
              idata)))))))

(define (im-range-channel channel width)
  (match (f32vector-range channel)
    ((mini p-mini maxi p-maxi)
     (list mini (quotient p-mini width) (remainder p-mini width)
           maxi (quotient p-maxi width) (remainder p-maxi width)))))

(define (im-min image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-min-channel c width))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (receive (val row col)
                          (im-min-channel channel width)
                        (list val row col)))
              idata)))))))

(define (im-min-channel channel width)
  (receive (val pos)
      (f32vector-min channel)
    (values val (quotient pos width) (remainder pos width))))

(define (im-max image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-max-channel c width))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (receive (val row col)
                          (im-max-channel channel width)
                        (list val row col)))
              idata)))))))

(define (im-max-channel channel width)
  (receive (val pos)
      (f32vector-max channel)
    (values val (quotient pos width) (remainder pos width))))

(define (im-reduce image proc default)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-reduce-channel c proc default))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (im-reduce-channel channel proc default))
              idata)))))))

(define (im-reduce-channel channel proc default)
  (f32vector-reduce channel proc default))

(define (im-map proc . images)
  (match images
    ((image)
     (match image
       ((width height n-chan idata)
        (list width height n-chan
              (let ((map-proc (if (and (> n-chan 1)
                                       (%use-par-map)) par-map map)))
                (map-proc (lambda (channel)
                            (im-map-channel proc width height channel))
                    idata))))))
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (apply im-map-channel proc width height channels))
                        (apply zip (apply im-collect 'channels images)))))
	    (error "Size mismatch.")))))
    (()
     (error "Invalid argument: " images))))

(define (im-map-channel proc width height . channels)
  (let ((to (im-make-channel width height))
        (n-cell (* width height))
        (n-channels (length channels)))
    (if (> n-channels 1)
        (do ((i 0
                (+ i 1)))
            ((= i n-cell))
          (f32vector-set! to i
                          (f32vector-reduce-at-offset channels proc 0.0 i)))
        (do ((c (car channels))
             (i 0
                (+ i 1)))
            ((= i n-cell))
          (f32vector-set! to i
                            (proc (f32vector-ref c i)))))
    to))

#;(define (im-and . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (apply im-collect 'gray (map %im-rgb->gray rest))))
	      (list width height n-chan
                    (let ((map-proc (if (and (> n-chan 1)
                                             (%use-par-map)) par-map map)))
		      (map-proc
		       (lambda (channel)
			 (do ((i 0
				 (+ i 1)))
			     ((= i n-cell) channel)
			   (unless (f32vector-and-at-offset c-channels i)
			     (f32vector-set! channel i 0.0))))
		       (match img-2 ((_ _ _ idata) idata))))))
	    (error "Size mismatch.")))))
    ((image) image)
    (() (error "Invalid argument: " images))))

(define (im-and . images)
  (let ((%im-rgb->gray
         (@ (cv transform) im-rgb->gray)))
    (match images
      ((image) image)
      ((i1 . rest)
       (match i1
         ((width height n-chan idata)
	  (if (and (apply = (apply im-collect 'width images))
                   (apply = (apply im-collect 'height images)))
              (let ((c-rest (apply im-collect 'gray (map %im-rgb->gray rest))))
                (match idata
                  ((c1 . rest)
                   (let ((c-and (apply im-and-channel width height c-rest)))
                     (list width height n-chan
                           (let ((map-proc (if (and (> n-chan 1)
                                                    (%use-par-map)) par-map map)))
                             (map-proc (lambda (chan)
                                         (im-and-channel width height chan c-and))
                                 idata)))))
                  ((c)
                   (list width height n-chan
                         (apply im-and-channel width height
                                (cons c c-rest))))))
	      (error "Size mismatch.")))))
      (() (error "Invalid argument: " images)))))

(define (im-and-channel width height . channels)
  (match channels
    ((c1) c1)
    ((c1 . rest)
     (let ((n-cell (* width height))
           (to (im-make-channel width height)))
       (f32vector-and-vectors to n-cell channels)))
    (else
     (error "Invalid argument:" channels))))


#;(define (im-or . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (apply im-collect 'gray (map %im-rgb->gray rest))))
	      (list width height n-chan
                    (let ((map-proc (if (and (> n-chan 1)
                                             (%use-par-map)) par-map map)))
		      (map-proc
		       (lambda (channel)
			 (do ((i 0
				 (+ i 1)))
			     ((= i n-cell) channel)
			   (unless (> (f32vector-ref channel i) 0.0)
			     (f32vector-set! channel i
					     (f32vector-mean-at-offset c-channels i)))))
		       (match img-2 ((_ _ _ idata) idata))))))
	    (error "Size mismatch.")))))
    ((image) image)
    (() (error "Invalid argument: " images))))

(define (im-or . images)
  (let ((%im-rgb->gray
         (@ (cv transform) im-rgb->gray)))
    (match images
      ((image) image)
      ((i1 . rest)
       (match i1
         ((width height n-chan idata)
	  (if (and (apply = (apply im-collect 'width images))
                   (apply = (apply im-collect 'height images)))
              (let ((c-rest (apply im-collect 'gray (map %im-rgb->gray rest))))
                (match idata
                  ((c1 . rest)
                   (let ((c-or (apply im-or-channel width height c-rest)))
                     (list width height n-chan
                           (let ((map-proc (if (and (> n-chan 1)
                                                    (%use-par-map)) par-map map)))
                             (map-proc (lambda (chan)
                                         (im-or-channel width height chan c-or))
                                 idata)))))
                  ((c)
                   (list width height n-chan
                         (apply im-or-channel width height
                                (cons c c-rest))))))
	      (error "Size mismatch.")))))
      (() (error "Invalid argument: " images)))))

(define (im-or-channel width height . channels)
  (match channels
    ((c1) c1)
    ((c1 . rest)
     (let ((n-cell (* width height))
           (to (im-make-channel width height)))
       (f32vector-or-vectors to n-cell channels)))
    (else
     (error "Invalid argument:" channels))))


#;(define (im-xor . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if  (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (im-xor-channels channels width height))
                        (apply zip (apply im-collect 'channels images)))))
	    (error "Size mismatch.")))))
    ((image) image)
    (() (error "Invalid argument: " images))))

#;(define (im-xor-channels channels width height)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell))
      (f32vector-set! to i
                      (f32vector-xor-at-offset channels i)))
    to))

(define (im-xor . images)
  (match images
    ((image) image)
    ((i1 . rest)
     (match i1
       ((width height n-chan idata)
	(if (and (apply = (apply im-collect 'width images))
                 (apply = (apply im-collect 'height images))
                 (apply = (apply im-collect 'n-channel rest)))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (apply im-xor-channel width height channels))
                        (apply zip (apply im-collect 'channels images)))))
	    (error "Size mismatch.")))))
    (() (error "Invalid argument: " images))))

(define (im-xor-channel width height . channels)
  (match channels
    ((c1) c1)
    ((c1 . rest)
     (let ((n-cell (* width height))
           (to (im-make-channel width height)))
       (f32vector-xor-vectors to n-cell channels)))
    (else
     (error "Invalid argument:" channels))))

(define* (im-normalize image #:key (val 255.0))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-normalize-channel channel width height #:val val))
	       idata))))))

(define* (im-normalize-channel channel width height #:key (val 255.0))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (f32vector-set! to i (/ (f32vector-ref channel i)
                              val)))
    to))

(define (%merge-channel-color i)
  (case (i)
    ((3) '(255.0 255.0 255.0))	;; white
    ((4) '(0.0 255.0 255.0))	;; cyan
    ((5) '(255.0 0.0 255.0))	;; magenta
    ((6) '(255.0 255.0 0.0))	;; yellow
    (else
     "No such merge channel color index:" i)))

(define (im-composite->rgb image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c1 c2 c3 c4 . rest)
        (let ((n-cell (* width height))
              (r-chan (im-make-channel width height))
              (g-chan (im-make-channel width height))
              (b-chan (im-make-channel width height)))
          (do ((i 0
                  (+ i 1)))
              ((= i n-cell)
               (list width height 3
                     (list r-chan g-chan b-chan)))
            (if (= (f32vector-ref c4 i) 255.0)
                (begin
                  (f32vector-set! r-chan i 255.0)
                  (f32vector-set! g-chan i 255.0)
                  (f32vector-set! b-chan i 255.0))
                (begin
                  (f32vector-set! r-chan i
                                  (f32vector-ref c1 i))
                  (f32vector-set! g-chan i
                                  (f32vector-ref c2 i))
                  (f32vector-set! b-chan i
                                  (f32vector-ref c3 i)))))))
       (else
        (error "Not a composite image: " image))))))
