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


(define-module (cv morphology)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv impex)
  #:use-module (cv utils)
  #:use-module (cv segmentation)
  #:use-module (cv features)
  #:use-module (cv process)
  #:use-module (cv transform)
  #:use-module (cv particles)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-disc-erode
	    im-disc-erode-channel
	    im-disc-dilate
	    im-disc-dilate-channel
	    im-open
	    im-open-channel
	    im-close
	    im-close-channel
	    im-fill-holes
	    im-fill-holes-channel
            im-delineate
            im-delineate-channel
            im-distance-map
            im-distance-map-channel
            im-reconstruct))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define (im-disc-erode image radius)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-disc-erode-channel channel width height radius))
		       idata))))))

(define (im-disc-erode-channel channel width height radius)
  (let ((to (im-make-channel width height)))
    (case (vigra-disc-erode channel to width height radius)
      ((0) to)
      (else
       (error "Disc erode failed.")))))

(define (im-disc-dilate image radius)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-disc-dilate-channel channel width height radius))
		       idata))))))

(define (im-disc-dilate-channel channel width height radius)
  (let ((to (im-make-channel width height)))
    (case (vigra-disc-dilate channel to width height radius)
      ((0) to)
      (else
       (error "Disc dilate failed.")))))

(define (im-open image radius)
  (im-disc-dilate (im-disc-erode image radius) radius))

(define (im-open-channel channel width height radius)
  (im-disc-dilate-channel (im-disc-erode-channel channel width height radius)
			  width height radius))

(define (im-close image radius)
  (im-disc-erode (im-disc-dilate image radius) radius))

(define (im-close-channel channel width height radius)
  (im-disc-erode-channel (im-disc-dilate-channel channel width height radius)
			 width height radius))

(define* (im-fill-holes image #:key (con 8))
  ;; (im-binary? image) is rather expensive
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
	(list width height n-chan
	      (list (im-fill-holes-channel c width height #:con con))))
       (else
	(error "Not a binary image."))))))

#!
(define* (im-fill-holes-channel channel width height #:key (con 8))
  (let* ((new-w (+ width 2))
	 (new-h (+ height 2))
	 (p-channel (im-padd-channel channel width height 1 1 1 1
				     #:new-w new-w #:new-h new-h))
	 (l-channel (im-label-all-channel p-channel new-w new-h #:con con))
	 (bg-label (f32vector-ref l-channel 0)))
    (do ((i 0
	    (+ i 1)))
	((= i (* new-w new-h)))
      ;; labels are 'discrete' floats, by definition, so we can use =
      ;; instead of float=?, which is 3 to 4 times faster
      (if (= #;float=? (f32vector-ref l-channel i) bg-label)
	  (f32vector-set! l-channel i 0.0)
	  (f32vector-set! l-channel i 255.0)))
    (im-unpadd-channel l-channel new-w new-h 1 1 1 1
		       #:new-w width #:new-h height)))
!#

(define* (im-fill-holes-channel channel width height #:key (con 8))
  (let* ((new-w (+ width 2))
	 (new-h (+ height 2))
	 (p-channel (im-padd-channel channel width height 1 1 1 1
				     #:new-w new-w #:new-h new-h))
	 (l-channel (im-label-all-channel p-channel new-w new-h #:con con))
	 (bg-label (f32vector-ref l-channel 0)))
    (f32vector-fill-holes l-channel (* new-w new-h) bg-label)
    (im-unpadd-channel l-channel new-w new-h 1 1 1 1
		       #:new-w width #:new-h height)))


#!
;; this is slower then the above, unexpectedly
(define* (im-fill-holes-channel channel width height #:key (con 8))
  (letrec* ((new-w (+ width 2))
            (new-h (+ height 2))
            (n-cell-new (* new-w new-h))
            (p-channel (im-padd-channel channel width height 1 1 1 1
                                        #:new-w new-w #:new-h new-h))
            (l-channel (im-label-all-channel p-channel new-w new-h #:con con))
            (bg-label (f32vector-ref l-channel 0))
            (proc (lambda (range)
                    (match range
                      ((start end)
                       (do ((i start
                               (+ i 1)))
                           ((= i end))
                         ;; labels are 'discrete' floats, by definition, so we can use =
                         ;; instead of float=?, which is 3 to 4 times faster
                         (if (= #;float=? (f32vector-ref l-channel i) bg-label)
                             (f32vector-set! l-channel i 0.0)
                             (f32vector-set! l-channel i 255.0))))))))
    (if (%use-par-map)
        (par-for-each proc
                      (n-cell->per-core-start-end n-cell-new))
        (proc (list 0 n-cell-new)))
    (im-unpadd-channel l-channel new-w new-h 1 1 1 1
		       #:new-w width #:new-h height)))
!#

(define* (im-delineate image #:key (threshold  10) (radius 2))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-delineate-channel channel width height
                                               #:threshold threshold
                                               #:radius radius))
		       idata))))))

#;(define* (im-delineate-channel channel width height
                               #:key (threshold  10) (radius 2))
  (let ((channel-min (im-disc-erode-channel channel width height radius))
        (channel-max (im-disc-dilate-channel channel width height radius))
        (to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (let* ((ori (f32vector-ref channel i))
             (mini (f32vector-ref channel-min i))
             (maxi (f32vector-ref channel-max i))
             (diff (- maxi mini)))
        (f32vector-set! to i
                        (if (< diff threshold)
                            ;; not an edge
                            ori
                            ;; an edge
                            (if (< (- ori mini)
                                   (- maxi ori))
                                mini
                                maxi)))))))

(define* (im-delineate-channel channel width height
                               #:key (threshold  10) (radius 2))
  (let ((to (im-make-channel width height)))
    (f32vector-delineate channel
                         (im-disc-erode-channel channel width height radius)
                         (im-disc-dilate-channel channel width height radius)
                         to
                         #:n-cell (* width height)
                         #:threshold threshold)
    to))

(define* (im-distance-map image
                          #:key (bg 'black) (mode 'euclidian))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-distance-map-channel channel width height
                                                  #:bg bg #:mode mode))
                 idata))))))

(define* (im-distance-map-channel channel width height
                                  #:key (bg 'black) (mode 'euclidian))
  (let ((to (im-make-channel width height)))
    (case (vigra-distance-transform channel to width height bg mode)
      ((0) to)
      (else
       (error "Distance map failed.")))))

(define* (im-reconstruct image seeds #:key (con 8))
  ;; Returns a copy of IMAGE excluding objects for which (a) none of the
  ;; bounding boxes of the SEEDS objects intersect and (b) those for
  ;; which none of the SEEDS objects that intersect actually 'share'
  ;; pixels.  Note (remember) that im-features returns an ascending
  ;; ordered list based on the label value, and the first feature is
  ;; always the background feature.
  (match image
    ((i-width i-height i-n-chan _)
     (match seeds
       ((s-width s-height s-n-chan _)
        (if (and (= i-width s-width)
                 (= i-height s-height)
                 (= i-n-chan s-n-chan 1))
            (receive (i-labels i-n-label)
                (im-label image #:con con)
              (receive (s-labels s-n-label)
                  (im-label seeds #:con con)
                (let* ((i-chan (im-channel image 0))
                       (i-label-chan (im-channel i-labels 0))
                       (i-features (im-features image i-labels))
                       (i-bb (features-bb i-features))
                       (s-features (im-features seeds s-labels))
                       (s-bb (features-bb s-features))
                       (i-bb-s-bbs (map
                                       (lambda (bbi)
                                         (cons bbi
                                               (filter-map (lambda (bbs)
                                                             (and (bb-intersect? bbi bbs) bbs))
                                                   ;; first feature is for the bg, do not process
                                                   (cdr s-bb))))
                                     ;; first feature is for the bg, do not process
                                     (cdr i-bb)))
                       (to-keep (cons 0 ;; keep the bg that on purpose has been'skipped' above
                                      (reverse!
                                       (fold (lambda (a-bbi i prev)
                                               ;; since we skipped the bg feature, the
                                               ;; i we receive here is (- label-id 1), hence
                                               ;; we must add 1 to get the correct label-id
                                               (let ((real-i (+ i 1)))
                                                 (if (or (null? (cdr a-bbi))
                                                         (member real-i prev)
                                                         (not (any-seeds? image seeds a-bbi)))
                                                     prev
                                                     (cons real-i prev))))
                                             '()
                                             i-bb-s-bbs
                                             (iota (length i-bb-s-bbs))))))
                       (to-scrap (lset-difference = (iota i-n-label) to-keep)))
                  (list i-width i-height 1
                        (list (im-scrap-channel i-chan i-label-chan i-width i-height
                                                to-scrap i-n-label))))))
            (error "Wrong argument(s): expecting same size binary images.")))))))

(define (any-seeds? image seeds i-bb-s-bbs)
  ;; Both IMAGE and SEEDS are binary images of the same size. I-BB-S-BBS
  ;; is a list of bounding boxes. The first is a bounding box for an
  ;; object in IMAGE, the rest is a list - which maybe empty - of
  ;; bounding boxes in SEEDS, that are (at least partially) inside the
  ;; former. This procedure returns #t if any of the later do 'share'
  ;; some pixels or even a single pixel with the object defined in IMAGE
  ;; by the first bounding box.
  (match i-bb-s-bbs
    ((i-bb . rest)
     (if (null? rest)
         #f
         ;; Note that it is essential to first obtain and clean the IMAGE
         ;; particle given by I-BB
         (match i-bb
           ((i-left i-top i-right i-bottom)
            (let ((i-p-clean (im-particle-clean
                              (im-crop image i-left i-top (+ i-right 1) (+ i-bottom 1))))
                  (n-rest (length rest))
                  (result #f))
              #;(im-save i-p-clean (%is-a-seed-tmp-filename "i-p-clean-"
                                                          i-left i-top i-right i-bottom))
              (do ((i 0
                      (+ i 1)))
                  ((or result
                       (= i n-rest))
                   result)
                (if (is-a-seed? i-p-clean i-left i-top seeds i-bb (list-ref rest i))
                    (set! result #t))))))))))

#;(define (is-a-seed? i-p-clean i-left i-top seeds i-bb s-bb)
  (match (shared-bb i-bb s-bb)
    ((left top right bottom)
     (let* ((i-p-left (- left i-left))
            (i-p-top (- top i-top))
            (i-p-right (- right i-left))
            (i-p-bottom (- bottom i-top))
            (i-crop (im-crop i-p-clean i-p-left i-p-top (+ i-p-right 1) (+ i-p-bottom 1)))
            (i-chan (im-channel i-crop 0))
            (s-crop (im-crop seeds left top (+ right 1) (+ bottom 1)))
            (s-chan (im-channel s-crop 0))
            (result #f))
       #;(dimfi i-p-left i-p-top (+ i-p-right 1) (+ i-p-bottom 1))
       #;(im-save i-crop (%is-a-seed-tmp-filename "i-crop-" i-p-left
                                                i-p-top i-p-right i-p-bottom))
       #;(im-save s-crop (%is-a-seed-tmp-filename "s-crop-" left top right bottom))
     (match i-crop
       ((width height _ _)
        (do ((i 0
                (+ i 1)))
            ((or result
                 (= i (* width height)))
               result)
          (if (and (= (f32vector-ref i-chan i) 255.0)
                   (= (f32vector-ref s-chan i) 255.0))
              (set! result #t)))))))))

(define (is-a-seed? i-p-clean i-left i-top seeds i-bb s-bb)
  (match (shared-bb i-bb s-bb)
    ((left top right bottom)
     (let* ((i-p-left (- left i-left))
            (i-p-top (- top i-top))
            (i-p-right (- right i-left))
            (i-p-bottom (- bottom i-top))
            (i-crop (im-crop i-p-clean i-p-left i-p-top (+ i-p-right 1) (+ i-p-bottom 1)))
            (i-chan (im-channel i-crop 0))
            (s-crop (im-crop seeds left top (+ right 1) (+ bottom 1)))
            (s-chan (im-channel s-crop 0)))
     (match i-crop
       ((width height _ _)
        (f32vector-is-a-seed? i-chan (* width height) s-chan)))))))

(define (%is-a-seed-tmp-filename prefix left top right bottom)
  (string-append "/tmp/david/guile-cv/"
                 prefix
                 (number->string left) "-"
                 (number->string top) "-"
                 (number->string right) "-"
                 (number->string bottom) "-"
                 ".png"))

(define (shared-bb i-bb s-bb)
  (match i-bb
    ((i-left i-top i-right i-bottom)
     (match s-bb
       ((s-left s-top s-right s-bottom)
        (match (sort (list i-left i-right s-left s-right) <)
          ((_ left right _)
           (match (sort (list i-top i-bottom s-top s-bottom) <)
             ((_ top bottom _)
              (list left top right bottom))))))))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-disc-erode from to width height radius)
  (vigra_disc_erode (bytevector->pointer from)
		    (bytevector->pointer to)
		    width
		    height
		    radius))

(define (vigra-disc-dilate from to width height radius)
  (vigra_disc_dilate (bytevector->pointer from)
		     (bytevector->pointer to)
		     width
		     height
		     radius))

(define (vigra-distance-transform from to width height bg mode)
  (vigra_distance_transform (bytevector->pointer from)
                            (bytevector->pointer to)
                            width
                            height
                            (case bg
                              ((black) 0.0)
                              ((white) 255.0)
                              (else
                               (error "No such background: " bg)))
                            (case mode
                              ((chessboard) 0)
                              ((manhattan) 1)
                              ((euclidian) 2)
                              (else
                               (error "No such mode: " mode)))))


;;;
;;; Vigra_c bindings
;;;

(define vigra_disc_erode
  (pointer->procedure int
		      (dynamic-func "vigra_discerosion_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int      ;; height
			    int)))   ;; radius

(define vigra_disc_dilate
  (pointer->procedure int
		      (dynamic-func "vigra_discdilation_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int      ;; height
			    int)))   ;; radius

(define vigra_distance_transform
  (pointer->procedure int
		      (dynamic-func "vigra_distancetransform_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int      ;; height
                            float    ;; bg
			    int)))   ;; mode
