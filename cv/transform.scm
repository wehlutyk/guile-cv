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


(define-module (cv transform)
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

  #:export (im-rgb->gray
            im-rgba->rgb
            im-rgba->gray
            im-resize
	    im-resize-channel
	    im-rotate
	    im-rotate-channel
	    im-flip
	    im-flip-channel
            im-invert
            im-invert-channel
            im-transpose
            im-transpose-channel
            im-complement
	    im-crop-size
	    im-crop
	    im-crop-channel
	    im-padd-size
	    im-padd
	    im-padd-channel
	    im-unpadd-size
	    im-unpadd
	    im-unpadd-channel
            im-clip
            im-clip-channel
            im-local-minima
            im-local-minima-channel
            im-local-maxima
            im-local-maxima-channel
            im-fft
            im-fft-channel
            im-fft-inverse
            im-fft-inverse-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

#;(define (im-rgb->gray image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        image)
       ((r g b)
        (receive (c-chan extra)
            (im-rgb->gray-1 width height r g b)
          (values (list width height 1 (list c-chan))
                  extra)))
       (else
	(error "Not an RGB (nor a GRAY) image."))))))

(define (im-rgb->gray image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        image)
       ((r g b)
        (list width height 1
              (list (im-rgb->gray-1 width height r g b))))
       (else
	(error "Not an RGB (nor a GRAY) image."))))))

(define (im-rgb->gray-1 width height r g b)
  (let ((n-cell (* width height))
        (to (im-make-channel width height)))
    (f32vector-rgb-to-gray to n-cell r g b)
    to))

#;(define (im-rgb->gray-1 width height r g b)
  (letrec* ((rgb->gray (lambda (i)
                         (/ (+ (f32vector-ref r i)
                               (f32vector-ref g i)
                               (f32vector-ref b i))
                            3)))
            (to (im-make-channel width height))
            (n-cell (* width height))
            (proc (lambda (range)
                    (match range
                      ((start end)
                       (let ((k0 (rgb->gray start)))
                         (let loop ((i (+ start 1))
                                    (mini k0)
                                    (maxi k0)
                                    (total k0))
                           (if (= i end)
                               (list mini maxi total)
                               (let ((k (rgb->gray i)))
                                 (f32vector-set! to i k)
                                 (loop (+ i 1)
                                       (min mini k)
                                       (max maxi k)
                                       (+ total k)))))))))))
    (if (%use-par-map)
        (let ((vals (par-map proc
                         (n-cell->per-core-start-end n-cell))))
          (values to
                  (list (apply min (map car vals))
                        (apply max (map cadr vals))
                        (/ (reduce + 0 (map caddr vals)) n-cell)
                        n-cell)))
        (match (proc (list 0 n-cell))
          ((mini maxi total)
           (values to
                   (list mini
                         maxi
                         (/ total n-cell)
                         n-cell)))))))

#;(define (im-rgb->gray-1 width height r g b)
  (letrec* ((rgb->gray (lambda (i)
                         (/ (+ (f32vector-ref r i)
                               (f32vector-ref g i)
                               (f32vector-ref b i))
                            3)))
            (n-cell (* width height))
            (to (make-f32vector n-cell 0.0))
            (k0 (rgb->gray 0))
            (t0 (f32vector-set! to 0 k0)))
    (let loop ((i 1)
               (mini k0)
               (maxi k0)
               (total k0))
      (if (= i n-cell)
          (values to
                  mini
                  maxi
                  (/ total n-cell))
          (let ((k (rgb->gray i)))
            (f32vector-set! to i k)
            (loop (+ i 1)
                  (min mini k)
                  (max maxi k)
                  (+ total k)))))))

#!
Source => Target = (BGColor + Source) =
Target.R = ((1 - Source.A) * BGColor.R) + (Source.A * Source.R)
Target.G = ((1 - Source.A) * BGColor.G) + (Source.A * Source.G)
Target.B = ((1 - Source.A) * BGColor.B) + (Source.A * Source.B)
!#

(define* (im-rgba->rgb image #:key (bg '(0.0 0.0 0.0)))
  (let ((%im-normalize-channel
         (@ (cv process) im-normalize-channel)))
    (match bg
      ((bg-r bg-g bg-b)
       (match image
         ((width height n-chan idata)
          (case n-chan
            ((4)
             (match idata
               ((r g b a)
                (let ((a-norm (%im-normalize-channel a width height)))
                  (list width height 3
                        (let ((map-proc (if (%use-par-map) par-map map)))
                          (map-proc (lambda (vals)
                                      (match vals
                                        ((c bg)
                                         (im-rgba-channel->rgb-channel c width height
                                                                       a-norm #:bg bg))))
                              (list (list r bg-r)
                                    (list g bg-g)
                                    (list b bg-b)))))))))
            (else
             (error "Not an RGBA image."))))))
      (else
       (error "Invalid background color: " bg)))))

(define* (im-rgba-channel->rgb-channel c width height a-norm #:key (bg 0.0))
  (let* ((%im-normalize-channel
          (@ (cv process) im-normalize-channel))
         (c-norm (%im-normalize-channel c width height))
         (bg-norm (/ bg 255.0))
         (to (im-make-channel width height))
         (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (f32vector-set! to i
                      (* (if (= bg 0.0)
                             (* (f32vector-ref a-norm i) (f32vector-ref c-norm i))
                             (+ (* (- 1.0 (f32vector-ref a-norm i)) bg-norm)
                                (* (f32vector-ref a-norm i) (f32vector-ref c-norm i))))
                         255.0)))
    to))

(define* (im-rgba->gray image #:key (bg '(0.0 0.0 0.0)))
  (match image
    ((_ _ n-chan idata)
     (case n-chan
       ((3 1)
        (im-rgb->gray image))
       ((4)
        (im-rgb->gray (im-rgba->rgb image #:bg bg)))
       (else
	(error "Not an RGBA (nor an RGB neither a GRAY) image."))))))

(define* (im-resize image new-w new-h
		    #:key (i-mode 'bilinear))
  (match image
    ((width height n-chan idata)
     (list new-w new-h n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-resize-channel channel width height new-w new-h
					    #:i-mode i-mode))
	         idata))))))

(define* (im-resize-channel channel width height new-w new-h
			    #:key (i-mode 'bilinear))
  (let ((to (im-make-channel new-w new-h)))
    (case (vigra-resize-channel channel to width height new-w new-h
				(resize-mode->number i-mode))
      ((0) to)
      (else
       (error "Resize failed.")))))

(define (resize-mode->number i-mode)
  (case i-mode
    ((none) 0)
    ((bilin bilinear) 1)
    ((biquad biquadratic) 2)
    ((bicub bicubic) 3)
    ((trilin trilinear) 4)
    (else
     (error "No such resize interpolation mode: " i-mode))))

(define* (im-rotate image angle
		    #:key (i-mode 'bilinear))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-rotate-channel channel width height angle
					    #:i-mode i-mode))
	         idata))))))

(define* (im-rotate-channel channel width height angle
			    #:key (i-mode 'bilinear))
  (let ((to (im-make-channel width height)))
    (case (vigra-rotate-channel channel to width height angle
				(rotate-mode->number i-mode))
      ((0) to)
      (else
       (error "Rotation failed.")))))

(define (rotate-mode->number i-mode)
  (case i-mode
    ((bilin bilinear) 1)
    ((biquad biquadratic) 2)
    ((bicub bicubic) 3)
    ((trilin trilinear) 4)
    (else
     (error "No such rotate interpolation mode: " i-mode))))

(define (im-flip image plane)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-flip-channel channel width height plane))
	         idata))))))

(define (im-flip-channel channel width height plane)
  (let ((to (im-make-channel width height)))
    (case (vigra-flip-channel channel to width height
			      (flip-plane->axis plane))
      ((0) to)
      (else
       (error "Flip failed.")))))

(define (flip-plane->axis plane)
  (case plane
    ;; vigra uses axis, not plane, so:
    ;;   horizontal plane -> vigra vertical axis, which is the integer 2
    ;;   vertical plane -> vigra horizontal axis, which is the integer 1
    ((hori horizontal) 2)
    ((vert vertical) 1)
    ((both) 3)
    (else
     (error "No such flip plane: " plane))))

(define (im-invert image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-invert-channel channel width height))
                       idata))))))

(define (im-invert-channel channel width height)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (f32vector-invert channel to #:n-cell n-cell)
    to))

(define (im-transpose image)
  (match image
    ((width height n-chan idata)
     (list height width n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-transpose-channel channel width height))
	       idata))))))

#;(define (im-transpose-channel channel width height)
  (let ((t-width height)
        (to (im-make-channel height width)))
    (do ((i 0
	    (+ i 1)))
	((= i height))
      (do ((j 0
	      (+ j 1)))
	  ((= j width))
        (im-fast-channel-set! to j i t-width
                              (im-fast-channel-ref channel i j width))))
    to))

(define (im-transpose-channel channel width height)
  (let ((to (im-make-channel height width)))
    (f32vector-transpose channel width height to)
    to))

(define (im-complement image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc
	      (lambda (channel)
		(f32vector-complement channel))
	      idata))))))

(define (im-crop-size width height left top right bottom)
  (let ((new-w (- right left))
	(new-h (- bottom top)))
    (if (and (< left right)
	     (< top bottom)
	     (<= new-w width)
	     (<= new-h height))
	(list new-w new-h)
	(error "Invalid crop indices:" left top right bottom))))

(define (im-crop image left top right bottom)
  (match image
    ((width height n-chan idata)
     (match (im-crop-size width height left top right bottom)
       ((new-w new-h)
	(list new-w new-h n-chan
	      (let ((map-proc (if (and (> n-chan 1)
                                       (%use-par-map)) par-map map)))
		(map-proc (lambda (channel)
			    (im-crop-channel channel width height left top right bottom
					     #:new-w new-w #:new-h new-h))
		    idata))))))))

(define* (im-crop-channel channel width height left top right bottom
			  #:key (new-w #f) (new-h #f))
  (let ((to (if (and new-w new-h)
		(im-make-channel new-w new-h)
		(match (im-crop-size width height left top right bottom)
		  ((new-w new-h)
		   (im-make-channel new-w new-h))))))
    (case (vigra-crop-channel channel to width height left top right bottom)
      ((0) to)
      (else
       (error "Crop failed.")))))

(define (im-padd-size width height left top right bottom)
  (if (and-l (map (lambda (padd) (>= padd 0))
	       (list left top right bottom)))
      (list (+ left width right)
	    (+ top height bottom))
      (error "Invalid padd value(s): " left top right bottom)))

(define* (im-padd image left top right bottom #:key (color '(0.0 0.0 0.0)))
  (match image
    ((width height n-chan idata)
     (match (im-padd-size width height left top right bottom)
       ((new-w new-h)
	(list new-w new-h n-chan
              (match idata
                ((c)
                 (list (im-padd-channel c width height left top right bottom
                                        #:new-w new-w #:new-h new-h
                                        #:value (/ (reduce + 0 color) 3))))
                ((r g b)
                 (let ((map-proc (if (%use-par-map) par-map map)))
                   (map-proc (lambda (chaco)
                               (match chaco
                                 ((channel value)
                               (im-padd-channel channel width height left top right bottom
                                                #:new-w new-w #:new-h new-h #:value value))))
			  (zip idata color)))))))))))

(define* (im-padd-channel channel width height left top right bottom
			  #:key (new-w #f) (new-h #f) (value 0.0))
  (let ((to (if (and new-w new-h)
		(im-make-channel new-w new-h value)
		(match (im-padd-size width height left top right bottom)
		  ((new-w new-h)
		   (im-make-channel new-w new-h value))))))
    (case (vigra-padd-channel channel to width height left top right bottom)
      ((0) to)
      (else
       (error "Padd failed.")))))

(define (im-unpadd-size width height left top right bottom)
  (if (and-l (map (lambda (padd) (>= padd 0.0))
	       (list left top right bottom)))
      (list (- width left right)
	    (- height top bottom))
      (error "Invalid unpadd value(s): " left top right bottom)))

(define (im-unpadd image left top right bottom)
  (match image
    ((width height n-chan idata)
     (match (im-unpadd-size width height left top right bottom)
       ((new-w new-h)
	(list new-w new-h n-chan
	      (let ((map-proc (if (and (> n-chan 1)
                                       (%use-par-map)) par-map map)))
		(map-proc (lambda (channel)
			    (im-unpadd-channel channel width height left top right bottom
					       #:new-w new-w #:new-h new-h))
		    idata))))))))

(define* (im-unpadd-channel channel width height left top right bottom
			    #:key (new-w #f) (new-h #f))
  (let ((to (if (and new-w new-h)
		(im-make-channel new-w new-h)
		(match (im-unpadd-size width height left top right bottom)
		  ((new-w new-h)
		   (im-make-channel new-w new-h))))))
    (case (vigra-crop-channel channel to
			      width height left top (+ left new-w) (+ top new-h))
      ((0) to)
      (else
       (error "Unpadd failed.")))))

(define* (im-clip image #:key (lower 0.0) (upper 255.0))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-clip-channel channel width height #:lower lower #:upper upper))
	         idata))))))

(define* (im-clip-channel channel width height #:key (lower 0.0) (upper 255.0))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-clip-channel channel to width height lower upper)
      ((0) to)
      (else
       (error "Clip failed.")))))

(define* (im-local-minima image
                          #:key (con 8)
                          (marker 1.0)
                          (threshold +float-max+)
                          (borders? #f)
                          (plateaus? #f)
                          (epsilon 1.0e-4))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-local-minima-channel channel width height
                                                  #:con con
                                                  #:marker marker
                                                  #:threshold threshold
                                                  #:borders? borders?
                                                  #:plateaus? plateaus?
                                                  #:epsilon epsilon))
	         idata))))))

(define* (im-local-minima-channel channel width height
                                  #:key (con 8)
                                  (marker 1.0)
                                  (threshold +float-max+)
                                  (borders? #f)
                                  (plateaus? #f)
                                  (epsilon 1.0e-4))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-local-minima channel to width height con marker threshold
                              borders? plateaus? epsilon)
      ((0) to)
      (else
       (error "Local minima failed.")))))

(define* (im-local-maxima image
                          #:key (con 8)
                          (marker 1.0)
                          (threshold (- +float-max+))
                          (borders? #f)
                          (plateaus? #f)
                          (epsilon 1.0e-4))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-local-maxima-channel channel width height
                                                  #:con con
                                                  #:marker marker
                                                  #:threshold threshold
                                                  #:borders? borders?
                                                  #:plateaus? plateaus?
                                                  #:epsilon epsilon))

	         idata))))))

(define* (im-local-maxima-channel channel width height
                                  #:key (con 8)
                                  (marker 1.0)
                                  (threshold (- +float-max+))
                                  (borders? #f)
                                  (plateaus? #f)
                                  (epsilon 1.0e-4))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-local-maxima channel to width height con marker threshold
                              borders? plateaus? epsilon)
      ((0) to)
      (else
       (error "Local maxima failed.")))))

(define (im-fft image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (receive (to-real to-imaginary)
            (im-fft-channel c width height)
          (values (list width height n-chan (list to-real))
                  (list width height n-chan (list to-imaginary)))))
       ((r g b)
        (let* ((map-proc (if (%use-par-map) par-map map))
               (fft-ri (map-proc (lambda (channel)
                                   (receive (c-real c-imaginary)
                                       (im-fft-channel channel width height)
                                     (list c-real c-imaginary)))
	                   idata)))
          (apply values
                 (map (lambda (z-item)
                        (cons* width height n-chan (list z-item)))
                   (apply zip fft-ri)))))
       (else
        (error "Not a GRAY nor an RGB image" width height n-chan))))))

(define (im-fft-channel channel width height)
  (let ((to-real (im-make-channel width height))
        (to-imaginary (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-fft-channel channel to-real to-imaginary width height)
      ((0)
       (values to-real
               to-imaginary))
      (else
       (error "Fft failed.")))))

(define (im-fft-inverse real imaginary)
  (match real
    ((width height n-chan idata)
     (match idata
       ((r-chan)
        (receive (to-real to-imaginary)
            (im-fft-inverse-channel r-chan
                                    (im-channel imaginary 0)
                                    width height)
          (values (list width height n-chan (list to-real))
                  (list width height n-chan (list to-imaginary)))))
       ((rr-chan rg-chan rb-chan)
        (match (im-channels imaginary)
          ((ir-chan ig-chan ib-chan)
           (let* ((map-proc (if (%use-par-map) par-map map))
                  (fft-ri (map-proc (lambda (ri-chan)
                                      (match ri-chan
                                        ((r-chan i-chan)
                                         (receive (c-real c-imaginary)
                                             (im-fft-inverse-channel r-chan i-chan width height)
                                           (list c-real c-imaginary)))))
	                      (apply zip (list idata (im-channels imaginary))))))
             (apply values
                    (map (lambda (z-item)
                           (cons* width height n-chan (list z-item)))
                      (apply zip fft-ri)))))))
       (else
        (error "Not GRAY nor RGB images" width height n-chan))))))

(define (im-fft-inverse-channel from-real from-imaginary width height)
  (let ((to-real (im-make-channel width height))
        (to-imaginary (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-fft-inverse-channel from-real from-imaginary
                                     to-real to-imaginary
                                     width height)
      ((0)
       (values to-real
               to-imaginary))
      (else
       (error "Fft inverse failed.")))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-resize-channel from to width height new-w new-h i-mode)
  (vigra_resize_channel (bytevector->pointer from)
			(bytevector->pointer to)
			width
			height
			new-w
			new-h
			i-mode))

(define (vigra-rotate-channel from to width height angle i-mode)
  (vigra_rotate_channel (bytevector->pointer from)
			(bytevector->pointer to)
			width
			height
			angle
			i-mode))

(define (vigra-flip-channel from to width height axis)
  (vigra_flip_channel (bytevector->pointer from)
		      (bytevector->pointer to)
		      width
		      height
		      axis))

(define (vigra-crop-channel from to width height left top right bottom)
  (vigra_crop_channel (bytevector->pointer from)
		      (bytevector->pointer to)
		      width
		      height
		      left
		      top
		      right
		      bottom))

(define (vigra-padd-channel from to width height left top right bottom)
  (vigra_padd_channel (bytevector->pointer from)
		      (bytevector->pointer to)
		      width
		      height
		      left
		      top
		      right
		      bottom))

(define (vigra-clip-channel from to width height lower upper)
  (vigra_clip_channel (bytevector->pointer from)
		      (bytevector->pointer to)
		      width
		      height
		      lower
		      upper))

(define (vigra-local-minima from to width height con marker threshold
                            borders? plateaus? epsilon)
  (vigra_local_minima (bytevector->pointer from)
                      (bytevector->pointer to)
                      width
                      height
                      (case con
                        ((8) 1)
                        ((4) 0)
                        (else
                         (error "No such connectivity: " con)))
                      marker
                      threshold
                      (if borders? 1 0)
                      (if plateaus? 1 0)
                      epsilon))

(define (vigra-local-maxima from to width height con marker threshold
                            borders? plateaus? epsilon)
  (vigra_local_maxima (bytevector->pointer from)
                      (bytevector->pointer to)
                      width
                      height
                      (case con
                        ((8) 1)
                        ((4) 0)
                        (else
                         (error "No such connectivity: " con)))
                      marker
                      threshold
                      (if borders? 1 0)
                      (if plateaus? 1 0)
                      epsilon))

(define (vigra-fft-channel from to-real to-imaginary width height)
  (vigra_fft_channel (bytevector->pointer from)
		     (bytevector->pointer to-real)
                     (bytevector->pointer to-imaginary)
		     width
		     height))

(define (vigra-fft-inverse-channel from-real from-imaginary
                                   to-real to-imaginary
                                   width height)
  (vigra_fft_inverse_channel (bytevector->pointer from-real)
                             (bytevector->pointer from-imaginary)
		             (bytevector->pointer to-real)
                             (bytevector->pointer to-imaginary)
		             width
		             height))


;;;
;;; Vigra_c bindings
;;;

(define vigra_resize_channel
  (pointer->procedure int
		      (dynamic-func "vigra_resizeimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; width
			    int		;; height
			    int		;; new width
			    int		;; new height
			    int)))	;; interpolation mode

(define vigra_rotate_channel
  (pointer->procedure int
		      (dynamic-func "vigra_rotateimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; width
			    int		;; height
			    float	;; angle
			    int)))	;; interpolation mode

(define vigra_flip_channel
  (pointer->procedure int
		      (dynamic-func "vigra_reflectimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; width
			    int		;; height
			    int)))	;; flip axis

(define vigra_crop_channel
  (pointer->procedure int
		      (dynamic-func "vigra_subimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
			    int		;; left
			    int		;; top
			    int		;; right
			    int)))	;; bottom

(define vigra_padd_channel
  (pointer->procedure int
		      (dynamic-func "vigra_paddimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
			    int		;; left
			    int		;; top
			    int		;; right
			    int)))	;; bottom

(define vigra_clip_channel
  (pointer->procedure int
		      (dynamic-func "vigra_clipimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
			    float	;; lower
			    float)))	;; upper

(define vigra_local_minima
    (pointer->procedure int
		      (dynamic-func "vigra_localminima_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
                            int		;; 8_con?
                            float	;; marker
                            float	;; threshold
                            int		;; borders?
                            int		;; plateaus?
                            float)))	;; epsilon

(define vigra_local_maxima
    (pointer->procedure int
		      (dynamic-func "vigra_localmaxima_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
                            int		;; 8_con?
                            float	;; marker
                            float	;; threshold
                            int		;; borders?
                            int		;; plateaus?
                            float)))	;; epsilon

(define vigra_fft_channel
  (pointer->procedure int
		      (dynamic-func "vigra_fouriertransform_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to-real channel
                            '*		;; to-imaginary channel
			    int		;; width
			    int)))	;; height

(define vigra_fft_inverse_channel
  (pointer->procedure int
		      (dynamic-func "vigra_fouriertransforminverse_c"
				    %libvigra-c)
		      (list '*		;; from-real channel
                            '*		;; from-imaginary channel
			    '*		;; to-real channel
                            '*		;; to-imaginary channel
			    int		;; width
			    int)))	;; height
