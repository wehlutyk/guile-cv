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


(define-module (cv filter)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-gaussian-blur
	    im-gaussian-blur-channel
	    im-gaussian-gradient
	    im-gaussian-gradient-channel
            im-gaussian-sharp
	    im-gaussian-sharp-channel
            im-sharpen
            im-sharpen-channel
            im-median-filter
            im-median-filter-channel
	    im-convolve
            im-convolve-channel
            im-nl-means
            im-nl-means-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define (im-gaussian-blur image sigma)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-blur-channel channel width height sigma))
		 idata))))))

(define (im-gaussian-blur-channel channel width height sigma)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-smoothing channel to width height sigma)
      ((0) to)
      (else
       (error "Gaussian blur failed.")))))

(define (im-gaussian-gradient image sigma)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-gradient-channel channel width height sigma))
		 idata))))))

(define (im-gaussian-gradient-channel channel width height sigma)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-gradient channel to width height sigma)
      ((0) to)
      (else
       (error "Gaussian gradient failed.")))))

(define (im-gaussian-sharp image factor scale)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-sharp-channel channel width height factor scale))
		 idata))))))

(define (im-gaussian-sharp-channel channel width height factor scale)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-sharpening channel to width height factor scale)
      ((0) to)
      (else
       (error "Gaussian sharp failed.")))))

(define (im-sharpen image factor)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-sharpen-channel channel width height factor))
		 idata))))))

(define (im-sharpen-channel channel width height factor)
  (let ((to (im-make-channel width height)))
    (case (vigra-simple-sharpening channel to width height factor)
      ((0) to)
      (else
       (error "Sharpen failed.")))))

(define (median-obs->int obs)
  (case obs
    ((avoid) 0)
    ;; ((clip) 1) not supported for median filters
    ((repeat) 2)
    ;; vigra says 'reflect, I prefer 'mirror
    ((mirror) 3)
    ((wrap) 4)
    ((zero) 5)
    (else
     (error "Unkown out-of-bound strategy: " obs))))

(define* (im-median-filter image w-width w-height #:key (obs 'repeat))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-median-filter-channel channel width height w-width w-height
                                                   #:obs obs))
		 idata))))))

(define* (im-median-filter-channel channel width height w-width w-height
                                   #:key (obs 'repeat))
  (let ((obs (median-obs->int obs))
        (to (im-make-channel width height)))
    (case (vigra-median-filter channel to width height w-width w-height obs)
      ((0) to)
      ((1)
       (error "Window dimensions must be odd."))
      ((2)
       (error "Invalid out-of-bound strategy."))
      (else
       (error "Median filter failed.")))))

(define (convolve-obs->int obs)
  (case obs
    ((avoid) 0)
    ((clip) 1)
    ((repeat) 2)
    ;; vigra says 'reflect, I prefer 'mirror
    ((mirror) 3)
    ((wrap) 4)
    ((zero) 5)
    (else
     (error "Unkown out-of-bound strategy: " obs))))

(define* (im-convolve image kernel #:key (obs 'repeat))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (match kernel
               ((k-width k-height kernel)
                (map-proc (lambda (channel)
                            (im-convolve-channel channel width height
                                                 kernel k-width k-height
                                                 #:obs obs))
                    idata))))))))

(define* (im-convolve-channel channel width height kernel k-width k-height
                              #:key (obs 'repeat))
  (let ((obs (convolve-obs->int obs))
        (to (im-make-channel width height)))
    (case (vigra-convolve-channel channel to width height
                                  kernel k-width k-height obs)
      ((0) to)
      ((1)
       (error "Convolve failed."))
      ((2)
       (error "Kernel dimensions must be odd."))
      ((3)
       (error "Invalid out-of-bound strategy.")))))

(define* (im-nl-means image #:key
                      (policy-type 1)
                      (sigma (if (= policy-type 0) 5.0 15.0))
                      (mean-ratio (if (= policy-type 0) 0.95 5.0))
                      (variance-ratio 0.5)
                      (epsilon 1.0e-5)
                      (spatial-sigma 2.0)
                      (search-radius 3)
                      (patch-radius 1)
                      (mean-sigma 1.0)
                      (step-size 2)
                      (n-iteration 1))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let* ((map-proc (if (and (> n-chan 1)
                                     (%use-par-map)) par-map map))
                  (n-core (current-processor-count))
                  (n-thread (if (> n-chan 1)
                                (inexact->exact (floor (/ (- n-core 1) n-chan)))
                                (- n-core 1))))
             (map-proc (lambda (channel)
                         (im-nl-means-channel channel width height
                                              #:policy-type policy-type
                                              #:sigma sigma
                                              #:mean-ratio mean-ratio
                                              #:variance-ratio variance-ratio
                                              #:epsilon epsilon
                                              #:spatial-sigma spatial-sigma
                                              #:search-radius search-radius
                                              #:patch-radius patch-radius
                                              #:mean-sigma mean-sigma
                                              #:step-size step-size
                                              #:n-iteration n-iteration
                                              #:n-thread n-thread))
                 idata))))))

(define* (im-nl-means-channel channel width height #:key
                              (policy-type 1)
                              (sigma (if (= policy-type 0) 5.0 15.0))
                              (mean-ratio (if (= policy-type 0) 0.95 5.0))
                              (variance-ratio 0.5)
                              (epsilon 1.0e-5)
                              (spatial-sigma 2.0)
                              (search-radius 3)
                              (patch-radius 1)
                              (mean-sigma 1.0)
                              (step-size 2)
                              (n-iteration 1)
                              (n-thread (- (current-processor-count) 1)))
  (let ((to (im-make-channel width height)))
    (case (vigra-nl-means-channel channel to width height
                                  policy-type
                                  sigma
                                  mean-ratio
                                  variance-ratio
                                  epsilon
                                  spatial-sigma
                                  search-radius
                                  patch-radius
                                  mean-sigma
                                  step-size
                                  n-iteration
                                  n-thread
                                  #f) ;; verbose
      ((0) to)
      ((1)
       (error "Nl-means failed."))
      ((2)
       (error "Policy must be 0 or 1.")))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-gaussian-smoothing from to width height sigma)
  (vigra_gaussian_smoothing (bytevector->pointer from)
			    (bytevector->pointer to)
			    width
			    height
			    sigma))

(define (vigra-gaussian-gradient from to width height sigma)
  (vigra_gaussian_gradient (bytevector->pointer from)
			   (bytevector->pointer to)
			   width
			   height
			   sigma))

(define (vigra-gaussian-sharpening from to width height factor scale)
  (vigra_gaussian_sharpening (bytevector->pointer from)
                             (bytevector->pointer to)
                             width
                             height
                             factor
                             scale))

(define (vigra-simple-sharpening from to width height factor)
  (vigra_simple_sharpening (bytevector->pointer from)
                           (bytevector->pointer to)
                           width
                           height
                           factor))

(define (vigra-median-filter from to width height w-width w-height obs)
  (vigra_median_filter (bytevector->pointer from)
                       (bytevector->pointer to)
                       width
                       height
                       w-width
                       w-height
                       obs))

(define (vigra-convolve-channel from to width height kernel k-width k-height obs)
  (vigra_convolve_channel (bytevector->pointer from)
                          (bytevector->pointer kernel)
                          (bytevector->pointer to)
                          width
                          height
                          k-width
                          k-height
                          obs))

(define (vigra-nl-means-channel from to width height
                                policy-type
                                sigma
                                mean-ratio
                                variance-ratio
                                epsilon
                                spatial-sigma
                                search-radius
                                patch-radius
                                mean-sigma
                                step-size
                                n-iteration
                                n-thread
                                verbose)
  (vigra_nl_means_channel (bytevector->pointer from)
                          (bytevector->pointer to)
                          width
                          height
                          policy-type
                          sigma
                          mean-ratio
                          variance-ratio
                          epsilon
                          spatial-sigma
                          search-radius
                          patch-radius
                          mean-sigma
                          step-size
                          n-iteration
                          n-thread
                          (if verbose 1 0)))


;;;
;;; Vigra_c bindings
;;;

(define vigra_gaussian_smoothing
  (pointer->procedure int
		      (dynamic-func "vigra_gaussiansmoothing_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    float))) ;; sigma

(define vigra_gaussian_gradient
  (pointer->procedure int
		      (dynamic-func "vigra_gaussiangradientmagnitude_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    float))) ;; sigma

(define vigra_gaussian_sharpening
  (pointer->procedure int
		      (dynamic-func "vigra_gaussiansharpening_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float    ;; factor
			    float))) ;; scale

(define vigra_simple_sharpening
  (pointer->procedure int
		      (dynamic-func "vigra_simplesharpening_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float))) ;; factor

(define vigra_median_filter
  (pointer->procedure int
		      (dynamic-func "vigra_medianfilter_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            int      ;; window width
                            int      ;; window height
                            int)))   ;; border treatment

(define vigra_convolve_channel
  (pointer->procedure int
		      (dynamic-func "vigra_convolveimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
                            '*       ;; kernel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            int	     ;; kernel width
			    int      ;; kernel height
                            int)))   ;; border treatment

(define vigra_nl_means_channel
    (pointer->procedure int
		      (dynamic-func "vigra_nonlocalmean_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            int	     ;; policy type
                            float    ;; sigma
                            float    ;; mean ratio
                            float    ;; variance ratio
                            float    ;; epsilon
                            float    ;; spatial sigma
                            int      ;; search radius
                            int      ;; patch radius
                            float    ;; mean sigma
                            int      ;; step size
                            int      ;; iterations
                            int      ;; n_thread
                            int)))   ;; verbose (a C bool)
