;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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


(define-module (cv support f32vector)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module (cv init)
  #:use-module (cv support utils)
  #:use-module (cv support float)
  #:use-module (cv support libguile-cv)

  #:export (;; Using libguile-cv
            f32vector-min
	    f32vector-max
            f32vector-range
            f32vector-scrap
            f32vector-scrap-in-place
            f32vector-threshold
            u8vector-threshold
            f32vector-fill-holes
            f32vector-rgb-to-gray
            f32vector-add-value
            f32vector-add-vectors
            f32vector-subtract-value
            f32vector-subtract-vectors
            f32vector-times-value
            f32vector-times-vectors
            f32vector-mtimes
            f32vector-divide-value
            f32vector-divide-vectors
            f32vector-invert
            f32vector-and-vectors
            f32vector-or-vectors
            f32vector-xor-vectors
            f32vector-transpose
            f32vector-=-vectors?
            f32vector-binary-vectors?
            f32vector-is-a-seed?
            f32vector-scale
            f32vector->s32vector
            f32vector-delineate

            ;; Pure scheme code
            f32vector-display
            f32vector-reduce
            f32vector-mean
            f32vector-std-dev
	    f32vector-copy
	    f32vector-complement
            f32vector-ref-at-offset
	    f32vector-and-at-offset
	    f32vector-or-at-offset
            f32vector-xor-at-offset
            f32vector-reduce-at-offset
	    f32vector-sum-at-offset
	    f32vector-mean-at-offset
	    f32vector=-at-offset?
	    f32vector-list=?
	    f32vector=?
	    f32vector-pred-at-offset?
	    f32vector-index
	    f32vector-count-distinct))


;;;
;;; Using libguile-cv
;;;

(define* (f32vector-min v #:key (n-cell #f))
  (let ((n-cell (if n-cell n-cell (f32vector-length v)))
        (r (make-f32vector 2 0.0)))
    (f32vector_min (bytevector->pointer v)
                   n-cell
                   (bytevector->pointer r))
    (values (f32vector-ref r 0)
            (float->int (f32vector-ref r 1)))))

(define* (f32vector-max v #:key (n-cell #f))
  (let ((n-cell (if n-cell n-cell (f32vector-length v)))
        (r (make-f32vector 2 0.0)))
    (f32vector_max (bytevector->pointer v)
                   n-cell
                   (bytevector->pointer r))
    (values (f32vector-ref r 0)
            (float->int (f32vector-ref r 1)))))

(define* (f32vector-range v #:key (n-cell #f))
  (let ((n-cell (if n-cell n-cell (f32vector-length v)))
        (r (make-f32vector 4 0.0)))
    (f32vector_range (bytevector->pointer v)
                     n-cell
                     (bytevector->pointer r))
    (values (list (f32vector-ref r 0)
                  (float->int (f32vector-ref r 1))
                  (f32vector-ref r 2)
                  (float->int (f32vector-ref r 3)))
            r)))

(define (f32vector-scrap channel l-channel n-cell scrap-cache to)
  (f32vector_scrap (bytevector->pointer channel)
                   (bytevector->pointer l-channel)
                   n-cell
                   (bytevector->pointer scrap-cache)
                   (bytevector->pointer to)))

(define (f32vector-scrap-in-place channel l-channel n-cell scrap-cache)
  (f32vector_scrap_in_place (bytevector->pointer channel)
                            (bytevector->pointer l-channel)
                            n-cell
                            (bytevector->pointer scrap-cache)))

(define (f32vector-threshold to n-cell channels threshold bg)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_threshold (bytevector->pointer to)
                           n-cell
                           (bytevector->pointer v-ptr)
                           n-chan
                           threshold
                           bg)
      to)))

(define (u8vector-threshold to n-cell channels threshold bg)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
                channels
                (iota n-chan))
      (u8vector_threshold (bytevector->pointer to)
                          n-cell
                          (bytevector->pointer v-ptr)
                          n-chan
                          threshold
                          bg)
      to)))

(define (f32vector-fill-holes l-channel n-cell bg-label)
  (f32vector_fill_holes (bytevector->pointer l-channel)
                        n-cell
                        bg-label))

(define (f32vector-rgb-to-gray to n-cell r g b)
  (f32vector_rgb_to_gray (bytevector->pointer to)
                         n-cell
                         (bytevector->pointer r)
                         (bytevector->pointer g)
                         (bytevector->pointer b)))

(define (get-v-ptr-maker-setter)
  (let ((p-size (pointer_address_size)))
    (case p-size
      ((32)
       (values make-s32vector
               s32vector-set!))
      ((64)
       (values make-s64vector
               s64vector-set!))
      (else
       (error "Unkown pointer address size:" p-size)))))

(define* (f32vector-add-value channel val to #:key (n-cell #f))
  (let ((n-cell (or n-cell (f32vector-length channel))))
    (f32vector_add_value (bytevector->pointer channel)
                         n-cell
                         val
                         (bytevector->pointer to))
    to))

(define (f32vector-add-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_add_vectors (bytevector->pointer to)
                             n-cell
                             (bytevector->pointer v-ptr)
                             n-chan)
      to)))

(define* (f32vector-subtract-value channel val to #:key (n-cell #f))
  (let ((n-cell (or n-cell (f32vector-length channel))))
    (f32vector_subtract_value (bytevector->pointer channel)
                              n-cell
                              val
                              (bytevector->pointer to))
    to))

(define (f32vector-subtract-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_subtract_vectors (bytevector->pointer to)
                                  n-cell
                                  (bytevector->pointer v-ptr)
                                  n-chan)
      to)))

(define* (f32vector-times-value channel val to #:key (n-cell #f))
  (let ((n-cell (or n-cell (f32vector-length channel))))
    (f32vector_times_value (bytevector->pointer channel)
                           n-cell
                           val
                           (bytevector->pointer to))
    to))

(define (f32vector-times-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_times_vectors (bytevector->pointer to)
                               n-cell
                               (bytevector->pointer v-ptr)
                               n-chan)
      to)))

(define (f32vector-mtimes v1 width-1 height-1 v2 width-2 to)
  (f32vector_mtimes (bytevector->pointer v1)
                    width-1
                    height-1
                    (bytevector->pointer v2)
                    width-2
                    (bytevector->pointer to))
  to)

(define* (f32vector-divide-value channel val to #:key (n-cell #f))
  (if (= val 0.0)
      (error "Attempt to divide by 0")
      (let ((n-cell (or n-cell (f32vector-length channel))))
        (f32vector_divide_value (bytevector->pointer channel)
                                n-cell
                                val
                                (bytevector->pointer to))
        to)))

(define (f32vector-divide-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_divide_vectors (bytevector->pointer to)
                                n-cell
                                (bytevector->pointer v-ptr)
                                n-chan)
      to)))

(define* (f32vector-invert channel to #:key (n-cell #f))
  (let ((n-cell (f32vector-length channel)))
    (f32vector_invert  (bytevector->pointer channel)
                       n-cell
                       (bytevector->pointer to))
    to))

(define (f32vector-and-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_and_vectors (bytevector->pointer to)
                             n-cell
                             (bytevector->pointer v-ptr)
                             n-chan)
      to)))

(define (f32vector-or-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_or_vectors (bytevector->pointer to)
                            n-cell
                            (bytevector->pointer v-ptr)
                            n-chan)
      to)))

(define (f32vector-xor-vectors to n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (f32vector_xor_vectors (bytevector->pointer to)
                             n-cell
                             (bytevector->pointer v-ptr)
                             n-chan)
      to)))

(define (f32vector-transpose channel width height to)
  (let ((n-cell (f32vector-length channel)))
    (f32vector_transpose (bytevector->pointer channel)
                         width
                         height
                         (bytevector->pointer to))
    to))

(define* (f32vector-=-vectors? n-cell channels #:key (prec 1.0e-4))
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (= (f32vector_equal_vectors n-cell
                                  (bytevector->pointer v-ptr)
                                  n-chan
                                  prec)
         0))))

(define (f32vector-binary-vectors? n-cell channels)
  (receive (maker setter!)
      (get-v-ptr-maker-setter)
    (let* ((n-chan (length channels))
           (v-ptr (maker n-chan 0)))
      (for-each (lambda (chan i)
                  (setter! v-ptr i
                           (pointer-address (bytevector->pointer chan))))
          channels
        (iota n-chan))
      (= (f32vector_binary_vectors n-cell
                                   (bytevector->pointer v-ptr)
                                   n-chan)
         0))))

(define (f32vector-is-a-seed? i-chan n-cell s-chan)
  (= (f32vector_is_a_seed (bytevector->pointer i-chan)
                          n-cell
                          (bytevector->pointer s-chan))
     0))

(define* (f32vector-scale channel n-max to #:key (n-cell #f) (p-max 255))
  (let ((n-cell (or n-cell (f32vector-length channel))))
    (f32vector_scale (bytevector->pointer channel)
                     n-cell
                     p-max
                     n-max
                     (bytevector->pointer to))
    to))

(define* (f32vector->s32vector v #:key (n-cell #f))
  (let* ((n-cell (or n-cell (f32vector-length v)))
	 (to (make-s32vector n-cell)))
    (f32vector_to_s32vector (bytevector->pointer v)
                            n-cell
                            (bytevector->pointer to))
    to))

(define* (f32vector-delineate chan chan-min chan-max to
                              #:key (n-cell #f) (threshold 10))
  (let ((n-cell (or n-cell (f32vector-length chan))))
    (f32vector_delineate (bytevector->pointer chan)
                         (bytevector->pointer chan-min)
                         (bytevector->pointer chan-max)
                         n-cell
                         threshold
                         (bytevector->pointer to))
    to))


;;;
;;; Pure scheme code
;;;

(define* (f32vector-display v #:key (proc #f) (n-cell #f)
                            (port (current-output-port)))
  (let ((proc (if proc
                  proc
                  (lambda (val)
                    (if (float>=? val 1000.0 0)
                        (format #f "~9e" val)
                        (format #f "~9,5,,,f" val)))))
        (n-cell (or n-cell
                    (f32vector-length v))))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell))
      (format port "  ~A~%" (proc (f32vector-ref v i))))))

(define* (f32vector-reduce v proc default #:key (n-cell #f))
  (let ((n-cell (or n-cell
                    (f32vector-length v))))
    (if (= n-cell 0)
        default
        (do ((i 1
                (+ i 1))
             (prev (f32vector-ref v 0)
                   (proc (f32vector-ref v i) prev)))
            ((= i n-cell) prev)))))

(define* (f32vector-mean v #:key (n-cell #f))
  (let ((n-cell (f32vector-length v)))
    (/ (f32vector-reduce v + 0 #:n-cell n-cell) n-cell)))

(define* (f32vector-std-dev v #:key (n-cell #f) (mean #f))
  (let* ((n-cell (or n-cell (f32vector-length v)))
         (mean (or mean (f32vector-mean v #:n-cell n-cell)))
         (deviations 0.0))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell)
         (sqrt (/ deviations n-cell)))
      (set! deviations
            (+ deviations (expt (- (f32vector-ref v i) mean) 2))))))

#!
;; nice but too slow
(define (f32vector-copy from)
  (let* ((n-cell (f32vector-length from))
	 (copy (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) copy)
      (f32vector-set! copy i (f32vector-ref from i)))))
!#

(define* (f32vector-copy from #:key (n-cell #f))
  (let* ((n-cell (or n-cell (f32vector-length from)))
	 (to (make-f32vector n-cell)))
    (case (vigra-copy-float-array from to n-cell)
      ((0) to)
      (else
       (error "Vector copy failed.")))))

(define (vigra-copy-float-array from to size)
  (vigra_copy_float_array (bytevector->pointer from)
                          (bytevector->pointer to)
                          size))

(define* (f32vector-complement vector #:key (of 255.0))
  (let* ((n-cell (f32vector-length vector))
	 (copy (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) copy)
      (f32vector-set! copy i
		      (- of (f32vector-ref vector i))))))

(define (f32vector-ref-at-offset vectors i)
  (map (lambda (vector)
         (f32vector-ref vector i))
    vectors))

(define (f32vector-and-at-offset vectors i)
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 0
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (when (= (f32vector-ref (list-ref vectors j) i)
               0.0)
	(set! result #f)))))

(define* (f32vector-or-at-offset vectors i
				 #:key (prec 1.0e-4))
  (float>? (f32vector-sum-at-offset vectors i) 0.0 prec))

(define (pixel-logior-1 vals result)
  (if (null? vals)
      result
      (let ((a result)
            (b (car vals)))
        (pixel-logior-1 (cdr vals)
                        (logior (logand a (- 255 b))
                                (logand (- 255 a) b))))))

(define (pixel-logior vals)
  (match vals
    ((a) a)
    ((a . rests)
     (pixel-logior-1 rests a))
    (else
     (error "Invalid argument:" vals))))

(define (f32vector-xor-at-offset vectors i)
  (pixel-logior (map (lambda (vector)
                       (float->int (f32vector-ref vector i)))
                  vectors)))

(define (f32vector-reduce-at-offset vectors proc default i)
  (if (null? vectors)
      default
      (let ((n-vec (length vectors))
            (result (f32vector-ref (list-ref vectors 0) i)))
        (do ((j 1
                (+ j 1)))
            ((= j n-vec) result)
          (set! result
                (proc result (f32vector-ref (list-ref vectors j) i)))))))

(define (f32vector-sum-at-offset vectors i)
  (f32vector-reduce-at-offset vectors + 0.0 i))

(define (f32vector-mean-at-offset vectors i)
  (let ((n-vec (length vectors)))
    (/ (f32vector-sum-at-offset vectors i)
       n-vec)))

(define* (f32vector=-at-offset? vectors i
				#:key (prec 1.0e-4))
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  ;;   vectors length > 0
  ;;   i is a valid indice
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 1 ;; we compare all with vec 0, see below
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (float=? (f32vector-ref (list-ref vectors j) i)
		       (f32vector-ref (list-ref vectors 0) i)
		       prec)
	(set! result #f)))))

(define* (f32vector-list=? vectors
			   #:key (prec 1.0e-4))
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  (match vectors
    ((vector . rest)
     (let ((n-cell (f32vector-length vector))
	   (result #t))
       (case n-cell
	 ((0) #t)
	 (else
	  (do ((i 0
		  (+ i 1)))
	      ((or (= i n-cell)
		   (not result)) result)
	    (unless (f32vector=-at-offset? vectors i #:prec prec)
	      (set! result #f)))))))
    ((vector) #t)
    (() #t)))

(define (f32vector=? . vectors)
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  (match vectors
    ((precision . rest)
     (if (number? precision)
	 (f32vector-list=? rest #:prec precision)
	 (f32vector-list=? vectors)))
    ((vector) #t)
    (() #t)))

;; the version below is 58% faster. we keep this as an example and for
;; the record.
#;(define* (f32vector-pred-at-offset? pred vectors i)
  (let ((n-vec (length vectors)))
    (catch 'exit
      (lambda ()
	(do ((j 0
		(+ j 1)))
	    ((= j n-vec) #t)
	  (unless (pred (f32vector-ref (list-ref vectors j) i))
	    (throw 'exit #f))))
      (lambda (key value)
	value))))

(define* (f32vector-pred-at-offset? pred vectors i)
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 0
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (pred (f32vector-ref (list-ref vectors j) i))
	(set! result #f)))))

(define (f32vector-index pred . vectors)
  (let ((n-cell (apply min (map f32vector-length vectors)))
	(result #f))
    (do ((i 0
	    (+ i 1)))
	((or (= i n-cell)
	     result) result)
      (when (f32vector-pred-at-offset? pred vectors i)
	(set! result i)))))

(define* (f32vector-count-distinct v #:key (prec 1.0e-4))
  (let ((n-cell (f32vector-length v)))
    (case n-cell
      ((0) (values 0 '()))
      ((1) (values 1 (list (f32vector-ref v 0))))
      (else
       (let ((vals (list (f32vector-ref v 0))))
	 (do ((i 1
		 (+ i 1)))
	     ((= i n-cell) (values (length vals) vals))
	   (let ((val (f32vector-ref v i)))
	     (unless (float-member val vals prec)
	       (set! vals
		     (cons val vals))))))))))


;;;
;;; Matrix ops
;;;

#;(define (f32vector-mtimes v1 width-1 height-1 v2 width-2 to)
  ;; In math, we'd write:
  ;; 	A[n, m] and B[m, p]
  ;;	n = the number of lines of Ab
  ;;	m = the number of columns of A
  ;;	    the number of lines of B
  ;;	p = the number of columns of B
  ;; In guile-cv, an image is represented as a list
  ;;	(width height n-chan idata)
  ;; So:
  ;;	n = height-1
  ;;	m = width-1
  ;;	p = width-2
  ;; Here is a 'naive' implementation, till we bind a fast linear algebra
  ;; library, gsl or cblas, but this is a rabbit hole task!
  (let* ((a v1)
         (b v2)
         (n height-1)
         (m width-1)
         (p width-2)
         #;(n-cell (* n p))
	 #;(ab (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n) to)
      (do ((j 0
              (+ j 1)))
          ((= j p))
        (do ((sub 0)
             (k 0
                (+ k 1)))
            ((= k m)
             (f32vector-set! to (+ (* i p) j) sub))
          (set! sub
                (+ sub (* (f32vector-ref a (+ (* i m) k))
                          (f32vector-ref b (+ (* k p) j))))))))))

#;(define (f32vector-invert v)
  (let* ((n-cell (f32vector-length v))
	 (to (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (f32vector-set! to i
                      (expt (f32vector-ref v i) -1)))))

;; in libguile-cv
#;(define* (f32vector->s32vector v  v #:key (n-cell #f))
  (let* ((n-cell (or n-cell (f32vector-length v)))
	 (to (make-s32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (s32vector-set! to i
                      (float->int (f32vector-ref v i))))))


;;;
;;; Vigra_c bindings
;;;

(define vigra_copy_float_array
  (pointer->procedure int
		      (dynamic-func "vigra_copy_float_array_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int)))	;; size


;;;
;;; In C - kept for Guile-3.0
;;;

#!

;; ok for small images, but too slow otherwse so, till Guile-3.0, I have
;; to do this in C instead which is fine anyway, because memory is
;; allocated on the scheme side.

(define* (f32vector-min v #:key (n-cell #f))
  (let ((n-cell (if n-cell n-cell (f32vector-length v))))
    (case n-cell
      ((0)
       (error "Empty vector: " v))
      (else
       (do ((mini (f32vector-ref v 0))
            (p-mini 0)
            (i 1
               (+ i 1)))
           ((= i n-cell)
            (values mini p-mini))
         (let ((val (f32vector-ref v i)))
           (when (< val mini)
             (set! mini val)
             (set! p-mini i))))))))

(define* (f32vector-max v #:key (n-cell #f))
  (let ((n-cell (if n-cell n-cell (f32vector-length v))))
    (case n-cell
      ((0)
       (error "Empty vector: " v))
      (else
       (do ((maxi (f32vector-ref v 0))
            (p-maxi 0)
            (i 1
               (+ i 1)))
           ((= i n-cell)
            (values maxi p-maxi))
         (let ((val (f32vector-ref v i)))
           (when (> val maxi)
             (set! maxi val)
             (set! p-maxi i))))))))

(define* (f32vector-range v #:key (n-cell #f))
  (let ((n-cell (if n-cell n-cell (f32vector-length v))))
    (case n-cell
      ((0)
       (error "Empty vector: " v))
      (else
       (do ((mini (f32vector-ref v 0))
            (maxi (f32vector-ref v 0))
            (p-mini 0)
            (p-maxi 0)
            (i 1
               (+ i 1)))
           ((= i n-cell)
            (list mini p-mini maxi p-maxi))
         (let ((val (f32vector-ref v i)))
           (when (< val mini)
             (set! mini val)
             (set! p-mini i))
           (when (> val maxi)
             (set! maxi val)
             (set! p-maxi i))))))))

!#
