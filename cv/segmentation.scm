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


(define-module (cv segmentation)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-label
	    im-label-channel
	    im-label-all
	    im-label-all-channel
        im-label-all-u8channel->u32channel
            im-canny
            im-canny-channel
            im-crack-edge
            im-crack-edge-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-label image #:key (con 8) (bg 'black))
  ;; (im-binary? image) is rather expensive
  (match image
    ((width height n-chan idata)
     ;; so we only check for n-chan
     (case n-chan
       ((1)
	(match idata
	  ((c)
	   (receive (l-channel n-label)
	       (im-label-channel c width height #:con con #:bg bg)
	     (values (list width height 1 (list l-channel))
                     n-label)))))
       (else
	(error "Not a binary image."))))))

(define* (im-label-channel channel width height #:key (con 8) (bg 'black))
  (let* ((to (im-make-channel width height))
	 (n-object (vigra-label channel to width height con bg)))
    (case n-object
      ((-1)
       (error "Label failed."))
      (else
       (values to
               ;; vigra returns the highest label value, which
               ;; correspond to the number of object _but_ 0 is a label,
               ;; so n-label is (+ n-object 1)
               (+ n-object 1))))))

(define* (im-label-all image #:key (con 8) (to #f))
  ;; (im-binary? image) is rather expensive so we only check for n-chan
  (match image
    ((width height n-chan idata)
     (case n-chan
       ((1)
	(match idata
	  ((c)
	   (receive (l-channel n-label)
	       (im-label-all-channel c width height #:con con #:to to)
	     (values (list width height 1 (list l-channel))
		     n-label)))))
	       (else
	(error "Not a binary image."))))))

(define* (im-label-all-channel channel width height #:key (con 8) (to #f))
  (let* ((to (or to (im-make-channel width height)))
	 (n-object (vigra-label-all channel to width height con)))
    (case n-object
      ((-1)
       (error "Label failed."))
      (else
       (values to
               ;; vigra returns the highest label value, which
               ;; correspond to the number of object _but_ 0 is a label,
               ;; so n-label is (+ n-object 1)
               (+ n-object 1)
               ;; n-object
               )))))

(define* (im-label-all-u8channel->u32channel channel width height #:key (con 8) (to #f) (to-tmp #f))
  (let* ((size (* width height))
         (to (or to (make-u32vector size 0)))
         (to-tmp (or to-tmp (make-u32vector size 0)))
	     (n-object (vigra-uint8-uint32tmp-uint32-label-all channel to-tmp to
                                                           width height con)))
    (case n-object
      ((-1)
       (error "Label failed."))
      (else
       (values to
               ;; vigra returns the highest label value, which
               ;; correspond to the number of object _but_ 0 is a label,
               ;; so n-label is (+ n-object 1)
               (+ n-object 1)
               ;; n-object
               )))))

(define* (im-canny image
                   #:key (sigma 1.0) (threshold 0.0) (marker 255.0))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-canny-channel channel width height
                                           #:sigma sigma
                                           #:threshold threshold
                                           #:marker marker))
                 idata))))))

(define* (im-canny-channel channel width height
                           #:key (sigma 1.0) (threshold 0.0) (marker 255.0))
  (let ((to (im-make-channel width height)))
    (case (vigra-canny-edge-channel channel to width height sigma threshold marker)
      ((0) to)
      (else
       (error "Canny failed.")))))

(define* (im-crack-edge image #:key (marker 255.0))
  (match image
    ((width height n-chan idata)
     (list (- (* 2 width) 1) (- (* 2 height) 1) n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-crack-edge-channel channel width height #:marker marker))
                 idata))))))

(define* (im-crack-edge-channel channel width height #:key (marker 255.0))
  (let ((to (im-make-channel (- (* 2 width) 1) (- (* 2 height) 1))))
    (case (vigra-crack-edge-channel channel to width height marker)
      ((0) to)
      (else
       (error "Crack edge.")))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-label from to width height con bg)
  (vigra_label (bytevector->pointer from)
	       (bytevector->pointer to)
	       width
	       height
	       (case con
		 ((8) 1)
		 ((4) 0)
		 (else
		  (error "No such connectivity: " con)))
	       (case bg
		 ((black) 0.0)
		 ((white) 255.0)
		 (else
		  (error "No such background: " bg)))))

(define (vigra-label-all from to width height con)
  (vigra_label_all (bytevector->pointer from)
		   (bytevector->pointer to)
		   width
		   height
		   (case con
		     ((8) 1)
		     ((4) 0)
		     (else
		      (error "No such connectivity: " con)))))

(define (vigra-int-label-all from to width height con)
  (vigra_intlabel_all (bytevector->pointer from)
		              (bytevector->pointer to)
		              width
		              height
		              (case con
		                ((8) 1)
		                ((4) 0)
		                (else
		                 (error "No such connectivity: " con)))))

(define (vigra-uint8-uint32tmp-uint32-label-all from to-tmp to width height con)
  (vigra_uint8_uint32tmp_uint32labelimage (bytevector->pointer from)
                                          (bytevector->pointer to-tmp)
		                                  (bytevector->pointer to)
		                                  width
		                                  height
		                                  (case con
		                                    ((8) 1)
		                                    ((4) 0)
		                                    (else
		                                     (error "No such connectivity: " con)))))

(define (vigra-canny-edge-channel from to width height sigma threshold marker)
  (vigra_canny_edge_channel (bytevector->pointer from)
                            (bytevector->pointer to)
                            width
                            height
                            sigma
                            threshold
                            marker))

(define (vigra-crack-edge-channel from to width height marker)
  (vigra_region_image_to_crack_edge_image (bytevector->pointer from)
                                          (bytevector->pointer to)
                                          width
                                          height
                                          marker))


;;;
;;; Vigra_c bindings
;;;

(define vigra_label
  (pointer->procedure int
		      (dynamic-func "vigra_labelimagewithbackground_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    int	     ;; 8_con?
			    float))) ;; background

(define vigra_label_all
  (pointer->procedure int
		      (dynamic-func "vigra_labelimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    int)))   ;; 8_con?

(define vigra_uint8_uint32tmp_uint32labelimage
  (pointer->procedure int
		              (dynamic-func "vigra_uint8_uint32tmp_uint32labelimage_c"
				                    %libvigra-c)
		              (list '*	     ;; from channel, u8vector
                            '*	     ;; to-tmp channel, u32vector
			                '*	     ;; to channel, u32vector
			                int	     ;; width
			                int	     ;; height
			                int)))

(define vigra_canny_edge_channel
  (pointer->procedure int
		      (dynamic-func "vigra_cannyedgeimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float    ;; scale
                            float    ;; gradient_threshold
			    float))) ;; edge_marker

(define vigra_region_image_to_crack_edge_image
  (pointer->procedure int
		      (dynamic-func "vigra_regionimagetocrackedgeimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float))) ;; marker
