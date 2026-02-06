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


(define-module (cv features)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv segmentation)
  #:use-module (srfi srfi-11)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-features
            im-area-box-gray!
            %vigra-area-box-length-gray
	    im-features-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-features image l-image #:key (n-label #f))
  (match l-image
    ((_ _ _ l-idata)
     (match l-idata
       ((l-c)
        (let ((n-label (or n-label
                           (+ (float->int (f32vector-max l-c)) 1))))
          (match image
            ((width height n-chan idata)
             (match idata
               ((c)
                (im-features-gray c l-c width height #:n-label n-label))
               ((r g b)
                (im-features-rgb r g b l-c width height #:n-label n-label))
               (else
                (error "Not a GRAY neither an RGB image.")))))))
       (else
        (error "Not a labeled image."))))))

(define %vigra-area-box-length-gray 5)

(define* (im-area-box-gray! image l-image features #:key (n-label #f))
  (match l-image
    ((_ _ _ l-idata)
     (match l-idata
       ((l-c)
        (let ((n-label (or n-label
                           (+ (float->int (f32vector-max l-c)) 1))))
          (match image
            ((width height n-chan idata)
             (match idata
               ((c)
                (let ((result (vigra-extract-area-box-gray
                               c l-c features width height n-label)))
                  (case result
                    ((1) (error "Features failed."))
                    (else result))              ))
               (else
                (error "Not a GRAY image.")))))))
       (else
        (error "Not a labeled image."))))))

(define %vigra-feature-length-gray 22)

(define* (im-features-gray channel l-c width height #:key (n-label #f))
  (let* ((n-label (or n-label
		      (+ (float->int (f32vector-max l-c)) 1)))
	 (features (im-make-channel %vigra-feature-length-gray n-label))
	 (result (vigra-extract-features-gray channel l-c
                                              features width height n-label)))
    (case result
      ((1)
       (error "Features failed."))
      (else
       (vigra-features->list features n-label 'gray)))))

(define %vigra-feature-length-rgb 34)

(define* (im-features-rgb r g b l-c width height #:key (n-label #f))
  (let* ((n-label (or n-label
		      (+ (float->int (f32vector-max l-c)) 1)))
	 (features (im-make-channel %vigra-feature-length-rgb n-label))
	 (result (vigra-extract-features-rgb r g b l-c
                                             features width height n-label)))
    (case result
      ((1)
       (error "Features failed."))
      (else
       (vigra-features->list features n-label 'rgb)))))

(define (vigra-features->list features n-feature im-type)
  ;;
  ;; Notes:
  ;;  - n-feature is n-label, since vigra returns one feature for
  ;;    the background as well
  ;;  - features is a 'channel' (f32vector), see below for a full
  ;;    description of feature 'entries'
  ;;
  (do ((proc (case im-type
               ((gray) vigra-feature-gray)
               ((rgb) vigra-feature-rgb)))
       (result '())
       (i 0
          (+ i 1)))
      ((= i n-feature)
       (reverse! result))
    (set! result
          (cons (proc features i) result))))

(define (eigen-vigra-coord->trigono-coord x y)
  ;; vigra returns eigen major and minor vector coordinates in the image
  ;; coordinate system (Y axis points down), but we prefer to return
  ;; these values using the trigonometric model (Y axis points up).  To
  ;; do this, we actually just have to change the sign, here are the
  ;; possibilities

  ;;      |             Y                     X   Y   Angle
  ;;      |           . |             Vigra   1   1    45
  ;;      |             |             Trigo  -1   1   135
  ;;  ----|---> X   ----|---> X
  ;;      | .           |
  ;;      |             |
  ;;      Y             |

  ;;      |             Y                     X   Y   Angle
  ;;      |             | .           Vigra  -1   1    135
  ;;      |             |             Trigo   1   1     45
  ;;  ----|---> X   ----|---> X
  ;;    . |             |
  ;;      |             |
  ;;      Y             |

  ;;      |             Y                     X   Y   Angle
  ;;    . |             |             Vigra  -1  -1    -135
  ;;      |             |             Trigo   1  -1     -45
  ;;  ----|---> X   ----|---> X
  ;;      |             | .
  ;;      |             |
  ;;      Y             |

  ;;      |             Y                     X   Y   Angle
  ;;      | .           |             Vigra   1  -1    -45
  ;;      |             |             Trigo  -1  -1   -135
  ;;  ----|---> X   ----|---> X
  ;;      |           . |
  ;;      |             |
  ;;      Y             |

  (cond ((and (positive? x) (positive? y))
         (values (- x) y))
        ((and (negative? x) (positive? y))
         (values (- x) y))
        ((and (negative? x) (negative? y))
         (values (- x) y))
        ((and (positive? x) (negative? y))
         (values (- x) y))
        (else
         ;; some tests images would call this procedure with either x
         ;; and/or y equal to 0, in which case we just return 'as is',
         ;; they won't be used anyway and we don't want to raise an
         ;; exception.
         (values x y))))

(define (ellipse-axis-optimization-factor a b area)
  (sqrt (/ area (* %pi a b))))

(define (vigra-feature-gray features i)
  ;;
  ;;  Gray object's offset and feature names are:
  ;;
  ;;  | Index         | Feature                       |
  ;;  | ------------- | ----------------------------- |
  ;;  |  0            | region_size                   |
  ;;  |  1,  2        | upperleft-x, y-coord          |
  ;;  |  3,  4        | lowerright-x, y-coord         |
  ;;  |  5,  6        | mean-x and y-coord            |
  ;;  |  7            | min gray value                |
  ;;  |  8            | max gray value                |
  ;;  |  9            | mean gray value               |
  ;;  | 10            | std. dev. gray value          |
  ;;  | 11, 12        | major-ev-x, major-ev-y        |
  ;;  | 13, 14        | minor-ev-x, minor-ev-y        |
  ;;  | 15            | major axis                    |
  ;;  | 16            | minor axis                    |
  ;;  | 17, 18        | center of mass x, y           |
  ;;  | 19            | perimeter                     |
  ;;  | 20            | skewness                      |
  ;;  | 21            | kurtosis                      |
  ;;
  (let* ((p-size %vigra-feature-length-gray)
         (offset (* i p-size)))
    (match (map (lambda (k)
                  (f32vector-ref features (+ offset k)))
             (iota p-size))
      ((area
        left top right bottom
        mean-x mean-y
        mini maxi meani
        std-dev
        major-ev-x major-ev-y
        minor-ev-x minor-ev-y
        major-axis minor-axis
        center-mass-x center-mass-y
        perimeter
        skewness
        kurtosis)
       (let* ((axis-optimization-factor
               (ellipse-axis-optimization-factor major-axis minor-axis area))
              (semi-major-axis (* major-axis axis-optimization-factor))
              (semi-minor-axis (* minor-axis axis-optimization-factor)))
         (receive (major-ev-trigo-x major-ev-trigo-y)
             (eigen-vigra-coord->trigono-coord major-ev-x major-ev-y)
           (receive (minor-ev-trigo-x minor-ev-trigo-y)
               (eigen-vigra-coord->trigono-coord minor-ev-x minor-ev-y)
             (list (float->int area)
                   (float->int left)
                   (float->int top)
                   (float->int right)
                   (float->int bottom)
                   mean-x mean-y
                   mini maxi meani
                   std-dev
                   major-ev-trigo-x major-ev-trigo-y
                   minor-ev-trigo-x minor-ev-trigo-y
                   semi-major-axis
                   semi-minor-axis
                   (radian->degree (atan major-ev-trigo-y major-ev-trigo-x))
                   center-mass-x center-mass-y
                   perimeter
                   skewness
                   kurtosis
                   ;; circularity
                   (/ (* 4 %pi area) (expt perimeter 2))
                   ;; aspect ratio
                   (/ semi-major-axis semi-minor-axis)
                   ;; roundness
                   ;; (/ (* 4 area) (* %pi (expt major-axis 2)))
                   (/ semi-minor-axis semi-major-axis)))))))))

(define %f-display-gray-format-str
  "\n                     area : ~A (pixels)
    left top right bottom : ~A ~A ~A ~A
            mean-x mean-y : ~9,5,,,f ~9,5,,,f
             min max mean : ~9,5,,,f ~9,5,,,f ~9,5,,,f
       standard deviation : ~9,5,,,f
            major ev x, y : ~9,5,,,f ~9,5,,,f
            minor ev x, y : ~9,5,,,f ~9,5,,,f
        major, minor axis : ~9,5,,,f ~9,5,,,f (radius)
                    angle : ~9,5,,,f (degrees)
      center of mass x, y : ~9,5,,,f ~9,5,,,f
                perimeter : ~9,5,,,f
                 skewness : ~9,5,,,f
                 kurtosis : ~9,5,,,f
              circularity : ~9,5,,,f
             aspect ratio : ~9,5,,,f
                roundness : ~9,5,,,f\n\n")

(define %f-display-rgb-format-str
  "\n                          area : ~A (pixels)
         left top right bottom : ~A ~A ~A ~A
                 mean-x mean-y : ~9,5,,,f ~9,5,,,f
        min (red, green, blue) : ~9,5,,,f ~9,5,,,f ~9,5,,,f
        max (red, green, blue) : ~9,5,,,f ~9,5,,,f ~9,5,,,f
       mean (red, green, blue) : ~9,5,,,f ~9,5,,,f ~9,5,,,f
  std. dev. (red, green, blue) : ~9,5,,,f ~9,5,,,f ~9,5,,,f
                 major ev x, y : ~9,5,,,f ~9,5,,,f
                 minor ev x, y : ~9,5,,,f ~9,5,,,f
             major, minor axis : ~9,5,,,f ~9,5,,,f (radius)
                         angle : ~9,5,,,f (degrees)
           center of mass x, y : ~9,5,,,f ~9,5,,,f
                     perimeter : ~9,5,,,f
   skewness (red, green, blue) : ~9,5,,,f ~9,5,,,f ~9,5,,,f
   kurtosis (red, green, blue) : ~9,5,,,f ~9,5,,,f ~9,5,,,f
                   circularity : ~9,5,,,f
                  aspect ratio : ~9,5,,,f
                     roundness : ~9,5,,,f\n\n")

(define* (f-display vals #:key (port (current-output-port)))
  (case (length vals)
    ((26) ;; gray particle
     (format port "~?" %f-display-gray-format-str vals))
    ((38) ;; rgb particle
     (format port "~?" %f-display-rgb-format-str vals)))
  (values))

(define (vigra-feature-rgb features i)
  ;;
  ;;  RGB object's offset and feature names are:
  ;;
  ;;  | Index         | Feature                       |
  ;;  | ------------- | ----------------------------- |
  ;;  |  0            | region_size                   |
  ;;  |  1,  2        | upperleft-x and y-coord       |
  ;;  |  3,  4        | lowerright-x and y-coord      |
  ;;  |  5,  6        | mean-x and y-coord            |
  ;;  |  7,  8,  9    | min red, green, blue          |
  ;;  | 10, 11, 12    | max red, green, blue          |
  ;;  | 13, 14, 15    | mean red, green, blue         |
  ;;  | 16, 17, 18    | std.dev. red, green, blue     |
  ;;  | 19, 20        | major-ev-x, major-ev-y        |
  ;;  | 21, 22        | minor-ev-x, minor-ev-y        |
  ;;  | 23            | major axis                    |
  ;;  | 24            | minor axis                    |
  ;;  | 25, 26        | center of mass x, y           |
  ;;  | 27            | perimeter                     |
  ;;  | 28, 29, 30    | skewness red, green, blue     |
  ;;  | 31, 23, 33    | kurtosis red, green, blue     |
  ;;
  (let* ((p-size %vigra-feature-length-rgb)
         (offset (* i p-size)))
    (match (map (lambda (k)
                  (f32vector-ref features (+ offset k)))
             (iota p-size))
      ((area
        left top right bottom
        mean-x mean-y
        min-r min-g min-b
        max-r max-g max-b
        mean-r mean-g mean-b
        std-dev-r std-dev-g std-dev-b
        major-ev-x major-ev-y
        minor-ev-x minor-ev-y
        major-axis
        minor-axis
        center-mass-x center-mass-y
        perimeter
        skewness-r skewness-g skewness-b
        kurtosis-r kurtosis-g kurtosis-b)
       (let* ((axis-optimization-factor
               (ellipse-axis-optimization-factor major-axis minor-axis area))
              (semi-major-axis (* major-axis axis-optimization-factor))
              (semi-minor-axis (* minor-axis axis-optimization-factor)))
         (receive (major-ev-trigo-x major-ev-trigo-y)
             (eigen-vigra-coord->trigono-coord major-ev-x major-ev-y)
           (receive (minor-ev-trigo-x minor-ev-trigo-y)
               (eigen-vigra-coord->trigono-coord minor-ev-x minor-ev-y)
             (list (float->int area)
                   (float->int left)
                   (float->int top)
                   (float->int right)
                   (float->int bottom)
                   mean-x mean-y
                   min-r min-g min-b
                   max-r max-g max-b
                   mean-r mean-g mean-b
                   std-dev-r std-dev-g std-dev-b
                   major-ev-trigo-x major-ev-trigo-y
                   minor-ev-trigo-x minor-ev-trigo-y
                   semi-major-axis
                   semi-minor-axis
                   (radian->degree (atan major-ev-trigo-y major-ev-trigo-x))
                   center-mass-x center-mass-y
                   perimeter
                   skewness-r skewness-g skewness-b
                   kurtosis-r kurtosis-g kurtosis-b
                   ;; circularity
                   (/ (* 4 %pi area) (expt perimeter 2))
                   ;; aspect ratio
                   (/ major-axis minor-axis)
                   ;; roundness
                   ;; (/ (* 4 area) (* %pi (expt major-axis 2)))
                   (/ minor-axis major-axis)))))))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-extract-area-box-gray from labels results width height n-label)
  (vigra_extractareabox_gray (bytevector->pointer from)
                             (bytevector->pointer labels)
                             (bytevector->pointer results)
                             width
                             height
                             (- n-label 1)))

(define (vigra-extract-features-gray from labels results width height n-label)
  (vigra_extractfeatures_gray (bytevector->pointer from)
                              (bytevector->pointer labels)
                              (bytevector->pointer results)
                              width
                              height
                              (- n-label 1))) ;; vigra wants n-object

(define (vigra-extract-features-rgb r g b labels results width height n-label)
  (vigra_extractfeatures_rgb (bytevector->pointer r)
                             (bytevector->pointer g)
                             (bytevector->pointer b)
                             (bytevector->pointer labels)
                             (bytevector->pointer results)
                             width
                             height
                             (- n-label 1))) ;; vigra wants n-object


;;;
;;; Vigra_c bindings
;;;

(define vigra_extractareabox_gray
  (pointer->procedure int
		      (dynamic-func "vigra_extractareabox_gray_c"
				    %libvigra-c)
		      (list '*     ;; from channel
			    '*     ;; labels channel
			    '*     ;; results vector
			    int    ;; width
			    int    ;; height
			    int)))

(define vigra_extractfeatures_gray
  (pointer->procedure int
		      (dynamic-func "vigra_extractfeatures_gray_c"
				    %libvigra-c)
		      (list '*     ;; from channel
			    '*     ;; labels channel
			    '*     ;; results vector
			    int    ;; width
			    int    ;; height
			    int))) ;; n_object

(define vigra_extractfeatures_rgb
  (pointer->procedure int
		      (dynamic-func "vigra_extractfeatures_rgb_c"
				    %libvigra-c)
		      (list '*     ;; red channel
                            '*     ;; green channel
                            '*     ;; blue channel
			    '*     ;; labels channel
			    '*     ;; results vector
			    int    ;; width
			    int    ;; height
			    int))) ;; n_object
