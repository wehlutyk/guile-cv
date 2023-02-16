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


(define-module (cv compose)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv process)
  #:use-module (cv transform)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-compose))


;;;
;;; Compose
;;;

(define* (im-compose position alignment #:key (color '(0 0 0)) . images)
  ;; ... when #:key is used together with a rest argument, the keyword
  ;; parameters in a call all remain in the rest list. This is the same
  ;; as Common Lisp ...
  ;; =>
  ;; so let's make sure images does not include the kw
  (let ((images (match images
                  ((#:color val . rest) rest)
                  (else
                   images))))
    (match images
      ((image) image)
      ((image . rest)
       (match image
         ((_ _ n-chan _)
          (if (apply = (apply im-collect 'n-channel images))
              (let* ((map-proc (if (and (> n-chan 1)
                                        (%use-par-map)) par-map map))
                     (composed-channels
                      (map-proc (lambda (chaval)
                                  (match chaval
                                    ((channels value)
                                     (im-compose-channels position
                                                          alignment
                                                          channels
                                                          (apply im-collect 'width images)
                                                          (apply im-collect 'height images)
                                                          #:value value))))
                          (zip (apply zip (apply im-collect 'channels images))
                               (if (> n-chan 1)
                                   color
                                   (list (/ (reduce + 0 color) 3)))))))
                (match (car composed-channels)
                  ((width height _)
                   (list width height n-chan
                         (map (lambda (item)
                                (match item
                                  ((_ _ c-chan) c-chan)))
                           composed-channels)))))
              (error "Channel number mismatch")))))

      (()
       (error "The list of images to compose can't be empty")))))

(define* (im-compose-channels position alignment channels widths heights
                              #:key (value 0.0))
  (case position
    ((above)
     (im-compose-channels-below alignment
                                (reverse! channels)
                                (reverse! widths)
                                (reverse! heights)
                                #:value value))
    ((below)
     (im-compose-channels-below alignment channels widths heights
                                #:value value))
    ((left)
     (im-compose-channels-right alignment
                                (reverse! channels)
                                (reverse! widths)
                                (reverse! heights)
                                #:value value))
    ((right)
     (im-compose-channels-right alignment channels widths heights
                                #:value value))
    (else
     (error "No such compose position: " position))))

(define* (im-compose-channels-below alignment channels widths heights
                                    #:key (value 0.0))
  (let* ((max-width (apply max widths))
         (total-height (reduce + 0 heights))
         (to (im-make-channel max-width total-height)))
    (match (fold (lambda (c w h prev)
                   (match prev
                     ((start . to)
                      (do ((ac (if (= w max-width)
                                   c
                                   (im-adjust-channel-width alignment c w h max-width
                                                            #:value value)))
                           (n-cell (* max-width h))
                           (i 0
                              (+ i 1)))
                          ((= i n-cell)
                           (cons (+ start n-cell) to))
                        (f32vector-set! to (+ start i) (f32vector-ref ac i))))))
                 `(0 . ,to)
                 channels
                 widths
                 heights)
      ((start . to)
       (list max-width total-height to)))))

(define* (im-adjust-channel-width alignment channel width height to-width
                                  #:key (value 0.0))
  (if (= width to-width)
      channel
      (receive (total left right)
          (im-adjust-channel-padd width to-width)
        (if (> left 0)
            (case alignment
              ((center)
               (im-padd-channel channel width height left 0 right 0 #:value value))
              ((left)
               (im-padd-channel channel width height 0 0 total 0 #:value value))
              ((right)
               (im-padd-channel channel width height total 0 0 0 #:value value)))
            (im-crop-channel channel width height (abs left) 0 (abs right) 0)))))

(define* (im-compose-channels-right alignment channels widths heights
                                    #:key (value 0.0))
  (let* ((total-width (reduce + 0 widths))
         (max-height (apply max heights))
         (to (im-make-channel total-width max-height)))
    (match (fold (lambda (c w h prev)
                   (match prev
                     ((start . to)
                      (do ((ac (if (= h max-height)
                                   c
                                   (im-adjust-channel-height alignment c w h max-height
                                                             #:value value)))
                           (n-cell (* w max-height))
                           (i 0
                              (+ i 1)))
                          ((= i max-height)
                           (cons (+ start w) to))
                        (do ((j 0
                                (+ j 1)))
                            ((= j w))
                          (f32vector-set! to
                                          (+ (* i total-width) j start)
                                          (f32vector-ref ac (+ (* i w) j))))))))
                 `(0 . ,to)
                 channels
                 widths
                 heights)
      ((start . to)
       (list total-width max-height to)))))

(define* (im-adjust-channel-height alignment channel width height to-height
                            #:key (value 0.0))
  (receive (total top bottom)
      (im-adjust-channel-padd height to-height)
    (if (> top 0)
        (case alignment
          ((center)
           (im-padd-channel channel width height 0 top 0 bottom #:value value))
          ((top)
           (im-padd-channel channel width height 0 0 0 total #:value value))
          ((bottom)
           (im-padd-channel channel width height 0 total 0 0 #:value value)))
        (im-crop-channel channel width height 0 (abs top) 0 (abs bottom)))))

(define (im-adjust-channel-padd from to)
  (let* ((diff (- to from))
         (padd (/ diff 2)))
    (if (even? diff)
        (values diff padd padd)
        (values diff
                (float->int (ceiling padd))
                (float->int (floor padd))))))
