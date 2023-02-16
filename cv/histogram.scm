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


(define-module (cv histogram)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv impex)
  #:use-module (cv process)
  #:use-module (cv transform)
  #:use-module (cv compose)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-histogram))


;;;
;;; Histograms
;;;

(define %h-width 256)
(define %h-height-gray 130)
(define %h-height-rgb 80)
(define %h-padd 11)
(define %h-padd-color '(255 255 255))

(define %hl-height 14)
(define %hl-padd 2)

(define %h-total-width 282)

(define* (im-histogram image #:key (subtitle "Untitled"))
  (case (im-n-channel image)
    ((1) (im-histogram-gray image #:subtitle subtitle))
    ((3) (im-histogram-rgb image #:subtitle subtitle))
    (else
     (error "Not a GRAY not an RGB image."))))

(define* (im-histogram-gray image #:key (subtitle "Untitled"))
  (match image
    ((width height n-chan idata)
     (match idata
       ((channel)
        (let* ((map-proc (if (%use-par-map) par-map map))
               (items (map-proc
                          (lambda (i)
                            (case i
                              ((0) (list (make-histogram-title subtitle #:gray #t)
                                         (make-histogram-legend 'gray)))
                              ((1) (receive (histogram
                                             n-cell mean std-dev mini maxi mode val)
                                       (make-histogram-gray channel width height)
                                     (list histogram
                                           (list n-cell mean std-dev mini maxi mode val)
                                           (make-histogram-table-gray
                                            n-cell mean std-dev mini maxi mode val))))))
                          (iota 2))))
          (match (apply append items)
            ((h-title h-legend histogram h-vals h-table)
             (values (im-compose 'below 'center #:color %h-padd-color
                                 h-title histogram h-legend h-table)
                     h-vals)))))))))

(define* (im-histogram-rgb image #:key (subtitle "Untitled"))
  (match image
    ((width height n-chan idata)
     (let* ((h-padd %h-padd) ;; pixels
            (h-padd-color %h-padd-color)
            (hl-padd %hl-padd)
            (n-cell (* width height))
            (map-proc (if (%use-par-map) par-map map))
            (h-results
             (map-proc (lambda (channel)
                         (receive (h-chan n-cell mean std-dev mini maxi mode val)
                             (im-histogram-channel channel width height %h-height-rgb)
                           (list h-chan n-cell mean std-dev mini maxi mode val)))
                 idata))
            (h-channels (map car h-results))
            (h-vals (map cdr h-results))
            (h-values (cons n-cell (apply append (map cddr h-results))))
            (o-items (map-proc (lambda (i)
                                 (case i
                                   ((0) (make-histogram-title subtitle))
                                   ((1) (make-histogram-table h-values))))
                         (iota 2))))
       (values
        (match o-items
          ((h-title h-table)
           (match (map-proc (lambda (h-channel-c-type)
                              (match h-channel-c-type
                                ((h-channel c-type)
                                 (case c-type
                                   ((red)
                                    (im-padd (im-histogram-rgb-with-legend h-channel c-type)
                                             0 h-padd 0 h-padd #:color h-padd-color))
                                   ((green)
                                    (im-padd (im-histogram-rgb-with-legend h-channel c-type)
                                             0 0 0 h-padd #:color h-padd-color))
                                   ((blue)
                                    (im-histogram-rgb-with-legend h-channel c-type))))))
                      (zip h-channels '(red green blue)))
             ((h-red h-green h-blue)
              (im-compose 'below 'center #:color h-padd-color
                          h-title
                          h-red
                          h-green
                          h-blue
                          h-table)))))
        h-vals)))))

(define (im-histogram-rgb-with-legend h-channel c-type)
  (let* ((width %h-width)
         (height %h-height-rgb)
         (idata (list h-channel
                      (im-copy-channel h-channel width height)
                      (im-copy-channel h-channel width height)))
        (legend (make-histogram-legend c-type)))
    (im-compose 'below 'center #:color %h-padd-color
                (list width height 3 idata)
                legend)))

(define (im-histogram-channel channel width height hi-height)
  (let* ((n-gray 256)
         (n-cell (* width height))
         (h-vals (make-f32vector n-gray 0.0)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell))
      (let ((val #;(inexact->exact (f32vector-ref channel i))
             (float->int (f32vector-ref channel i))))
        (f32vector-set! h-vals val
                        (+ (f32vector-ref h-vals val) 1))))
    (let ((c-min (f32vector-min channel))
          (c-max (f32vector-max channel))
          (c-mean (float-round (f32vector-mean channel #:n-cell n-cell) 3))
          (c-std-dev (float-round (f32vector-std-dev channel #:n-cell n-cell) 3)))
      (receive (c-maxi c-mode)
          (f32vector-max h-vals)
        (values (make-histogram-channel h-vals hi-height)
                n-cell
                c-mean
                c-std-dev
                (float->int c-min)
                (float->int c-max)
                c-mode
                (float->int c-maxi))))))

(define (make-histogram-channel h-vals hi-height)
  (let* ((hi-width %h-width)
         (h-max (f32vector-max h-vals))
         (hi-max (- hi-height 1))
         (factor (/ hi-max h-max))
         (hi-chan (im-make-channel hi-width hi-height 255.0)))
    (do ((k 0
            (+ k 1)))
        ((= k 256))
      (let* ((h-val (f32vector-ref h-vals k))
             (hi-val (float->int (* h-val factor)))
             (start #;(if (= hi-val 0) hi-height (- hi-max hi-val))
              (- hi-height hi-val)))
        (do ((i start
                (+ i 1)))
            ((= i hi-height))
          (f32vector-set! hi-chan (+ (* i hi-width) k) 0.0))))
    hi-chan))

(define (histogram-item-left-right-padd h-width)
  (let* ((diff (- %h-total-width h-width))
         (l-padd (/ diff 2)))
    (if (even? diff)
        (values l-padd l-padd)
        (values (float->int (ceiling l-padd))
                (float->int (floor l-padd))))))

(define* (make-histogram-title subtitle #:key (gray #f))
  (let ((title (im-load (latex-pdftoppm
                         (latex-compile
                          (latex-write-histogram-title %latex-cache subtitle)))))
        (op (if gray im-rgb->gray identity)))
    (op (match title
          ((width height n-cha idata)
           (receive (l-padd r-padd)
               (histogram-item-left-right-padd width)
             (if (> l-padd 0)
                 (im-padd title l-padd 13 r-padd 0 #:color '(255 255 255))
                 (im-padd (im-crop title (abs l-padd) 0 (abs r-padd) 0)
                          0 13 0 0  #:color '(255 255 255)))))))))

(define* (make-histogram-gray channel width height
                              #:key (h-height %h-height-gray))
  (receive (h-chan n-cell mean std-dev mini maxi mode val)
      (im-histogram-channel channel width height %h-height-gray)
    (receive (l-padd r-padd)
        (histogram-item-left-right-padd %h-width)
      (values (im-padd (list %h-width %h-height-gray 1
                             (list h-chan))
                       l-padd 0 r-padd 0 #:color '(255 255 255))
               n-cell mean std-dev mini maxi mode val))))

(define make-histogram-legend #f)

(let ((h-legend-cache '()))
  (set! make-histogram-legend
        (lambda* (#:optional (type 'gray))
          (or (assq-ref type h-legend-cache)
              (let* ((h-padd-color %h-padd-color)
                     (header (let* ((hl-width %h-width)
                                    (hl-height %hl-height)
                                    (hl-chan (im-make-channel hl-width hl-height)))
                               (do ((k 0
                                       (+ k 1)))
                                   ((= k 256))
                                 (do ((i 0
                                         (+ i 1)))
                                     ((= i hl-height))
                                   (f32vector-set! hl-chan (+ (* i hl-width) k) k)))
                               (case type
                                 ((gray) (list 256 14 1 (list hl-chan)))
                                 (else
                                  (let ((ec1 (im-make-channel hl-width hl-height))
                                        (ec2 (im-make-channel hl-width hl-height)))
                                    (list 256 14 3
                                          (case type
                                            ((red) (list hl-chan ec1 ec2))
                                            ((green) (list ec1 hl-chan ec2))
                                            ((blue) (list ec1 ec2 hl-chan)))))))))
                     (footer (case type
                               ((gray)
                                (im-rgb->gray (make-histogram-legend-footer)))
                               (else
                                (make-histogram-legend-footer))))
                     (h-legend (im-compose 'below 'center #:color h-padd-color
                                           (im-padd header 13 2 13 2 #:color h-padd-color)
                                           footer)))
                (set! h-legend-cache
                      (assq-set! h-legend-cache type h-legend))
                h-legend)))))

(define (make-histogram-legend-footer)
  (let ((h-padd-color %h-padd-color)
        (n0 (im-load (latex-pdftoppm
                      (latex-compile
                       (latex-write-text %latex-cache "0")))))
        (n255 (im-load (latex-pdftoppm
                        (latex-compile
                         (latex-write-text %latex-cache "255"))))))
    (im-compose 'right 'top #:color h-padd-color
                (im-padd n0 5 0 229 0 #:color h-padd-color)
                n255)))

(define (make-histogram-table vals)
  (let ((table (im-load (latex-pdftoppm
                         (latex-compile
                          (latex-write-histogram-table %latex-cache vals))))))
    (match table
      ((width height n-chan idata)
       (receive (l-padd r-padd)
           (histogram-item-left-right-padd width)
         (if (> l-padd 0)
             (im-padd table l-padd 13 r-padd 0 #:color '(255 255 255))
             (im-padd (im-crop table (abs l-padd) 0 (abs r-padd) 0)
                      0 13 0 0  #:color '(255 255 255))))))))

(define (make-histogram-table-gray n-cell mean std-dev mini maxi mode val)
  (let ((table (im-load (latex-pdftoppm
                         (latex-compile
                          (latex-write-histogram-table-gray %latex-cache
                                                            n-cell mean std-dev
                                                            mini maxi mode val))))))
    (im-rgb->gray (match table
                    ((width height n-chan idata)
                     (receive (l-padd r-padd)
                         (histogram-item-left-right-padd width)
                       (if (> l-padd 0)
                           (im-padd table l-padd 13 r-padd 0 #:color '(255 255 255))
                           (im-padd (im-crop table (abs l-padd) 0 (abs r-padd) 0)
                                    0 13 0 0  #:color '(255 255 255)))))))))
