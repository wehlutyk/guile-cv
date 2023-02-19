;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2023
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


(define-module (cv particles)
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
  #:use-module (cv segmentation)
  #:use-module (cv process)
  #:use-module (cv transform)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-particles
            im-particle-clean))


#;(g-export )


;;;
;;; Guile-CV additional API
;;;

(define %scrap-cache
  (@@ (cv process) scrap-cache))

(define* (im-particles image features #:key (clean #t))
  (let* ((map-proc (if (%use-par-map) par-map map))
         (particles (map-proc
                        (lambda (prop)
                          (match prop
                            ((area left top right bottom . rest)
                             (parameterize ((%use-par-map #f))
                               (let ((particle (im-crop image
                                                        left top (+ right 1) (+ bottom 1))))
                                 (list (if clean
                                           (im-particle-clean particle)
                                           particle)
                                       (list left top right bottom)))))))
                        (cdr features))))
    (values (map car particles)
            (map cadr particles))))

(define* (im-particle-clean particle #:key (binary? #t))
  (let* ((p-bin (if binary? particle (im-threshold particle 1.0)))
         (p-clean (match p-bin
                    ((width height n-chan idata)
                     (receive (p-label n-label)
                         (im-label p-bin)
                       (let* ((n-object (- n-label 1))
                              (r (- width 1))
                              (b (- height 1))
                              (p-bin-chan (im-channel p-bin 0))
                              (p-label-chan (im-channel p-label 0))
                              (n-cell (* width height))
                              (to-scrap (fold (lambda (prop i prev)
                                                (match prop
                                                  ((_ left top right bottom . rest)
                                                   (if (or (not (= left 0))
                                                           (not (= top 0))
                                                           (not (= right r))
                                                           (not (= bottom b)))
                                                       ;; we did skip the bg, so i needs 1+
                                                       (cons (+ i 1) prev)
                                                       prev))))
                                              '()
                                              (cdr (im-features p-bin p-label
                                                                #:n-label n-label))
                                              (iota n-object))))
                         (list width height n-chan
                               (list (match to-scrap
                                       (() p-bin-chan)
                                       (else ;; [1]
                                        (f32vector-scrap-in-place p-bin-chan
                                                                  p-label-chan
                                                                  n-cell
                                                                  (%scrap-cache to-scrap n-label))
                                        p-bin-chan))))))))))
    (if binary?
        p-clean
        (im-and particle p-clean))))

#!

;; [1]

(let ((cache (%scrap-cache to-scrap n-label)))
  (do ((i 0
          (+ i 1)))
      ((= i n-cell) p-bin-chan)
    (let ((val
           (float->int (f32vector-ref p-label-chan i))))
      ;; (when (vector-ref cache val)
      (unless (= (s32vector-ref cache val) 0)
        (f32vector-set! p-bin-chan i 0.0)))))

!#
