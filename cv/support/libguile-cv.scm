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


(define-module (cv support libguile-cv)
  #:use-module (system foreign)
  #:use-module (cv init)

  #:export (;; misc.
            pointer_address_size

            ;; limits
            float_min
            float_max

            ;; bounding box
            bb_intersect

            ;; floats
            float_to_int
            float_equal

            ;; f32vectors
            f32vector_min
            f32vector_max
            f32vector_range
            f32vector_scrap
            f32vector_scrap_in_place
            f32vector_threshold
            f32vector_fill_holes
            f32vector_rgb_to_gray
            f32vector_add_value
            f32vector_add_vectors
            f32vector_subtract_value
            f32vector_subtract_vectors
            f32vector_times_value
            f32vector_times_vectors
            f32vector_mtimes
            f32vector_divide_value
            f32vector_divide_vectors
            f32vector_invert
            f32vector_and_vectors
            f32vector_or_vectors
            f32vector_xor_vectors
            f32vector_transpose
            f32vector_equal_vectors
            f32vector_binary_vectors
            f32vector_is_a_seed
            f32vector_scale
            f32vector_to_s32vector
            f32vector_delineate

            ;; u8vectors
            u8vector_threshold
            rgb_u8vector_threshold

            ;; glcm
            glcm_c))


;;;
;;; misc.
;;;

(define pointer_address_size
  (pointer->procedure size_t
                      (dynamic-func "pointer_address_size"
                                    %libguile-cv)
                      (list)))


;;;
;;; limits
;;;

(define float_min
  (pointer->procedure float
                      (dynamic-func "float_min"
                                    %libguile-cv)
                      (list)))

(define float_max
  (pointer->procedure float
                      (dynamic-func "float_max"
                                    %libguile-cv)
                      (list)))


;;;
;;; bouding box
;;;

(define bb_intersect
  (pointer->procedure int
                      (dynamic-func "bb_intersect"
                                    %libguile-cv)
                      (list int int int int int int int int)))


;;;
;;; floats
;;;

(define float_to_int
  (pointer->procedure int
                      (dynamic-func "float_to_int"
                                    %libguile-cv)
                      (list float)))

(define float_equal
  (pointer->procedure int
                      (dynamic-func "float_equal"
                                    %libguile-cv)
                      (list float	;; f1
                            float	;; f2
                            float)))	;; precision


;;;
;;; f32vectors
;;;

(define f32vector_min
  (pointer->procedure int
                      (dynamic-func "f32vector_min"
                                    %libguile-cv)
                      (list '*		;; channel
                            int		;; n_cell
                            '*)))	;; result vector

(define f32vector_max
  (pointer->procedure int
                      (dynamic-func "f32vector_max"
                                    %libguile-cv)
                      (list '*		;; channel
                            int		;; n_cell
                            '*)))	;; result vector

(define f32vector_range
  (pointer->procedure int
                      (dynamic-func "f32vector_range"
                                    %libguile-cv)
                      (list '*		;; channel
                            int		;; n_cell
                            '*)))	;; result vector

(define f32vector_scrap
  (pointer->procedure int
                      (dynamic-func "f32vector_scrap"
                                    %libguile-cv)
                      (list '*		;; chan
                            '*		;; l_chan
                            int		;; n_cell
                            '*		;; scrap_cache
                            '*)))	;; to

(define f32vector_scrap_in_place
  (pointer->procedure int
                      (dynamic-func "f32vector_scrap_in_place"
                                    %libguile-cv)
                      (list '*		;; chan
                            '*		;; l_chan
                            int		;; n_cell
                            '*)))	;; scrap_cache

(define f32vector_threshold
  (pointer->procedure int
                      (dynamic-func "f32vector_threshold"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int		;; n_vectors
                            float	;; threshold
                            int)))	;; bg [0 or 255]

(define f32vector_fill_holes
  (pointer->procedure int
                      (dynamic-func "f32vector_fill_holes"
                                    %libguile-cv)
                      (list '*       ;; labeled chan
                            int      ;; n_cell
                            float))) ;; bg label value

(define f32vector_rgb_to_gray
  (pointer->procedure int
                      (dynamic-func "f32vector_rgb_to_gray"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; r
                            '*		;; g
                            '*)))	;; b

(define f32vector_add_value
  (pointer->procedure int
                      (dynamic-func "f32vector_add_value"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n_cell
                            float	;; value
                            '*)))	;; to

(define f32vector_add_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_add_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_subtract_value
  (pointer->procedure int
                      (dynamic-func "f32vector_subtract_value"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n_cell
                            float	;; value
                            '*)))	;; to

(define f32vector_subtract_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_subtract_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_times_value
  (pointer->procedure int
                      (dynamic-func "f32vector_times_value"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n_cell
                            float	;; value
                            '*)))	;; to

(define f32vector_times_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_times_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_mtimes
  (pointer->procedure int
                      (dynamic-func "f32vector_mtimes"
                                    %libguile-cv)
                      (list '*		;; v1
                            int		;; width_1
                            int		;; height1
                            '*		;; v2
                            int		;; width_2
                            '*)))	;; to

(define f32vector_divide_value
  (pointer->procedure int
                      (dynamic-func "f32vector_divide_value"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n_cell
                            float	;; value
                            '*)))	;; to

(define f32vector_divide_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_divide_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_invert
  (pointer->procedure int
                      (dynamic-func "f32vector_invert"
                                    %libguile-cv)
                      (list '*		;; from
                            int		;; n_cell
                            '*)))	;; to

(define f32vector_and_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_and_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_or_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_or_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_xor_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_xor_vectors"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_transpose
  (pointer->procedure int
                      (dynamic-func "f32vector_transpose"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; width
                            int		;; height
                            '*)))	;; to

(define f32vector_equal_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_equal_vectors"
                                    %libguile-cv)
                      (list int		;; n_cell
                            '*		;; v_ptr[]
                            int		;; n_vectors
                            float)))	;; precision

(define f32vector_binary_vectors
  (pointer->procedure int
                      (dynamic-func "f32vector_binary_vectors"
                                    %libguile-cv)
                      (list int		;; n_cell
                            '*		;; v_ptr[]
                            int)))	;; n_vectors

(define f32vector_is_a_seed
  (pointer->procedure int
                      (dynamic-func "f32vector_is_a_seed"
                                    %libguile-cv)
                      (list '*		;; i_chan
                            int		;; n_cell
                            '*)))	;; s_chan

(define f32vector_scale
  (pointer->procedure int
                      (dynamic-func "f32vector_scale"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n_cell
                            float	;; o_gl
                            float	;; n_gl
                            '*)))	;; to

(define f32vector_to_s32vector
  (pointer->procedure int
                      (dynamic-func "f32vector_to_s32vector"
                                    %libguile-cv)
                      (list '*    ;; chan
                            int   ;; n_cell
                            '*))) ;; to

(define f32vector_delineate
  (pointer->procedure int
                      (dynamic-func "f32vector_delineate"
                                    %libguile-cv)
                      (list '*		;; chan
                            '*		;; chan_min
                            '*		;; chan_max
                            int		;; n_cell
                            int		;; threshold
                            '*)))	;; to


;;;
;;; u8vectors
;;;

(define rgb_u8vector_threshold
  (pointer->procedure int
                      (dynamic-func "rgb_u8vector_threshold"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int		;; n_vectors
                            int 	;; threshold
                            int)))

(define u8vector_threshold
  (pointer->procedure int
                      (dynamic-func "u8vector_threshold"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n_cell
                            '*		;; v_ptr[]
                            int		;; n_vectors
                            int 	;; threshold
                            int)))


;;;
;;; glcm
;;;

(define glcm_c
  (pointer->procedure int
                      (dynamic-func "glcm_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; width
                            int		;; height
                            '*		;; g0_chan
                            '*		;; g45_chan
                            '*		;; g90_chan
                            '*		;; g135_chan
                            int		;; n_gl
                            int)))	;; dist
