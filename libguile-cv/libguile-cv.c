/*  -*- mode: C; coding: utf-8 -*-

####
#### Copyright (C) 2016 - 2019
#### Free Software Foundation, Inc.

#### This file is part of GNU Guile-CV.

#### GNU Guile-CV is free software; you can redistribute it and/or
#### modify it under the terms of the GNU General Public License as
#### published by the Free Software Foundation; either version 3 of the
#### License, or (at your option) any later version.

#### GNU Guile-CV is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with GNU Guile-CV.  If not, see
#### <https://www.gnu.org/licenses/gpl.html>.
####

*/

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
#include <float.h>
#include <math.h>
/* #include <libguile.h> */


/*
 * misc.
 *
*/

size_t pointer_address_size ()
{
  size_t n = sizeof(float *) * CHAR_BIT;

  return n;
}

int im_fast_channel_offset (int i,
                            int j,
                            int width)
{
  return (i * width) + j;
}


/*
 * limits
 *
*/

float float_min ()
{
  return (FLT_MIN);
}

float float_max ()
{
  return (FLT_MAX);
}


/*
 * bounding box
 *
*/

int point_inside (int left,
                  int top,
                  int right,
                  int bottom,
                  int pt_x,
                  int pt_y)
{
  if ((pt_x >= left) &&
      (pt_x <= right) &&
      (pt_y >= top) &&
      (pt_y <= bottom)) {
    return 1;
  }
  else {
    return 0;
  }
}

int bb_intersect (int l_one,
                  int t_one,
                  int r_one,
                  int b_one,
                  int l_two,
                  int t_two,
                  int r_two,
                  int b_two)
{
  if (((point_inside (l_one, t_one, r_one, b_one, l_two, t_two)) == 1) ||
      ((point_inside (l_one, t_one, r_one, b_one, r_two, t_two)) == 1) ||
      ((point_inside (l_one, t_one, r_one, b_one, l_two, b_two)) == 1) ||
      ((point_inside (l_one, t_one, r_one, b_one, r_two, b_two)) == 1)) {
    return 1;
  }
  else {
    return 0;
  }
}


/*
 * floats
 *
*/

int float_to_int (float f)
{
  int i;

  i = (int)f;
  return (i);
}

int float_equal (float f1, float f2, float prec)
{
  if ((abs (f1 - f2)) <= prec) {
    return 1;
  }
  else {
    return 0;
  }
}


/*
 * s32vectors
 *
*/

int s32_ref (int *chan,
             int i,
             int j,
             int width)
{
  int offset;

  offset = im_fast_channel_offset (i, j, width);
  return chan[offset];
}

int s32_set (int *chan,
             int i,
             int j,
             int width,
             int val)
{
  int offset;

  offset = im_fast_channel_offset (i, j, width);
  chan[offset] = val;
  return 0;
}


/*
 * f32vectors
 *
*/

int f32vector_min (float *v, int n_cell, float *r)
{
  int i;

  r[0] = v[0];
  r[1] = 0.0;
  for (i = 1; i < n_cell; i++) {
    if (v[i] < r[0]) {
        r[0] = v[i];
        r[1] = (float)i;
    }
  }
  return 0;
}

int f32vector_max (float *v, int n_cell, float *r)
{
  int i;

  r[0] = v[0];
  r[1] = 0.0;
  for (i = 1; i < n_cell; i++) {
    if (v[i] > r[0]) {
      r[0] = v[i];
      r[1] = (float)i;
    }
  }
  return 0;
}

int f32vector_range (float *v, int n_cell, float *r)
{
  int i;

  r[0] = r[2] = v[0];
  r[1] = r[3] = 0.0;
  for (i = 1; i < n_cell; i++) {
    if (v[i] < r[0]) {
      r[0] = v[i];
      r[1] = (float)i;
    }
    if (v[i] > r[2]) {
      r[2] = v[i];
      r[3] = (float)i;
    }
  }
  return 0;
}

int f32vector_scrap (float *chan,
                     float *l_chan,
                     int n_cell,
                     int *scrap_cache,
                     float *to)
{
  int i, val;

  for (i = 0; i < n_cell; i++) {
    val = (int)l_chan[i];
    if ((val == 0) | (scrap_cache[val] == 1)) {
      to[i] = 0;
    }
    else {
      to[i] = chan[i];
    }
  }
  return 0;
}

int f32vector_scrap_in_place (float *chan,
                              float *l_chan,
                              int n_cell,
                              int *scrap_cache)
{
  int i, val;

  for (i = 0; i < n_cell; i++) {
    val = (int)l_chan[i];
    if ((val == 0) | (scrap_cache[val] == 1)) {
      chan[i] = 0.0;
    }
  }
  return 0;
}

int f32vector_threshold (float *to,
                         int n_cell,
                         float *v_ptr[],
                         int n_vectors,
                         float threshold,
                         int bg)
{
  int i, j;
  float sum;

  for (i = 0; i < n_cell; i++) {
    sum = 0;
    for (j = 0; j < n_vectors; j++) {
      sum += v_ptr[j][i];
    }
    sum = sum / n_vectors;
    if (bg == 0) {
      if (sum >= threshold) {
        to[i] = 255.0;
      }
      else {
        to[i] = 0.0;
      }
    }
    else {
      if (sum <= threshold) {
        to[i] = 255.0;
      }
      else {
        to[i] = 0.0;
      }
    }
  }
  return 0;
}

int rgb_u8vector_threshold (uint8_t *to,
                            int n_cell,
                            uint8_t *v_ptr[],
                            int n_vectors,
                            uint8_t threshold,
                            int bg)
{
  int i, j;
  int sum;
  int rgb_i;

  for (i = 0; i < n_cell; i++) {
    sum = 0;
    rgb_i = 3 * i;
    for (j = 0; j < n_vectors; j++) {
        sum += v_ptr[j][rgb_i];
        sum += v_ptr[j][rgb_i+1];
        sum += v_ptr[j][rgb_i+2];
    }
    sum = (int)round((float)sum / (3 * n_vectors));
    if (bg == 0) {
      if (sum >= threshold) {
        to[i] = 255;
      }
      else {
        to[i] = 0;
      }
    }
    else {
      if (sum <= threshold) {
        to[i] = 255;
      }
      else {
        to[i] = 0;
      }
    }
  }
  return 0;
}

int u8vector_threshold (uint8_t *to,
                        int n_cell,
                        uint8_t *v_ptr[],
                        int n_vectors,
                        uint8_t threshold,
                        int bg)
{
  int i, j;
  int sum;

  for (i = 0; i < n_cell; i++) {
    sum = 0;
    for (j = 0; j < n_vectors; j++) {
      sum += v_ptr[j][i];
    }
    sum = (int)round((float)sum / n_vectors);
    if (bg == 0) {
      if (sum >= threshold) {
        to[i] = 255;
      }
      else {
        to[i] = 0;
      }
    }
    else {
      if (sum <= threshold) {
        to[i] = 255;
      }
      else {
        to[i] = 0;
      }
    }
  }
  return 0;
}

int f32vector_fill_holes (float *labels,
                          int n_cell,
                          float bg_label)
{
  int i;

  for (i = 0; i < n_cell; i++) {
    if (labels[i] == bg_label) {
      labels[i] = 0.0;
    }
    else {
      labels[i] = 255.0;
    }
  }
  return 0;
}

int f32vector_rgb_to_gray (float *to,
                           int n_cell,
                           float *r,
                           float *g,
                           float *b)
{
  int i;

  for (i = 0; i < n_cell; i++) {
    to[i] = (r[i] + g[i] + b[i]) / 3;
  }
  return 0;
}

int f32vector_add_value (float *v,
                         int n_cell,
                         float val,
                         float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = v[i] + val;
  }
  return 0;
}

int f32vector_add_vectors (float *to,
                           int n_cell,
                           float *v_ptr[],
                           int n_vectors)
{
  int i, j;
  float sum;

  for (i = 0; i < n_cell; i++) {
    sum = 0;
    for (j = 0; j < n_vectors; j++) {
      sum += v_ptr[j][i];
    }
    to[i] = sum;
  }
  return 0;
}

int f32vector_subtract_value (float *v,
                              int n_cell,
                              float val,
                              float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = v[i] - val;
  }
  return 0;
}

int f32vector_subtract_vectors (float *to,
                                int n_cell,
                                float *v_ptr[],
                                int n_vectors)
{
  int i, j;
  float result;

  for (i = 0; i < n_cell; i++) {
    result = v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      result -= v_ptr[j][i];
    }
    to[i] = result;
  }
  return 0;
}

int f32vector_times_value (float *v,
                           int n_cell,
                           float val,
                           float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = v[i] * val;
  }
  return 0;
}

int f32vector_times_vectors (float *to,
                             int n_cell,
                             float *v_ptr[],
                             int n_vectors)
{
  int i, j;
  float result;

  for (i = 0; i < n_cell; i++) {
    result = v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      result *= v_ptr[j][i];
    }
    to[i] = result;
  }
  return 0;
}

int f32vector_mtimes (float *v1,
                      int w1,
                      int h1,
                      float *v2,
                      int w2,
                      float *to)
/*
  In math, we'd write:
	A[n, m] and B[m, p]
	n = the number of lines of Ab
	m = the number of columns of A
	    the number of lines of B
	p = the number of columns of B
  In guile-cv, an image is represented as a list
	(width height n-chan idata)
  So:
	n = h1
	m = w1
	p = w2
  Here is a 'naive' implementation, till we bind a fast linear algebra
  library, gsl or cblas, but this is a rabbit hole task!
*/
{
  int i, j, k, n, m, p;
  float sub, *a, *b;

  a = v1;
  b = v2;

  n = h1;
  m = w1;
  p = w2;

  for (i = 0; i < n; i++) {
    for (j = 0; j < p; j++) {
      sub = 0.0;
      for (k = 0; k < m; k++) {
        sub = sub + a[((i * m) + k)] * b[((k * p) + j)];
      }
      to[((i * p) + j)] = sub;
    }
  }
  return 0;
}

int f32vector_divide_value (float *v,
                            int n_cell,
                            float val,
                            float *to)
{
  int i;

  if (val == 0) {
    printf ("ERROR: Attempt to divide by 0");
    return -1;
  }
  else {
    for (i = 0; i < n_cell; i++) {
      to[i] =  v[i] / val;
    }
    return 0;
  }
}

int f32vector_divide_vectors (float *to,
                              int n_cell,
                              float *v_ptr[],
                              int n_vectors)
{
  int i, j;
  float result;

  for (i = 0; i < n_cell; i++) {
    result = v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      result /= v_ptr[j][i];
    }
    to[i] = result;
  }
  return 0;
}

int f32vector_invert (float *v,
                      int n_cell,
                      float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = 1 / v[i];
  }
  return 0;
}

int f32vector_and_vectors (float *to,
                           int n_cell,
                           float *v_ptr[],
                           int n_vectors)
{
  int i, j, bool;

  for (i = 0; i < n_cell; i++) {
    bool = 1;
    for (j = 0; j < n_vectors; j++) {
      if (v_ptr[j][i] == 0) {
        bool = 0;
      }
    }
    if (bool == 1) {
      to[i] = v_ptr[0][i];
    }
    else {
      to[i] = 0;
    }
  }
  return 0;
}

int f32vector_or_vectors (float *to,
                          int n_cell,
                          float *v_ptr[],
                          int n_vectors)
{
  int i, j;
  float val;

  for (i = 0; i < n_cell; i++) {
    val = 0;
    for (j = 0; j < n_vectors; j++) {
      if ((val == 0) && (v_ptr[j][i] > 0)) {
        val = v_ptr[j][i];
        }
    }
    to[i] = val;
  }
  return 0;
}

int f32vector_xor_vectors (float *to,
                           int n_cell,
                           float *v_ptr[],
                           int n_vectors)
{
  int i, j, a, b;

  for (i = 0; i < n_cell; i++) {
    a = (int)v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      b = (int)v_ptr[j][i];
      a = (a & (255 - b) ) | ((255 - a) & b);
    }
    to[i] = a;
  }
  return 0;
}

int f32vector_transpose (float *chan,
                         int width,
                         int height,
                         float *to)
{
  int i, j, d, o;

  for (i = 0; i < height; i++) {
    for (j = 0; j < width; j++) {
      d = im_fast_channel_offset (j, i, height);
      o = im_fast_channel_offset (i, j, width);
      to[d] = chan[o];
    }
  }
  return 0;
}

int f32vector_equal_vectors (int n_cell,
                             float *v_ptr[],
                             int n_vectors,
                             float prec)
{
  int i, j;
  float val;

  for (i = 0; i < n_cell; i++) {
    val =  v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      if (prec == 0) {
        if (v_ptr[j][i] != val) {
          return -1;
        }
      }
      else {
        if (float_equal (v_ptr[j][i], val, prec) == 0) {
          return -1;
        }
      }
    }
  }
  return 0;
}

int f32vector_binary_vectors (int n_cell,
                              float *v_ptr[],
                              int n_vectors)
{
  int i, j;

  for (i = 0; i < n_cell; i++) {
    for (j = 0; j < n_vectors; j++) {
      if ((v_ptr[j][i] == 0) || (v_ptr[j][i] == 255)) {
      }
      else {
        return -1;
      }
    }
  }
  return 0;
}

int f32vector_is_a_seed (float *i_chan,
                         int n_cell,
                         float *s_chan)
{
  int i, j;

  for (i = 0; i < n_cell; i++) {
    if ((i_chan[i] == 255.0) && (s_chan[i] == 255.0)) {
      return 0;
      }
  }
  return -1;
}

int f32vector_scale (float *v,
                     int n_cell,
                     float p_max,
                     float n_max,
                     float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
    to[i] = round ((v[i] / p_max) * n_max);
  }
  return 0;
}

int f32vector_to_s32vector (float *v,
                            int n_cell,
                            int *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
    to[i] = (int)v[i];
  }
  return 0;
}

int f32vector_delineate (float *v,
                         float *v_min,
                         float *v_max,
                         int n_cell,
                         int threshold,
                         float *to)
{
  int i;
  float ori, mini, maxi, diff;

  for (i = 0; i < n_cell; i++) {
    ori = v[i];
    mini = v_min[i];
    maxi = v_max[i];
    diff = maxi - mini;
    if (diff < threshold) {
      to[i] = ori;
    }
    else {
      if ((ori - mini) < (maxi - ori)) {
        to[i] = mini;
      }
      else {
        to[i] = maxi;
      }
    }
  }
  return 0;
}


/*
 * glcm
 *
*/

int glcm_c (int *chan,
            int width,
            int height,
            int *g0,
            int *g45,
            int *g90,
            int *g135,
            int n_gl,
            int dist)
{
  int i, j, row, col_g0, col_g45, col_g90, col_g135;

  for (i = 0; i < height; i++) {
    for (j = 0; j < width; j++) {
      row = s32_ref (chan , i, j, width);
      // g0
      if (j < ( width - dist)) {
        col_g0 = s32_ref (chan, i, j + dist, width);
        s32_set (g0, row, col_g0, n_gl, s32_ref (g0, row, col_g0, n_gl) + 1);
        if (i > (dist - 1)) {
          // g45
          col_g45 = s32_ref (chan, i - dist, j + dist, width);
          s32_set (g45, row, col_g45, n_gl, s32_ref (g45, row, col_g45, n_gl) + 1);
        }
      }
      if (i > (dist - 1)) {
        // g90
        col_g90 = s32_ref (chan, i - dist, j, width);
        s32_set (g90, row, col_g90, n_gl, s32_ref (g90, row, col_g90, n_gl) + 1);
        if (j > (dist - 1)) {
          // g135
          col_g135 = s32_ref (chan, i - dist, j - dist, width);
          s32_set (g135, row, col_g135, n_gl, s32_ref (g135, row, col_g135, n_gl) + 1);
        }
      }
    }
  }
  return 0;
}
