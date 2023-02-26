dnl -*- Autoconf -*-
dnl
dnl   configuration script for GNU Guile-CV
dnl   Process this file with autoconf to produce configure.
dnl

define(GUILE_CV_CONFIGURE_COPYRIGHT,[[

Copyright (C) 2016 - 2023
Free Software Foundation, Inc.

This file is part of GNU Guile-CV.

GNU Guile-CV is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

GNU Guile-CV is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Guile-CV.  If not, see
<https://www.gnu.org/licenses/gpl.html>.

]])

AC_PREREQ(2.69)

AC_INIT(
  [guile-cv],
  [0.3.1],
  [bug-guile-cv@gnu.org])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
dnl Use `serial-tests' so the output `check-guile' is not hidden
dnl (`parallel-tests' is the default in Automake 1.13.)
dnl `serial-tests' was introduced in Automake 1.12.
AM_INIT_AUTOMAKE([1.14 gnu -Wall -Werror -Wno-portability -Wno-syntax \
  color-tests serial-tests foreign])
AM_MAINTAINER_MODE([enable])
AM_SILENT_RULES([yes])

AC_PROG_CC
AC_STDC_HEADERS
LT_INIT([disable-static])

AC_COPYRIGHT(GUILE_CV_CONFIGURE_COPYRIGHT)
AC_CONFIG_SRCDIR([cv])

MAKEINFO_PROG([6.6])

AC_CHECK_PROG(PDFLATEX, pdflatex, yes)
if test -z "$PDFLATEX"; then
  AC_MSG_ERROR([pdflatex required but not found])
fi

LATEX_DOCUMENT_CLASS_REQUIRED([standalone])
LATEX_PACKAGE_REQUIRED([inputenc])
LATEX_PACKAGE_REQUIRED([fontenc])
LATEX_PACKAGE_REQUIRED([lmodern])
LATEX_PACKAGE_REQUIRED([xcolor])
LATEX_PACKAGE_REQUIRED([booktabs])
LATEX_PACKAGE_REQUIRED([siunitx])
LATEX_PACKAGE_REQUIRED([iwona])

VIGRA_LIB([1.11.1])

VIGRA_C_LIB
VIGRA_C_FUNCTION([vigra_labelimagewithbackground_c])
VIGRA_C_FUNCTION([vigra_labelimage_c])
VIGRA_C_FUNCTION([vigra_cannyedgeimage_c])
VIGRA_C_FUNCTION([vigra_regionimagetocrackedgeimage_c])
VIGRA_C_FUNCTION([vigra_gaussiansmoothing_c])
VIGRA_C_FUNCTION([vigra_gaussiangradientmagnitude_c])
VIGRA_C_FUNCTION([vigra_gaussiansharpening_c])
VIGRA_C_FUNCTION([vigra_simplesharpening_c])
VIGRA_C_FUNCTION([vigra_medianfilter_c])
VIGRA_C_FUNCTION([vigra_convolveimage_c])
VIGRA_C_FUNCTION([vigra_nonlocalmean_c])
VIGRA_C_FUNCTION([vigra_discerosion_c])
VIGRA_C_FUNCTION([vigra_discdilation_c])
VIGRA_C_FUNCTION([vigra_distancetransform_c])
VIGRA_C_FUNCTION([vigra_imagewidth_c])
VIGRA_C_FUNCTION([vigra_imageheight_c])
VIGRA_C_FUNCTION([vigra_imagenumbands_c])
VIGRA_C_FUNCTION([vigra_importgrayimage_c])
VIGRA_C_FUNCTION([vigra_importrgbimage_c])
VIGRA_C_FUNCTION([vigra_importrgbaimage_c])
VIGRA_C_FUNCTION([vigra_exportgrayimage_c])
VIGRA_C_FUNCTION([vigra_exportrgbimage_c])
VIGRA_C_FUNCTION([vigra_exportrgbaimage_c])
VIGRA_C_FUNCTION([vigra_resizeimage_c])
VIGRA_C_FUNCTION([vigra_rotateimage_c])
VIGRA_C_FUNCTION([vigra_reflectimage_c])
VIGRA_C_FUNCTION([vigra_subimage_c])
VIGRA_C_FUNCTION([vigra_paddimage_c])
VIGRA_C_FUNCTION([vigra_clipimage_c])
VIGRA_C_FUNCTION([vigra_localminima_c])
VIGRA_C_FUNCTION([vigra_localmaxima_c])
VIGRA_C_FUNCTION([vigra_extractfeatures_gray_c])
VIGRA_C_FUNCTION([vigra_extractfeatures_rgb_c])
VIGRA_C_FUNCTION([vigra_fouriertransform_c])
VIGRA_C_FUNCTION([vigra_fouriertransforminverse_c])


GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS([2.0.14])
GUILE3_PROGS([3.0.7])
GUILE_SITE_DIR
GUILE_GLOBAL_SITE_DIR
GUILE_SITE_CCACHE_DIR
GUILE_FLAGS

AC_ARG_WITH(
  [guile-site],
  [AS_HELP_STRING(
     [--with-guile-site],
     [install source modules and compiled files using Guile's site and site-ccache dirs])],
  [case "x$withval" in
     xyes)
     guile_site="yes";;
     *)
     guile_site="no";;
   esac],
  [guile_site="no"])
AC_SUBST([guile_site])

if test "x$guile_site" = "xyes"; then
   SITEDIR="$GUILE_GLOBAL_SITE";
   SITECCACHEDIR="$GUILE_SITE_CCACHE";
else
   SITEDIR="$datadir/guile-cv";
   SITECCACHEDIR="$libdir/guile-cv/guile/$GUILE_EFFECTIVE_VERSION/site-ccache";
fi
AC_SUBST([SITEDIR])
AC_SUBST([SITECCACHEDIR])


PKG_CHECK_MODULES(
  [GUILE_LIB],
  [guile-lib-1.0 >= 0.2.5])


AC_CONFIG_FILES(
  [pre-inst-env],
  [chmod +x pre-inst-env])

AC_CONFIG_FILES([
  meta/guile-cv-1.0.pc
  Makefile
  libguile-cv/Makefile
  doc/Makefile
  test-suite/Makefile
])

AC_OUTPUT

AC_MSG_NOTICE(
  []
  []
  [prefix=$prefix]
  [exec_prefix=$exec_prefix]
  [libdir=$libdir]
  [datarootdir=$datarootdir]
  [datadir=$datadir]
  []
  [--with-guile-site="$guile_site"]
  []
  [sitedir (source code): ${SITEDIR}]
  [siteccachedir (compiled modules): ${SITECCACHEDIR}]
  [])

if test "x$guile_site" = "xno"; then
  AC_MSG_WARN(
    []
    []
    [Since --with-guile-site="no", sitedir and siteccachedir differ from]
    [the one used by Guile itself. You have to augment both Guile's %load-path]
    [and %load-compiled-path variables before to use Guile-CV's modules (see]
    ['Environment Variables' and 'Load Paths' in the Guile Reference Manual).]
    []
    [Otherwise, consider using --with-guile-site=yes])
fi
