
####
#### Copyright (C) 2016 - 2023
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


# AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-cv.texi

DOC_IMAGES = \
	$(wildcard images/*)
#	images/pp-17-bf.png		\
#	images/pp-17-bin.png		\
#	images/pp-17-label.png		\
#	images/pp-17-label-all.png

DOC_CSS = \
	$(wildcard css/*)

guile_cv_TEXINFOS = \
  version.texi			\
  variables.texi		\
  preface.texi     		\
  introduction.texi		\
  using-guile-cv.texi		\
  cr-ov-naming-conventions.texi	\
  cr-ov-abreviations.texi	\
  cr-ov.texi			\
  cr-ip-idata.texi		\
  cr-ip-kdata.texi		\
  cr-ip-impex.texi		\
  cr-ip-features.texi		\
  cr-ip-texture.texi		\
  cr-ip-filter.texi		\
  cr-ip-process.texi		\
  cr-ip-transform.texi		\
  cr-ip-morphology.texi		\
  cr-ip-segmentation.texi	\
  cr-ip-utils.texi		\
  cr-ip.texi			\
  cr-support-modules.texi	\
  cr-support.texi		\
  cr.texi			\
  indices.texi			\
  fdl.texi			\
  $(DOC_IMAGES)			\
  $(DOC_CSS)

nobase_doc_DATA = \
	$(DOC_IMAGES)		\
	$(DOC_CSS)

EXTRA_DIST = \
	$(DOC_IMAGES)		\
	$(DOC_CSS)

AM_MAKEINFOHTMLFLAGS = \
	-c DOCTYPE='<!DOCTYPE html>'	\
	-c FORMAT_MENU='menu'		\
	--css-ref=../css/manual.css

printenv:
	printf '$(DOC_IMAGES)\n'
