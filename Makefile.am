
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


include $(top_srcdir)/am/guile.mk


pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=meta/guile-cv-1.0.pc

SOURCES = \
	cv/support/libguile-cv.scm		\
	cv/support/modules.scm			\
	cv/support/goops.scm			\
	cv/support/g-export.scm			\
	cv/support/utils.scm			\
	cv/support/float.scm			\
	cv/support/push.scm			\
	cv/support/keyword.scm			\
	cv/support/s32vector.scm		\
	cv/support/f32vector.scm		\
	cv/support/f64vector.scm		\
	cv/support/latex.scm			\
	cv/support/pi.scm			\
	cv/support.scm				\
	cv/init.scm				\
	cv/idata.scm				\
	cv/kdata.scm				\
	cv/impex.scm				\
	cv/utils.scm				\
	cv/segmentation.scm			\
	cv/features.scm				\
	cv/filter.scm				\
	cv/process.scm				\
	cv/transform.scm			\
	cv/texture.scm				\
	cv/particles.scm			\
	cv/morphology.scm			\
	cv/compose.scm				\
	cv/histogram.scm			\
	cv.scm

# NOCOMP_SOURCES= \
#	$(wildcard cv/images/*)
# 	$(wildcard cv/latex/*)	\
# 	$(wildcard cv/gtkrc/*)	\
# 	$(shell find $(top_srcdir)/cv/pof -type f)

EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	build-aux/latex.scm		\
	pre-inst-env.in			\
	meta/guile-cv-1.0.pc.in		\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(SOURCES)
#	$(NOCOMP_SOURCES)

# bin_SCRIPTS = cv/cv

CLEANFILES = \
	meta/guile-cv-1.0.pc		\
	$(GOBJECTS)
# 	$(bin_SCRIPTS)

ETAGS_ARGS = \
	$(SOURCES)
# guile-cv

SUBDIRS = \
	libguile-cv			\
	test-suite			\
	doc

distclean-local:
	rm -f Makefile
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -f build-aux/compile
	rm -f build-aux/config.guess
	rm -f build-aux/config.sub
	rm -f build-aux/depcomp
	rm -f build-aux/install-sh
	rm -f build-aux/ltmain.sh
	rm -f build-aux/mdate-sh
	rm -f build-aux/missing
	rm -f build-aux/texinfo.tex
	rm -f build-aux/*~
	rm -f *~
	rm -f guile-cv-*.tar.gz
	rm -f guile-cv-*.tar.gz.sig
	rm -f libguile-cv/Makefile
	rm -f libguile-cv/Makefile.in
	rm -f libguile-cv/libguile-cv.la
	rm -f libguile-cv/libguile-cv.lo
	rm -f libguile-cv/libguile-cv.so
	rm -rf doc/guile-cv
	rm -f doc/TAGS
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.fns doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr doc/*.vrs
	rm -f doc/Makefile
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f doc/version.texi
	rm -f cv/TAGS
	rm -f cv/*~
	rm -f test-suite/Makefile
	rm -f test-suite/Makefile.in
	rm -f m4/libtool.m4
	rm -f m4/ltoptions.m4
	rm -f m4/ltsugar.m4
	rm -f m4/ltversion.m4
	rm -f m4/lt~obsolete.m4
	rm -f m4/*~


ACLOCAL_AMFLAGS = -I m4

# * .PHONY: gen-pof:

# Makefile.am ends here
