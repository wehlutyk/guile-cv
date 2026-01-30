(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages build-tools)
 (gnu packages gettext)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages image)
 (gnu packages pkg-config)
 (gnu packages tex)
 (gnu packages texinfo)
 (gnu packages)
 (guix build-system gnu)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (guix utils))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define-public guile-cv
  (package
    (name "guile-cv")
    (version "0.4.0")
    (source (local-file "." "guile-cv-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list "GUILE_AUTO_COMPILE=0") ; to prevent guild warnings
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-guile-site-directory
            (lambda _
              (substitute* "configure.ac"
                (("SITEDIR=\"\\$datadir/guile-cv\"")
                 "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\"")
                (("SITECCACHEDIR=\"\\$libdir/guile-cv/")
                 "SITECCACHEDIR=\"$libdir/"))))
          (add-after 'unpack 'substitute-libs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "cv/init.scm"
                (("\\(dynamic-link \"libvigra_c\"\\)")
                 (string-append "(dynamic-link \""
                                (assoc-ref inputs "vigra-c")
                                "/lib/libvigra_c\")"))
                (("\\(dynamic-link \"libguile-cv\"\\)")
                 (format #f "~s"
                         `(dynamic-link
                           (format #f "~alibguile-cv"
                                   (if (getenv "GUILE_CV_UNINSTALLED")
                                       ""
                                       ,(string-append #$output "/lib/")))))))
              (setenv "GUILE_CV_UNINSTALLED" "1")
              ;; Only needed to satisfy the configure script.
              (setenv "LD_LIBRARY_PATH"
                      (string-append (assoc-ref inputs "vigra-c") "/lib")))))))
    (inputs
     (list vigra
           vigra-c
           ;; (primitive-load
           ;;  (string-append (dirname (dirname (current-filename)))
           ;;                 "/vigra_c/guix.scm"))
           guile-3.0))
    (native-inputs
     (list (texlive-local-tree
            (list texlive-booktabs
                  texlive-iwona
                  texlive-lm
                  texlive-siunitx
                  texlive-standalone
                  texlive-xcolor))
           pkg-config
           autoconf
           automake
           texinfo
           libtool
           gettext-minimal))
    (propagated-inputs
     (list guile-lib))
    (home-page "https://www.gnu.org/software/guile-cv/")
    (synopsis "Computer vision library for Guile")
    (description "Guile-CV is a Computer Vision functional programming library
for the Guile Scheme language.  It is based on Vigra (Vision with Generic
Algorithms), a C++ image processing and analysis library.  Guile-CV contains
bindings to Vigra C (a C wrapper to most of the Vigra functionality) and is
enriched with pure Guile Scheme algorithms, all accessible through a nice,
clean and easy to use high level API.")
    (license license:gpl3+)))

guile-cv
