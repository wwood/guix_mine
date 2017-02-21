(define-module (ben packages r-studio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (ice-9 regex)
  
  #:use-module (ace packages external)
  #:use-module (gnu packages bioinformatics))


;; Commmented out because gstreamer 0.10 is not present, this package does not work.
;; This is a terrible package. It does not work inside a container, but does
;; seem to work outside, on Ubuntu 16.04 at least. It probably will not work on
;; other systems, and should be replaced with a package built properly from
;; source.
;; (define-public r-studio-binary
;;   (package
;;     (name "r-studio-binary")
;;     (version "1.0.136")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append
;;                     "https://download1.rstudio.org/rstudio-"
;;                     version "-amd64-debian.tar.gz"))
;;               (sha256
;;                (base32
;;                 "1ghdavahwn4qgn7dnd0yjz7i5lgyxcdpfi4bm5bsmj6vxwfj3sg2"))))
;;     (build-system gnu-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (delete 'configure)
;;          (delete 'build)
;;          (delete 'check)
;;          (add-before 'install 'patch-rpaths
;;            ;; Some libraries are not available in base Ubuntu, so we modify the
;;            ;; RPATH of some libraries
;;            (lambda* (#:key inputs #:allow-other-keys)
;;              (zero? (system*
;;                      "patchelf" "--set-rpath"
;;                      (string-append
;;                       (assoc-ref inputs "libxslt") "/lib:"
;;                       ;(assoc-ref inputs "gstreamer") "/lib"
;;                       )
;;                      "bin/libQt5WebKit.so.5"))))
;;          (replace 'install
;;                   (lambda* (#:key outputs #:allow-other-keys)
;;                     (copy-recursively "." (assoc-ref outputs "out"))
;;                     #t))
;;          (delete 'validate-runpath)
;;          )))
;;     (native-inputs
;;      `(("patchelf" ,patchelf)))
;;     (inputs
;;      `(("libxslt" ,libxslt)
;;        ;("gstreamer" ,gstreamer-0.10) ; Needed usually but works on Ubuntu.
;;        ))
;;     (home-page "")
;;     (synopsis "")
;;     (description
;;      "")
;;     (license license:agpl3)))

;; (define-public gstreamer-0.10
;;   (package (inherit gstreamer)
;;     (version "0.10.36")
;;     (source
;;      (origin
;;       (method url-fetch)
;;       (uri (string-append "http://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
;;                           version ".tar.xz"))
;;       (sha256
;;        (base32
;;         "1nkid1n2l3rrlmq5qrf5yy06grrkwjh3yxl5g0w58w0pih8allci"))
;;       (patches
;;         (list (search-patch "gstreamer-0.10-bison3.patch")
;;               (search-patch "gstreamer-0.10-silly-test.patch")))))
;;     (arguments
;;      `(#:tests? #f)) ; One test fails, hopefully no big deal.
;;     (propagated-inputs
;;      `(("libxml2" ,libxml2)))
;;     (inputs `(("glib" ,glib)))
;;     (native-inputs
;;      `(("bison" ,bison)
;;        ("flex" ,flex)
;;        ("perl" ,perl)
;;        ("pkg-config" ,pkg-config)
;;        ("glib" ,glib "bin")
;;        ("python" ,python-2)))))

;; (define-public gst-plugins-base-0.10
;;   (package (inherit gst-plugins-base)
;;     (version "0.10.36")
;;     (source
;;      (origin
;;       (method url-fetch)
;;       (uri (string-append 
;;             "http://gstreamer.freedesktop.org/src/gst-plugins-base/gst-plugins-base-"
;;             version ".tar.xz"))
;;       (sha256
;;        (base32
;;         "0jp6hjlra98cnkal4n6bdmr577q8mcyp3c08s3a02c4hjhw5rr0z"))))
;;     (inputs
;;      `(("glib" ,glib)
;;        ("gstreamer" ,gstreamer-0.10)))
;;     (native-inputs
;;       `(("pkg-config" ,pkg-config)
;;         ("glib" ,glib "bin")
;;         ("python" ,python-2)))))

;; Does not work due to dependency hole - installer expects to be able to
;; download many things (including build artifacts) as dependencies. Pain. Some
;; dependencies are already packaged by Guix e.g. pandoc, for these the file
;; https://github.com/rstudio/rstudio/blob/master/src/cpp/session/CMakeLists.txt
;; can probably be patched to avoid it looking for and installing these
;; packages, since it expects them to be bundled.
(define-public r-studio
  (package
    (name "r-studio")
    (version "1.1.50")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rstudio/rstudio/archive/v"
                    version ".tar.gz"))
	      (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ls6sd11r2kfvc91k6kwp3qykqb0h60cn3zlfm2afzvnc8lbbnlm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-dependencies
           (lambda* (#:key source inputs #:allow-other-keys)
             (let ((common "dependencies/common/"))
               (for-each
                (lambda (dependency)
                  (let ((directory (string-append common dependency)))
                    (mkdir-p directory)
                    (with-directory-excursion
                     directory
                     (system* "unzip" (assoc-ref inputs dependency)))))
                '("dictionaries" "mathjax-26"))))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("qt" ,qt) ; TODO: split into smaller components?
       ("qtwebkit" ,qtwebkit)
       ("r" ,r)
       ("zlib" ,zlib)
       ("boost" ,boost)
       ("openssl" ,openssl)
       ("linux-pam" ,linux-pam)
       ("pandoc" ,ghc-pandoc)
       ("dictionaries" ; TODO: replace this as it is effectively bundled code.
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-dictionaries/core-dictionaries.zip")
           (sha256
            (base32
             "153lg3ai97qzbqp6zjg10dh3sfvz80v42cjw45zwz7gv1risjha3"))))
       ("mathjax-26" ; TODO: This is a separate package, but what is copied in is
                  ; actually minified so unsuitable for Guix. Needs to be
                  ; packaged through npm?
        ,(origin
          (method url-fetch)
          (uri "https://s3.amazonaws.com/rstudio-buildtools/mathjax-26.zip")
          (sha256
           (base32
            "0wbcqb9rbfqqvvhqr1pbqax75wp8ydqdyhp91fbqfqp26xzjv6lk"))))))
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:agpl3)))
