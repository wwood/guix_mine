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
;  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcsh)
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


;; Commmented out because gstreamer 0.10 is not present
;; This is a terrible package. It does not work inside a container, but does
;; seem to work outside, on Ubuntu 16.04 at least. It probably will not work on
;; other systems, and should be replaced with a package built properly from
;; source.
;; (define-public r-studio-binary
;;   (package
;;     (name "r-studio-binary")
;;     (version "0.99.903")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append
;;                     "https://download1.rstudio.org/rstudio-"
;;                     version "-amd64-debian.tar.gz"))
;;               (sha256
;;                (base32
;;                 "044p9fr14s05nrmlxh860vz2h4qzn6jq2mlgnkidvnkan9pz7xhw"))))
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
;;                       (assoc-ref inputs "gstreamer") "/lib")
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
;;        ("gstreamer" ,gstreamer-0.10)))
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

(define-public qt-5.5
  (package
    (name "qt")
    (version "5.5.1")
    (source (origin
             (method url-fetch)
             (uri
               (string-append
                 "http://download.qt.io/official_releases/qt/"
                 (version-major+minor version)
                 "/" version
                 "/single/qt-everywhere-opensource-src-"
                 version ".tar.xz"))
             (sha256
               (base32
                 "0615cn4n3n78v48lnmapqz2jizm2pzrjwvsjlnsf4awrsiiqw0kg"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                ;; Remove qtwebengine, which relies on a bundled copy of
                ;; chromium. Not only does it fail compilation in qt 5.5:
                ;;    3rdparty/chromium/ui/gfx/codec/jpeg_codec.cc:362:10:
                ;;    error: cannot convert ‘bool’ to ‘boolean’ in return
                ;; it might also pose security problems.
                ;; Alternatively, we could use the "-skip qtwebengine"
                ;; configuration option.
                (delete-file-recursively "qtwebengine")
                ;; Remove one of the two bundled harfbuzz copies in addition
                ;; to passing "-system-harfbuzz".
                (delete-file-recursively "qtbase/src/3rdparty/harfbuzz-ng")
                ;; Remove the bundled sqlite copy in addition to
                ;; passing "-system-sqlite".
                (delete-file-recursively "qtbase/src/3rdparty/sqlite")))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("cups" ,cups)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libjpeg" ,libjpeg)
       ("libmng" ,libmng)
       ("libpci" ,pciutils)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mtdev" ,mtdev)
       ("mysql" ,mysql)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("pcre" ,pcre)
       ("sqlite" ,sqlite)
       ("udev" ,eudev)
       ("unixodbc" ,unixodbc)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ruby" ,ruby)
       ("which" ,(@ (gnu packages base) which))))
    (arguments
     `(;; FIXME: Disabling parallel building is a quick hack to avoid the
       ;; failure described in
       ;; https://lists.gnu.org/archive/html/guix-devel/2016-01/msg00837.html
       ;; A more structural fix is needed.
       #:parallel-build? #f
       #:phases
         (alist-replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("configure" "qtbase/configure")
                (("/bin/pwd") (which "pwd")))
              (substitute* "qtbase/src/corelib/global/global.pri"
                (("/bin/ls") (which "ls")))
              ;; do not pass "--enable-fast-install", which makes the
              ;; configure process fail
              (zero? (system*
                      "./configure"
                      "-verbose"
                      "-prefix" out
                      "-opensource"
                      "-confirm-license"
                      ;; Most "-system-..." are automatic, but some use
                      ;; the bundled copy by default.
                      "-system-sqlite"
                      "-system-harfbuzz"
                      ;; explicitly link with openssl instead of dlopening it
                      "-openssl-linked"
                      ;; explicitly link with dbus instead of dlopening it
                      "-dbus-linked"
                      ;; drop special machine instructions not supported
                      ;; on all instances of the target
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("-no-sse2"))
                      "-no-sse3"
                      "-no-ssse3"
                      "-no-sse4.1"
                      "-no-sse4.2"
                      "-no-avx"
                      "-no-avx2"
                      "-no-mips_dsp"
                      "-no-mips_dspr2"))))
          %standard-phases)))
    (home-page "http://qt-project.org/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license license:lgpl2.1)

    ;; Qt 4: 'QBasicAtomicPointer' leads to build failures on MIPS;
    ;; see <http://hydra.gnu.org/build/112828>.
    ;; Qt 5: assembler error; see <http://hydra.gnu.org/build/112526>.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

;; Does not work due to dependency hole - installer expects to be able to
;; download many things (including build artifacts) as dependencies. Pain.
(define-public r-studio
  (package
    (name "r-studio")
    (version "0.99.1279")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rstudio/rstudio/archive/v"
                    version ".tar.gz"))
	      (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q7ssx0b2gmvvjh3fzagybfcjfh9cq4ml4xcinzj6ngh3g7kyfi7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-dependencies
           (lambda* (#:key source inputs #:allow-other-keys)
             (let ((dictionaries "dependencies/common/dictionaries"))
               (mkdir-p dictionaries)
               (with-directory-excursion dictionaries
                                         (zero? (system* "unzip" (assoc-ref
                                                                  inputs
                                                                  "dictionaries"))))
               ))))))
                                   
                                   
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("qt" ,qt-5.5) ; After this webkit is missing and cannot be built
                      ; according to the ML, so have to use v5.5
       ("r" ,r)
       ("zlib" ,zlib)
       ("boost" ,boost)
       ("openssl" ,openssl)
       ("linux-pam" ,linux-pam)
       ("dictionaries" ; TODO: replace this as it is effectively bundled code.
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-dictionaries/core-dictionaries.zip")
           (sha256
            (base32
             "153lg3ai97qzbqp6zjg10dh3sfvz80v42cjw45zwz7gv1risjha3"))))))
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:agpl3)))
