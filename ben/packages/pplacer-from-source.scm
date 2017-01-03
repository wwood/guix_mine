(define-module (ben packages pplacer-from-source)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system ocaml)
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
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
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
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages less)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mit-krb5)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages openldap)
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
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)

  #:use-module (ace packages ace)
  #:use-module (ace packages external)
  #:use-module (gnu packages bioinformatics))

(define-public ocaml-ounit
  (package
    (name "ocaml-ounit")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://forge.ocamlcore.org/frs/download.php/1258/ounit-"
             version ".tar.gz"))
       (sha256
        (base32
         "118xsadrx84pif9vaq13hv4yh22w9kmr0ypvhrs0viir1jr0ajjd"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "test" ; need to configure to run the tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (zero? (system* "./configure" "--enable-tests"))))
         (add-before 'install 'setup-install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (destdir (string-append out "/lib/ocaml")))
                      (mkdir-p destdir)
                      (setenv "OCAMLFIND_DESTDIR" destdir)
                      (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                      #t))))))
    (inputs
     `(("libxml" ,libxml2)))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("pkg-config" ,pkg-config)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-ounit))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-ounit
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-ounit)))

(define-public ocaml-xmlm ; does not work
  (package
    (name "ocaml-xmlm")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://erratique.ch/software/xmlm/releases/xmlm-"
             version ".tbz"))
       (sha256
        (base32
         "1jywcrwn5z3gkgvicr004cxmdaqfmq8wh72f81jqz56iyn5024nh"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ; There are no tests, maybe?
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
                  (lambda _
                    (zero? (system* "pkg/build" "true"))))
         (add-before 'install 'setup-install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (destdir (string-append out "/lib/ocaml")))
                      (mkdir-p destdir)
                      (setenv "OCAMLFIND_DESTDIR" destdir)
                      (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                      #t)))
         (replace 'install
                  (lambda _
                    ;(system* "ls" "_build/**/*")
                    (zero? (system* "ocamlfind" "install" "xmlm"
                                    ;; List of files was compiled from _build/pkg/META.
                                    "_build/pkg/META"
                                    "_build/src/xmlm.a"
                                    "_build/src/xmlm.annot"
                                    "_build/src/xmlm.cma"
                                    "_build/src/xmlm.cmi"
                                    "_build/src/xmlm.cmt"
                                    "_build/src/xmlm.cmx"
                                    "_build/src/xmlm.cmxa"
                                    "_build/src/xmlm.cmxs"
                                    "_build/src/xmlm.mli"))
                    )))))
    (native-inputs
     `(("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-xmlm))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-xmlm
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-xmlm)))

(define-public ocaml-mcl ; only installs binaries, so is there any point of being ocaml specific?
  (package
    (name "ocaml-mcl")
    (version "12-068oasis4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/fhcrc/mcl/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1l5jbhwjpsj38x8b9698hfpkv75h8hn3kj0gihjhn8ym2cwwv110"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ; There are no tests, maybe?
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (setenv "SHELL" (which "sh"))
                    (setenv "CONFIG-SHELL" (which "sh"))
                    (substitute* "configure"
                      (("SHELL = /bin/sh") (string-append "SHELL = "(which "sh"))))
                    (substitute* "setup.ml"
                      (("LDFLAGS=-fPIC") (string-append "LDFLAGS=-fPIC\"; \"SHELL=" (which "sh"))))
                    (and
                                        ;(zero? (system* "./bootstrap"))
                     ;(zero? (system* "autoconf"))

                     (zero? (system* "ocaml" "setup.ml" "-configure" "--prefix" (assoc-ref outputs "out")))
                     (begin (substitute* '("src/gryphon/Makefile"
                                           "src/shmx/Makefile"
                                           "src/alien/oxygen/src/Makefile"
                                           "src/alien/oxygen/Makefile"
                                           "src/alien/oxygen/doc/Makefile"
                                           "src/alien/Makefile"
                                           "src/impala/Makefile"
                                           "src/Makefile"
                                           "src/shcl/Makefile"
                                           "src/mcl/Makefile"
                                           "src/shmcx/Makefile"
                                           "src/shmcl/Makefile"
                                           "src/clew/Makefile"
                                           "src/shmcxquery/Makefile"
                                           "img/Makefile"
                                           "graphs/Makefile"
                                           "testing/stream/Makefile"
                                           "testing/Makefile"
                                           "testing/blast/Makefile"
                                           "testing/setops/Makefile"
                                           "Makefile"
                                           "doc/Makefile"
                                           "util/Makefile"
                                           "include/Makefile"
                                           "scripts/Makefile"
                                           )
                              (("prefix = /usr/local") (string-append "prefix = " (assoc-ref outputs "out"))))
                            #t))))
         (replace 'build
                  (lambda _
                    (zero? (system* "ocaml" "setup.ml" "-build"))))
         (add-before 'install 'setup-install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (destdir (string-append out "/lib/ocaml")))
                      (mkdir-p destdir)
                      (setenv "OCAMLFIND_DESTDIR" destdir)
                      (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                      #t)))
         (replace 'install
                  (lambda _ (zero? (system* "ocaml" "setup.ml" "-install")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-mcl))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-mcl
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-mcl)))


(define-public ocaml-batteries
  (package
    (name "ocaml-batteries")
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/ocaml-batteries-team/batteries-included/archive/v"
         version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fbhafjdqarppp54nmzalng577s9wk1753qw11f449shwv4cydyl"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:parallel-tests? #f
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'setup-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (destdir (string-append out "/lib/ocaml")))
                         (mkdir-p destdir)
                         (setenv "OCAMLFIND_DESTDIR" destdir)
                         ;(setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                         #t))))))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("ocaml-ounit" ,ocaml-ounit)
       ("ocaml-qcheck" ,ocaml-qcheck)
       ("ocaml-qtest" ,ocaml-qtest)
       ("ocaml-bisect" ,ocaml-bisect))) ;native?
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-batteries))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-batteries
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-batteries)))

(define-public ocaml-qcheck
  (package
    (name "ocaml-qcheck")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://github.com/c-cube/qcheck/archive/"
         version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gxvh3yr2016m2i7xm4lc08vklj12z2jhhhhyz3if5mrh5ch63ck"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda _
                    (zero? (system* "./configure" "--enable-tests"))))
         (add-before 'install 'setup-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (destdir (string-append out "/lib/ocaml"))
                              (bin (string-append out "/bin")))
                         (mkdir-p destdir)
                         (setenv "OCAMLFIND_DESTDIR" destdir)
                         #t))))))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("ocaml-ounit" ,ocaml-ounit)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-qcheck))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-qcheck
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-qcheck)))

(define-public ocaml-qtest
  (package
    (name "ocaml-qtest")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/vincent-hugot/iTeML/archive/v"
         version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n7x5l6h4j44f75wzgzjsjkq349i4gj707w1hr7fx84igxxfr6vl"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ; There is a test directory but no 'make check' equivalent.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'setup-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (destdir (string-append out "/lib/ocaml"))
                              (bin (string-append out "/bin")))
                         (mkdir-p destdir)
                         (setenv "OCAMLFIND_DESTDIR" destdir)
                         (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                         (mkdir-p bin)
                         (setenv "BIN" bin)
                         #t))))))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("ocaml-ounit" ,ocaml-ounit)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-qtest))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-qtest
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-qtest)))

(define-public ocaml-gsl
  (package
    (name "ocaml-gsl")
    (version "1.19.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/mmottl/gsl-ocaml/releases/download/v"
         version"/gsl-ocaml-" version ".tar.gz"))
       (sha256
        (base32
         "0rjbng1540kn33c7dfhqga9hna71zkm1llq1yb1a0kivxna1b285"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "test" ; need to configure to run the tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (zero? (system* "./configure" "--enable-tests"))))
         (add-before 'install 'setup-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (destdir (string-append out "/lib/ocaml")))
                         (mkdir-p destdir)
                         (setenv "OCAMLFIND_DESTDIR" destdir)
                         (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                         #t))))))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)))
    (inputs
     `(("gsl" ,gsl)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-gsl))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-gsl
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-gsl)))

;; Version 1.05 is the last version to support OCaml 4.01.0.
(define-public ocaml4.01.0-camlzip-1.05
  (package
    (name "ocaml4.01.0-camlzip")
    (version "1.05")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://forge.ocamlcore.org/frs/download.php/1037/camlzip-"
         version ".tar.gz"))
       (sha256
        (base32
         "0syh72jk9s0qwjmmfrkqchaj98m020ii082jn38pwnmb6v3p02wk"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:ocaml ,ocaml-4.01.0
       #:test-target "test" ; need to configure to run the tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
                  (lambda _
                    (and
                     (zero? (system* "make" "all"))
                     (zero? (system* "make" "allopt"))
                     (zero? (system* "make" "allopt"))
                     )))
         (add-before 'install 'setup-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (destdir (string-append out "/lib/ocaml")))
                         (mkdir-p destdir)
                         (setenv "OCAMLFIND_DESTDIR" destdir)
                         (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                         #t)))
         (replace 'install
                  (lambda _
                    (zero? (system* "make" "install-findlib"))))
         )))
    (native-inputs
     `(("ocaml-findlib" ,ocaml4.01.0-findlib)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:x11))) ;?

(define-public ocaml-bisect
  (package
    (name "ocaml-bisect")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://bisect.x9c.fr/distrib/bisect-"
         version ".tar.gz"))
       (sha256
        (base32
         "0figr4jl1alzyjkmsz3s5bpsiqydmfcdsgz1nkwy089ayb05rj0r"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ;TODO
       #:test-target "test" ; need to configure to run the tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda _ (zero? (system* "./configure"))))
         (add-before 'build 'patch-makefile
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("^OCAMLBUILD=.*") "OCAMLBUILD=ocamlbuild\n")
                         (("^OCAMLBUILD_ENV=.*") "OCAMLBUILD_ENV=WARNINGS=$(WARNINGS)\n")
                         (("^OCAMLBUILD_FLAGS=-classic-display -no-links")
                          (string-append
                           "OCAMLBUILD_FLAGS=-classic-display -no-links -cflags '-I "
                           (assoc-ref inputs "camlp4") "/lib/camlp4'")))
                       #t))
         (replace 'build
                  (lambda* (#:key inputs #:allow-other-keys)
                    (zero? (system* "make" "all"))))
         (add-before 'install 'setup-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (destdir (string-append out "/lib/ocaml")))
                         (mkdir-p destdir)
                         (setenv "OCAMLFIND_DESTDIR" destdir)
                         (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                         #t)))
         )))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("which" ,which)
       ("camlp4" ,camlp4)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-bisect))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-bisect
  (let ((base (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-bisect))))
    (package
      (inherit base)
      (arguments
       `(#:ocaml ,ocaml-4.01.0
         #:tests? #f ;TODO
         #:test-target "test" ; need to configure to run the tests
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
                    (lambda _ (zero? (system* "./configure"))))
           (add-before 'build 'patch-makefile
                       (lambda _
                         (substitute* "Makefile"
                           (("^OCAMLBUILD_ENV=.*") "OCAMLBUILD_ENV=WARNINGS=$(WARNINGS)\n"))
                         #t))
           (replace 'build
                    (lambda* (#:key inputs #:allow-other-keys)
                      (zero? (system* "make" "all"))))
           (add-before 'install 'setup-install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (let* ((out (assoc-ref outputs "out"))
                                (destdir (string-append out "/lib/ocaml")))
                           (mkdir-p destdir)
                           (setenv "OCAMLFIND_DESTDIR" destdir)
                           (setenv "OCAMLFIND_LDCONF" (string-append destdir "/ld.conf"))
                           #t))))))
      (native-inputs
       `(("ocaml-findlib" ,ocaml4.01.0-findlib)
         ("which" ,which))))))

(define-public ocaml-csv
  (package
    (name "ocaml-csv")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://github.com/Chris00/ocaml-csv/releases/download/"
         version "/csv-" version ".tar.gz"))
       (sha256
        (base32
         "13zm5g390i741qwhvd25xn0aq3gmnd0qipqgp5j3vzpmwls7cc7n"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "test" ; need to configure to run the tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-setup-ml
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (substitute* "setup.ml"
                          (("/usr/local") out)))
                      #t))
           (add-before 'install 'setup-install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (let* ((out (assoc-ref outputs "out"))
                                (destdir (string-append out "/lib/ocaml")))
                           (mkdir-p destdir)
                           (setenv "OCAMLFIND_DESTDIR" destdir)
                           #t))))))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (properties `((ocaml4.01.0-variant . ,(delay ocaml4.01.0-csv))))
    (license license:x11))) ;?

(define-public ocaml4.01.0-csv
  (package-with-ocaml4.01.0 (strip-ocaml4.01.0-variant ocaml-csv)))


(define-public pplacer-from-source
  (package
    (name "pplacer-from-source")
    (version "1.1.alpha19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/matsen/pplacer/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "0z1lnd2s8sh6kpzg106wzbh2szw7h0hvq8syd5a6wv4rmyyz6x0f"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:ocaml ,ocaml-4.01.0
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-makefile
                    (lambda _
                      (substitute* "Makefile"
                        (("DESCRIPT:=pplacer-.*") "DESCRIPT:=pplacer-test\n")) ;fixme
                      #t))
         (replace 'build
                  (lambda _ (zero? (system* "make" "all"))))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (copy-recursively "bin" bin)
                      ;; TODO: Install scripts.
                      )
                    #t))
         )))
    (inputs
     `(("zlib" ,zlib)
       ("ocaml-findlib" ,ocaml4.01.0-findlib)
       ("ocaml-gsl" ,ocaml4.01.0-gsl)
       ("gsl" ,gsl)
       ("ocaml-batteries" ,ocaml4.01.0-batteries)
       ("ocaml-camlzip" ,ocaml4.01.0-camlzip-1.05)
       ("ocaml-csv" ,ocaml4.01.0-csv)
       ("ocaml-sqlite3" ,ocaml4.01.0-sqlite3)
       ("ocaml-xmlm" ,ocaml4.01.0-xmlm)
       ("ocaml-mcl" ,ocaml4.01.0-mcl)
       ("python" ,python-2)
       ("python-biopython" ,python2-biopython)))
    (native-inputs
     `(("ocaml-ounit" ,ocaml4.01.0-ounit)))
    (synopsis "")
    (description
     "")
    (home-page "")
    (license license:gpl3)))
