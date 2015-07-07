(define-module (ben packages pplacer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl)

  #:use-module (gnu packages databases)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ocaml)


  )

(define-public pplacer ;;unlikely to work since there is missing ocaml deps, at least
  (package
    (name "pplacer")
    (version "1.1.alpha16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/matsen/pplacer/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kqz8mf7399f102ynn4h92vjhax85rypv7y3d1qm2psqrvslrwjl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
                  (lambda* _ (zero? (system* "make" "-j1"))))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (string-append (assoc-ref outputs "out"))))
                      (mkdir-p out)
                      (copy-recursively "bin"
                                        (string-append out "bin"))))))))
    (native-inputs ;;other ocaml packages required most likely
     `(("sqlite" ,sqlite)
       ("ocaml-3.12.1" ,ocaml-3.12.1)
       ("gsl" ,gsl)
       ("zlib" ,zlib)))
    (home-page "http://matsen.fhcrc.org/pplacer/")
    (synopsis "Place query sequences on a fixed reference phylogenetic tree")
    (description
     "Pplacer places query sequences on a fixed reference phylogenetic
tree to maximize phylogenetic likelihood or posterior probability
according to a reference alignment.  Pplacer is designed to be fast, to
give useful information about uncertainty, and to offer advanced
visualization and downstream analysis.")
    (license license:gpl3)))



;;(define-public ocaml-3.12.1
  ;; (package (inherit ocaml)
  ;;   (version "3.12.1")
  ;;   (source (origin
  ;;             (method url-fetch)
  ;;             (uri (string-append
  ;;                   "http://caml.inria.fr/pub/distrib/ocaml-"
  ;;                   (version-major+minor version)
  ;;                   "/ocaml-" version ".tar.gz"))
  ;;             (sha256
  ;;              (base32
  ;;               "19x4r9j1rb7q67kj36142s30yjbhhs637mbxa2hyxq4b4n3ap0ag"))))
  ;;   (arguments
  ;;    `(#:phases
  ;;      (modify-phases %standard-phases
  ;;        (replace 'build
  ;;                 (lambda _
  ;;                   (zero? (system* "make" "-j1"; (number->string
  ;;                                                ;(parallel-job-count))
  ;;                                   "world.opt")))))))
  ;;   (license (li
 ;;             st license:qpl license:lgpl2.0))))

(define-public ocaml-3.12.1
  (package
    (name "ocaml")
    (version "3.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://caml.inria.fr/pub/distrib/ocaml-"
                     (version-major+minor version)
                     "/ocaml-" version ".tar.gz"))
              (sha256
               (base32
                "19x4r9j1rb7q67kj36142s30yjbhhs637mbxa2hyxq4b4n3ap0ag"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ;; For libiberty, needed for objdump support.
       ("gcc:lib" ,(canonical-package gcc-4.9) "lib")
       ("zlib" ,zlib)))                       ;also needed for objdump support
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (web server))
       #:tests? #f ;; didn't bother to get them to work.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((sh (string-append (assoc-ref inputs "bash")
                                                "/bin/sh"))
                             (quoted-sh (string-append "\"" sh "\"")))
                        (with-fluids ((%default-port-encoding #f))
                          (for-each (lambda (file)
                                      (substitute* file
                                        (("\"/bin/sh\"")
                                         (begin
                                           (format (current-error-port) "\
patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                                   file quoted-sh)
                                           quoted-sh))))
                                    (find-files "." "\\.ml$"))
                          #t))))
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (mandir (string-append out "/share/man")))
                      ;; Custom configure script doesn't recognize
                      ;; --prefix=<PREFIX> syntax (with equals sign).
                      (zero? (system* "./configure"
                                      "--prefix" out
                                      "--mandir" mandir)))))
         (replace 'build
                  (lambda _
                    (zero? (system* "make" "-j1";; (number->string
                                                ;; (parallel-job-count))
                                    "world.opt"))))
         (delete 'check)
         (add-after 'install 'check
                    (lambda _
                      (with-directory-excursion "testsuite"
                        (zero? (system* "make" "all")))))
         (add-before 'check 'prepare-socket-test
                     (lambda _
                       (format (current-error-port)
                               "Spawning local test web server on port 8080~%")
                       (when (zero? (primitive-fork))
                         (run-server (lambda (request request-body)
                                       (values '((content-type . (text/plain)))
                                               "Hello!"))
                                     'http '(#:port 8080)))
                       (let ((file "testsuite/tests/lib-threads/testsocket.ml"))
                         (format (current-error-port)
                                 "Patching ~a to use localhost port 8080~%"
                                 file)
                         (substitute* file
                           (("caml.inria.fr") "localhost")
                           (("80") "8080")
                           (("HTTP1.0") "HTTP/1.0"))
                         #t))))))
    (home-page "https://ocaml.org/")
    (synopsis "The OCaml programming language")
    (description
     "OCaml is a general purpose industrial-strength programming language with
an emphasis on expressiveness and safety.  Developed for more than 20 years at
Inria it benefits from one of the most advanced type systems and supports
functional, imperative and object-oriented styles of programming.")
    ;; The compiler is distributed under qpl1.0 with a change to choice of
    ;; law: the license is governed by the laws of France.  The library is
    ;; distributed under lgpl2.0.
    (license (list license:qpl license:lgpl2.0))))

