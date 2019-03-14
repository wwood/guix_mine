(define-module (ben packages metafrost)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages django)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
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
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)

  #:use-module (gnu packages bioinformatics))

(define-public bifrost
  (let ((commit "d479e63738bb3d14ef00e1fc3b31d35d61cd223a"))
    (package
     (name "bifrost")
     (version (string-append "0-1." (string-take commit 8)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pmelsted/bifrost")
             (commit commit)))
       (file-name (string-append name "-" version))
       (patches (search-patches "bifrost-remove-march-native.patch"))
       (sha256
        (base32
         "1gafd347xc59hg22yacrasc0v07dyb9cdm57iw8lp1prn4m7v8l7"))))
     (build-system cmake-build-system)
     (arguments
      `(#:tests? #f)) ; There are no tests.
     (inputs
      `(("zlib" ,zlib)))
     (home-page "https://github.com/pmelsted/bifrost")
     (synopsis "Highly parallel construction and indexing of colored and compacted de Bruijn graphs")
     (description "Highly parallel construction and indexing of colored and compacted de Bruijn graphs")
     (license license:bsd-2)))) ; But it includes other licenses.

(define-public metafrost
  (package
   (name "metafrost")
   (version (string-append "0.0.0.dev"))
   (source
    (local-file (string-append (getenv "HOME") "/git/metafrost")
                #:recursive? #t))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'validate-runpath) ; Currently having problems.
                     (add-before 'build 'delete-old ; Otherwise make does not overwrite, I think.
                                 (lambda _
                                   (delete-file "metafrost")
                                   #t))
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
                                       (let* ((bin (string-append (assoc-ref outputs "out") "/bin/"))
                                              (file "metafrost"))
                                         (install-file file bin)
                                         #t))))))
   (inputs
    `(("bifrost" ,bifrost)
      ("zlib" ,zlib)))
   (home-page #f) ;TODO
   (synopsis "De-Bruijn graph utilising metagenomic utilities")
   (description "De-Bruijn graph utilising metagenomic utilities")
   (license #f))) ;TODO

(define-public metafrost-rhys
  (package
   (inherit metafrost)
   (name "metafrost-rhys")
   (source
    (local-file (string-append (getenv "HOME") "/git/metafrost")
                #:recursive? #t))))

(define-public metafrost2
  (package
   (name "metafrost2")
   (version (string-append "0.0.0.dev2"))
   (source
    (local-file (string-append (getenv "HOME") "/git/metafrost2/target/release")
                #:recursive? #t))
   (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let* ((so (string-append
                                (assoc-ref inputs "libc")
                                ,(glibc-dynamic-linker)))
                           (zlib (assoc-ref inputs "zlib"))
                           (zlib-lib (string-append zlib "/lib"))
                           (gcc (assoc-ref inputs "gcc:lib"))
                           (gcc-lib (string-append gcc "/lib"))
                           (xz (assoc-ref inputs "xz"))
                           (xz-lib (string-append xz "/lib"))
                           (bzip (assoc-ref inputs "bzip2"))
                           (bzip-lib (string-append bzip "/lib")))
                      (and
                       (invoke "patchelf" "--set-interpreter" so "metafrost2")
                       (invoke "patchelf" "--set-rpath"
                               (string-append zlib-lib ":" xz-lib ":" gcc-lib ":" bzip-lib)
                               "metafrost2")
                       (invoke "patchelf" "--print-rpath" "metafrost2")
                       (invoke "patchelf" "--shrink-rpath" "metafrost2")
                       (invoke "./metafrost2" "-h")
                       ))))
         ;; (replace 'check ; this is just a binary, so run rudimentary check.
         ;;   (lambda _ (zero? (system* "./coverm" "--help"))))
         (delete 'strip) ; Does not work. Eh.
         (delete 'validate-runpath)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin/"))
                    (file "metafrost2"))
               (install-file file bin)))))))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs ; Rough - could be cut down.
     `(("zlib" ,zlib)
       ("bzip" ,bzip2)
       ("gcc:lib" ,gcc "lib")
       ("xz" ,xz)
       ("bwa" ,bwa)
       ("samtools" ,samtools)
       ("bash" ,bash)))
   (home-page #f) ;TODO
   (synopsis "Interpret metafrost output")
   (description "Interpret metafrost output")
   (license #f))) ;TODO
