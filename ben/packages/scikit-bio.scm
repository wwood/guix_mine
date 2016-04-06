;; Currently I'm not all the way down this rather deep rabbit hole, giving up for now.
;; TODO
;; 1. update descriptions
;; 3. split out docs for scikit-bio at least?
;; 4. get tests to work, have a GTK issue
;; get it to work properly with inputs - why isn't matplotlib included at runtime?
;;
;; This might become easier with a pypi importer that follows dependencies, but
;; that's not implemented yet in guix
;; 

(define-module (ben packages scikit-bio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  
  #:use-module (ice-9 regex)

  #:use-module (gnu packages bioinformatics))


(define-public python-scikit-bio
  (package
    (name "python-scikit-bio")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/scikit-bio/scikit-bio-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "06nrcgfz6c3jb2dnaf1wnvx3dyww94p454c4126gvcvfgv6scczy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-tests-headless
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "skbio/test.py"
               (("^from skbio.util import TestRunner")
                (string-append "print(\"yes!!!!!!\")\n"
                               "import matplotlib\n"
                               "matplotlib.use(\"Agg\")\n"
                               "from skbio.util import TestRunner")))
             #t))
         ;; Install procedure installs extraneous binaries.
         (add-after 'install 'remove-extraneous-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (delete-file-recursively bin))
             #t))
         ;; `setup.py test' does not run tests.
         (delete 'check)
         ;; (add-after 'install 'check-after
         ;;   (lambda* (#:key inputs outputs #:allow-other-keys)
         ;;     ;; Loading skbio loads matplotlib.pyplot which fails due to lack
         ;;     ;; of screen. Fix by doing "matplotlib.use('Agg')" at the
         ;;     ;; beginning (never got this to work though
         ;;     (let ((pythonpath
         ;;             (string-append
         ;;              (getenv "PYTHONPATH")
         ;;              ":" (assoc-ref outputs "out")
         ;;              "/lib/python"
         ;;              (string-take (string-take-right
         ;;                            (assoc-ref inputs "python") 5) 3)
         ;;              "/site-packages")))
         ;;       (display pythonpath)
         ;;       (display "\n")
         ;;       (setenv "PYTHONPATH" pythonpath)
         ;;       ;;(zero? (system* "make" "test"))
         ;;       (zero?
         ;;        (with-directory-excursion "ci"
         ;;          (system* "python" "-v" "skbio/test.py")))
         ;;       ))))))
         )))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-pep8" ,python-pep8)
       ("python-flake8" ,python-flake8)
       ("python-dateutil" ,python-dateutil-2)
       ))
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-bz2file" ,python-bz2file)
       ("python-lockfile" ,python-lockfile)
       ("python-cachecontrol" ,python-cachecontrol)
       ("python-contextlib2" ,python-contextlib2)
       ("python-decorator" ,python-decorator)
       ("python-future" ,python-future)
       ("python-ipython" ,python-ipython)
       ("python-matplotlib" ,python-matplotlib)
       ("python-natsort" ,python-natsort)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)
       ;("python-nose" ,python-nose) ;native or no?
       ))
    (home-page "http://scikit-bio.org")
    (synopsis
     "Data structures, algorithms and educational resources for bioinformatics.")
    (description
     "scikit-bio (canonically pronounced sigh-kit-buy-oh) is a library for
working with biological data in Python.  It provides python packages for
biological sequences, alignments, tree, visualisation, diversity calculation and
File I/O.")
    (license license:bsd-3)))

(define-public python2-scikit-bio
  (package-with-python2 python-scikit-bio))

(define-public python-natsort
  (package
   (name "python-natsort")
   (version "4.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/n/natsort/natsort-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0f8q66pyczgy1cm3nh8rkh7hgl9h49zx9r06mivg4y5sbzla6sy7"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-pytest" ,python-pytest)
      ("python-pytest-flakes" ,python-pytest-flakes)
      ("python-pytest-cov" ,python-pytest-cov)
      ("python-pytest-cache" ,python-pytest-cache)
      ("python-hypothesis" ,python-hypothesis)
      ("python-pytest-pep8", python-pytest-pep8)
      ))
   (inputs
    `(("python-setuptools" ,python-setuptools)))
   (home-page
    "https://github.com/SethMMorton/natsort")
   (synopsis "Sort lists naturally")
   (description "Sort lists naturally")
   (license license:expat)))

(define-public python2-natsort
  (package-with-python2 python-natsort))

;; (define-public python-hypothesis
;;   (package
;;    (name "python-hypothesis")
;;    (version "1.6.2")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/h/hypothesis/hypothesis-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "0kkbwpnzmrx8m0ba1410q7fkjv314alqax4gicrsyy2g9c5rkkcj"))))
;;    (build-system python-build-system)
;;    ;(arguments
;;    ; `(#:tests? #f)) ;; fails because test directory not included in the distribution?
;;    (native-inputs
;;     `(("python-flake8" ,python-flake8)
;;       ("python-pytest" ,python-pytest)
;;       ("python-pytest-cache" ,python-pytest-cache)))
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)))
;;    (home-page
;;     "https://github.com/DRMacIver/hypothesis")
;;    (synopsis "A library for property based testing")
;;    (description
;;     "A library for property based testing")
;;    (license #f)))

;; (define-public python2-hypothesis
;;   (package-with-python2 python-hypothesis))

;; (define-public python-pytest-cache
;;   (package
;;    (name "python-pytest-cache")
;;    (version "1.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/p/pytest-cache/pytest-cache-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "1a873fihw4rhshc722j4h6j7g3nj7xpgsna9hhg3zn6ksknnhx5y"))))
;;    (build-system python-build-system)
;;    (native-inputs
;;     `(("python-execnet" ,python-execnet)
;;       ("python-pytest" ,python-pytest)))
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)))
;;    (home-page
;;     "http://bitbucket.org/hpk42/pytest-cache/")
;;    (synopsis
;;     "pytest plugin with mechanisms for caching across test runs")
;;    (description
;;     "pytest plugin with mechanisms for caching across test runs")
;;    (license #f)))

;; (define-public python2-pytest-cache
;;   (package-with-python2 python-pytest-cache))

;; (define-public python-execnet
;;   (package
;;     (name "python-execnet")
;;     (version "1.4.1")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append
;;              "https://pypi.python.org/packages/source/e/execnet/execnet-"
;;              version
;;              ".tar.gz"))
;;        (sha256
;;         (base32
;;          "1rpk1vyclhg911p3hql0m0nrpq7q7mysxnaaw6vs29cpa6kx8vgn"))))
;;     (build-system python-build-system)
;;     (inputs
;;      `(("python-setuptools" ,python-setuptools)))
;;     (home-page "http://codespeak.net/execnet")
;;     (synopsis
;;      "execnet: rapid multi-Python deployment")
;;     (description
;;      "execnet: rapid multi-Python deployment")
;;     (license expat)))

;; (define-public python2-execnet
;;   (package-with-python2 python-execnet))

(define-public python-cachecontrol
  (package
   (name "python-cachecontrol")
   (version "0.11.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/C/CacheControl/CacheControl-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "15bn8xll6z15h0zqhfjy1n8dn8p0fcb4m0rhnfanq63z7r2wpprp"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-requests" ,python-requests)
      ("python-lockfile" ,python-lockfile))) ;;lockfile is only needed by the 'filecache' extras_require - include it in the default package version?
   (home-page
    "https://github.com/ionrock/cachecontrol")
   (synopsis "httplib2 caching for requests")
   (description "httplib2 caching for requests")
   (license #f)))

(define-public python2-cachecontrol
  (package-with-python2 python-cachecontrol))

(define-public python-future
  (package
    (name "python-future")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/f/future/future-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15wvcfzssc68xqnqi1dq4fhd0848hwi9jn42hxyvlqna40zijfrx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; The tests connect to the network
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://python-future.org")
    (synopsis
     "Clean single-source support for Python 3 and 2")
    (description
     "Clean single-source support for Python 3 and 2")
    (license license:expat)))

(define-public python2-future
  (package-with-python2 python-future))

(define-public python-pytest-flakes
  (package
   (name "python-pytest-flakes")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/p/pytest-flakes/pytest-flakes-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0flag3n33kbhyjrhzmq990rvg4yb8hhhl0i48q9hw0ll89jp28lw"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-pytest-cache" ,python-pytest-cache)
      ("python-pytest" ,python-pytest)
      ("python-pyflakes" ,python-pyflakes)))
   (home-page
    "https://github.com/fschulze/pytest-flakes")
   (synopsis
    "pytest plugin to check source code with pyflakes")
   (description
    "pytest plugin to check source code with pyflakes")
   (license expat)))

(define-public python2-pytest-flakes
  (package-with-python2 python-pytest-flakes))

;; (define-public python-pytest-cov
;;   (package
;;    (name "python-pytest-cov")
;;    (version "2.1.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/p/pytest-cov/pytest-cov-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "01l5c0m1a39r7p89xlk3x6sry5h0kf0m29j0y6ihz4z97fpc71kf"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)
;;       ("python-py" ,python-py)
;;       ("python-pytest" ,python-pytest)
;;       ("python-coverage" ,python-coverage)
;;       ("python-cov-core" ,python-cov-core)))
;;    (home-page
;;     "https://github.com/schlamar/pytest-cov")
;;    (synopsis
;;     "py.test plugin for coverage reporting with support for both centralised and distributed testing, including subprocesses and multiprocessing")
;;    (description
;;     "py.test plugin for coverage reporting with support for both centralised and distributed testing, including subprocesses and multiprocessing")
;;    (license #f)))

;; (define-public python2-pytest-cov
;;   (package-with-python2 python-pytest-cov))

(define-public python-cov-core
  (package
   (name "python-cov-core")
   (version "1.15.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/c/cov-core/cov-core-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0k3np9ymh06yv1ib96sb6wfsxjkqhmik8qfsn119vnhga9ywc52a"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-coverage" ,python-coverage)))
   (home-page
    "https://github.com/schlamar/cov-core")
   (synopsis
    "plugin core for use by pytest-cov, nose-cov and nose2-cov")
   (description
    "plugin core for use by pytest-cov, nose-cov and nose2-cov")
   (license #f)))

(define-public python2-cov-core
  (package-with-python2 python-cov-core))

(define-public python-pytest-pep8
  (package
   (name "python-pytest-pep8")
   (version "1.0.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/p/pytest-pep8/pytest-pep8-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "06032agzhw1i9d9qlhfblnl3dw5hcyxhagn7b120zhrszbjzfbh3"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ;;install_requires=['pytest-cache', 'pytest>=2.4.2', 'pep8>=1.3'
      ("python-pytest-cache", python-pytest-cache)
      ("python-pytest" ,python-pytest)
      ("python-pep8" ,python-pep8)))
   (home-page
    "http://bitbucket.org/hpk42/pytest-pep8/")
   (synopsis
    "pytest plugin to check PEP8 requirements")
   (description
    "pytest plugin to check PEP8 requirements")
   (license expat)))

(define-public python2-pytest-pep8
  (package-with-python2 python-pytest-pep8))

(define-public python-bz2file
  (package
    (name "python-bz2file")
    (version "0.98")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/b/bz2file/bz2file-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "126s53fkpx04f33a829yqqk8fj4png3qwg4m66cvlmhmwc8zihb4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; python setup.py test does not work as of 0.98
         ;; but there is only the one test file
         (replace 'check (lambda _ (zero? (system* "python"
                                                   "test_bz2file.py")))))))
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/nvawda/bz2file")
    (synopsis
     "Read and write bzip2-compressed files.")
    (description
     "Read and write bzip2-compressed files.")
    (license license:asl2.0)))

(define-public python2-bz2file
  (package-with-python2 python-bz2file))


