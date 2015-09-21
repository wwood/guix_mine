
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

;; (define-module (ben packages scikit-bio)
;;   #:use-module ((guix licenses) #:prefix license:)
;;   #:use-module (guix packages)
;;   #:use-module (guix utils)
;;   #:use-module (guix download)
;;   #:use-module (guix git-download)
;;   #:use-module (guix build-system gnu)
;;   #:use-module (guix build-system cmake)
;;   #:use-module (guix build-system perl)
;;   #:use-module (guix build-system python)
;;   #:use-module (guix build-system ruby)
;;   #:use-module (guix build-system trivial)
;;   #:use-module (gnu packages)
;;   #:use-module (gnu packages autotools)
;;   #:use-module (gnu packages algebra)
;;   #:use-module (gnu packages base)
;;   #:use-module (gnu packages boost)
;;   #:use-module (gnu packages compression)
;;   #:use-module (gnu packages file)
;;   #:use-module (gnu packages gcc)
;;   #:use-module (gnu packages java)
;;   #:use-module (gnu packages linux)
;;   #:use-module (gnu packages maths)
;;   #:use-module (gnu packages ncurses)
;;   #:use-module (gnu packages pcre)
;;   #:use-module (gnu packages perl)
;;   #:use-module (gnu packages pkg-config)
;;   #:use-module (gnu packages popt)
;;   #:use-module (gnu packages protobuf)
;;   #:use-module (gnu packages python)
;;   #:use-module (gnu packages ruby)
;;   #:use-module (gnu packages statistics)
;;   #:use-module (gnu packages swig)
;;   #:use-module (gnu packages tbb)
;;   #:use-module (gnu packages textutils)
;;   #:use-module (gnu packages valgrind)
;;   #:use-module (gnu packages vim)
;;   #:use-module (gnu packages web)
;;   #:use-module (gnu packages xml)
;;   #:use-module (gnu packages zip)
;;   #:use-module (gnu packages databases)
;;   #:use-module (gnu packages fontutils)
;;   #:use-module (gnu packages gdbm)
;;   #:use-module (gnu packages gcc)
;;   #:use-module (gnu packages ghostscript)
;;   #:use-module (gnu packages glib)
;;   #:use-module (gnu packages gtk)
;;   #:use-module (gnu packages icu4c)
;;   #:use-module (gnu packages image)
;;   #:use-module (gnu packages imagemagick)
;;   #:use-module (gnu packages libffi)
;;   #:use-module (gnu packages maths)
;;   #:use-module (gnu packages multiprecision)
;;   #:use-module (gnu packages networking)
;;   #:use-module (gnu packages ncurses)
;;   #:use-module (gnu packages openssl)
;;   #:use-module (gnu packages perl)
;;   #:use-module (gnu packages pkg-config)
;;   #:use-module (gnu packages readline)
;;   #:use-module (gnu packages texlive)
;;   #:use-module (gnu packages texinfo)
;;   #:use-module (gnu packages web)
;;   #:use-module (gnu packages base)
;;   #:use-module (gnu packages xml)
;;   #:use-module (gnu packages xorg)
;;   #:use-module (gnu packages zip)
  
;;   #:use-module (ice-9 regex)

;;   #:use-module (gnu packages bioinformatics))


;; (define-public python-scikit-bio
;;   (package
;;     (name "python-scikit-bio")
;;     (version "0.2.3")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append
;;              "https://pypi.python.org/packages/source/s/scikit-bio/scikit-bio-"
;;              version
;;              ".tar.gz"))
;;        (sha256
;;         (base32
;; 	 "10y6fiz7w6gc34rllixmcg8k1xb6f0yidl2mpg9614r494xwdvjz"))))
;;     (build-system python-build-system)
;;     (arguments
;;      `(#:tests? #f)) ; tests fail because of matplotlib import error (due to lack of display?)
;;     (native-inputs
;;      `(("python-nose" ,python-nose)
;;        ("python-pep8" ,python-pep8)
;;        ("python-flake8" ,python-flake8)
;;        ("python-dateutil" ,python-dateutil-2))) ;remote the -2 for python2 version
;;     (inputs
;;      `(("python-setuptools" ,python-setuptools)
;;        ("python-numpy" ,python-numpy)
;;        ("python-matplotlib" ,python-matplotlib)
;;        ("python-scipy" ,python-scipy)
;;        ("python-pandas" ,python-pandas)
;;        ("python-future" ,python-future)
;;        ("python-natsort" ,python-natsort)
;;        ("python-six" ,python-six)
;;        ("python-ipython" ,python-ipython)
;;        ("python-decorator" ,python-decorator)
;;        ("python-cachecontrol" ,python-cachecontrol)))
;;     (home-page "http://scikit-bio.org")
;;     (synopsis
;;      "Data structures, algorithms and educational resources for bioinformatics.")
;;     (description
;;      "scikit-bio (canonically pronounced sigh-kit-buy-oh) is a library for
;; working with biological data in Python.  It provides python packages for
;; biological sequences, alignments, tree, visualisation, diversity calculation and
;; File I/O.")
;;     (license license:bsd-3)))

;; (define-public python-natsort
;;   (package
;;    (name "python-natsort")
;;    (version "4.0.3")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/n/natsort/natsort-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "17rv4rr2j1avxds105kl6y6qbzvyfj691ldq0h53zl64wj6gqfr8"))))
;;    (build-system python-build-system)
;;    (native-inputs
;;     `(("python-pytest" ,python-pytest)
;;       ("python-pytest-flakes" ,python-pytest-flakes)
;;       ("python-pytest-cov" ,python-pytest-cov)
;;       ("python-pytest-cache" ,python-pytest-cache)
;;       ("python-hypothesis" ,python-hypothesis)
;;       ("python-pytest-pep8", python-pytest-pep8)))
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)))
;;    (home-page
;;     "https://github.com/SethMMorton/natsort")
;;    (synopsis "Sort lists naturally")
;;    (description "Sort lists naturally")
;;    (license license:expat)))

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
;;    (arguments
;;     `(#:tests? #f)) ;; fails because test directory not included in the distribution?
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

;; (define-public python-execnet
;;   (package
;;    (name "python-execnet")
;;    (version "1.3.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/e/execnet/execnet-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "1gmxzjhk9gwkr14fli47sw184hnif341nd9ymydfk3y77ihxbpzc"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)))
;;    (native-inputs
;;     `(("python-hgdistver" ,python-hgdistver)))
;;    (home-page "http://codespeak.net/execnet")
;;    (synopsis
;;     "execnet: rapid multi-Python deployment")
;;    (description
;;     "execnet: rapid multi-Python deployment")
;;    (license expat)))

;; (define-public python-cachecontrol
;;   (package
;;    (name "python-cachecontrol")
;;    (version "0.11.5")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/C/CacheControl/CacheControl-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "0hnglhgx8qxspmigz8swyg76zhlg98v8q17lcw950yxizvb8915p"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)
;;       ("python-requests" ,python-requests)
;;       ("python-lockfile" ,python-lockfile))) ;;lockfile is only needed by the 'filecache' extras_require - include it in the default package version?
;;    (home-page
;;     "https://github.com/ionrock/cachecontrol")
;;    (synopsis "httplib2 caching for requests")
;;    (description "httplib2 caching for requests")
;;    (license #f)))

;; (define-public python-future
;;   (package
;;    (name "python-future")
;;    (version "0.14.3")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/f/future/future-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "1savk7jx7hal032f522c5ajhh8fra6gmnadrj9adv5qxi18pv1b2"))))
;;    (build-system python-build-system)
;;    (arguments
;;     `(#:tests? #f)) ;; tests fail for some reason I don't understand, bad PYTHONPATH?
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)))
;;    (home-page "https://python-future.org")
;;    (synopsis
;;     "Clean single-source support for Python 3 and 2")
;;    (description
;;     "Clean single-source support for Python 3 and 2")
;;    (license expat)))

;; (define-public python-hgdistver
;;   (package
;;    (name "python-hgdistver")
;;    (version "0.25")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/h/hgdistver/hgdistver-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "13cd35d2r473k7r6w2lf84vpkpi49b5gnx8ka2s548l9rmdysfv0"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)))
;;    (home-page
;;     "http://bitbucket.org/RonnyPfannschmidt/hgdistver/")
;;    (synopsis
;;     "utility to generate python package version infos from mercurial/git tags")
;;    (description
;;     "utility to generate python package version infos from mercurial/git tags")
;;    (license expat)))

;; (define-public python-pytest-flakes
;;   (package
;;    (name "python-pytest-flakes")
;;    (version "1.0.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/p/pytest-flakes/pytest-flakes-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "0vvfprga6k4v2zq1qsr3yq1bjl22vygfsnvyn3hh80cc2386dk6h"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)
;;       ("python-pytest-cache" ,python-pytest-cache)
;;       ("python-pytest" ,python-pytest)
;;       ("python-pyflakes" ,python-pyflakes)))
;;    (home-page
;;     "https://github.com/fschulze/pytest-flakes")
;;    (synopsis
;;     "pytest plugin to check source code with pyflakes")
;;    (description
;;     "pytest plugin to check source code with pyflakes")
;;    (license expat)))

;; (define-public python-pytest-cov
;;   (package
;;    (name "python-pytest-cov")
;;    (version "1.8.1")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/p/pytest-cov/pytest-cov-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "118jlxricy378aj9q4lqlvixj084783ds3fk2glf347j0s61kr9i"))))
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

;; (define-public python-cov-core
;;   (package
;;    (name "python-cov-core")
;;    (version "1.15.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/c/cov-core/cov-core-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "0k3np9ymh06yv1ib96sb6wfsxjkqhmik8qfsn119vnhga9ywc52a"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)
;;       ("python-coverage" ,python-coverage)))
;;    (home-page
;;     "https://github.com/schlamar/cov-core")
;;    (synopsis
;;     "plugin core for use by pytest-cov, nose-cov and nose2-cov")
;;    (description
;;     "plugin core for use by pytest-cov, nose-cov and nose2-cov")
;;    (license #f)))

;; (define-public python-pytest-pep8
;;   (package
;;    (name "python-pytest-pep8")
;;    (version "1.0.6")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://pypi.python.org/packages/source/p/pytest-pep8/pytest-pep8-"
;;            version
;;            ".tar.gz"))
;;      (sha256
;;       (base32
;;        "06032agzhw1i9d9qlhfblnl3dw5hcyxhagn7b120zhrszbjzfbh3"))))
;;    (build-system python-build-system)
;;    (inputs
;;     `(("python-setuptools" ,python-setuptools)
;;       ;;install_requires=['pytest-cache', 'pytest>=2.4.2', 'pep8>=1.3'
;;       ("python-pytest-cache", python-pytest-cache)
;;       ("python-pytest" ,python-pytest)
;;       ("python-pep8" ,python-pep8)))
;;    (home-page
;;     "http://bitbucket.org/hpk42/pytest-pep8/")
;;    (synopsis
;;     "pytest plugin to check PEP8 requirements")
;;    (description
;;     "pytest plugin to check PEP8 requirements")
;;    (license expat)))