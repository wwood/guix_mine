(define-module (ben packages local)
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
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
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
  #:use-module (ice-9 regex)
  
  #:use-module (ace packages external)
  #:use-module (gnu packages bioinformatics))

(define-public bamm ;  does not work
  (package
    (name "bamm")
    (version "1.4.2")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "file:///tmp/bamm.tar.gz"))
	      ;;"https://github.com/Ecogenomics/BamM/archive/v"
	      ;;version ".tar.gz"))
	      (file-name (string-append name "-" version ".tar.gz"))
	      (sha256
	       (base32
		"1vdnc1r8z151h2r1bryl8ylqxggyah6hi84cvyfqhdkqsabjvcx4"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ;; no Python 3 support
       #:phases
       (modify-phases %standard-phases
                      (replace 'build
                               (lambda* (#:key inputs #:allow-other-keys)
                                 (let ((htslib (assoc-ref inputs "htslib"))) ;;TODO: use guix
                                   ;;htslib not the
                                   ;;bundled one,
                                   ;;currently bamm
                                   ;;doesn't respect
                                   ;;this
                                   (substitute* "setup.py"
                                                (("if 'sdist' not in sys.argv")
                                                 "if False"))
                                   
                                   (chdir "c")
                                   (system* "autoreconf" "--install")
                                   (substitute* "configure"
                                                (("/bin/sh") (which "bash")))
                                   (substitute* "libcfu-0.03/configure"
                                                (("/bin/sh") (which "bash")))
                                   (substitute* "htslib-1.2.1/configure"
                                                (("/bin/sh") (which "bash")))
                                   (system* "bash" "configure")
                                   (system* "make")
                                   
                                   (copy-file "libBamM.a" "../bamm/libBamM.a")
                                   (chdir "..")
                                   #t
                                   )))
                      (delete 'check)
                      )))
    (inputs
     `(("bwa" ,bwa)
       ("samtools-0.1" ,samtools-0.1)
       ("htslib" ,htslib)
       ("zlib" ,zlib)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("python-nose" ,python2-nose)
       ("python-setuptools" ,python2-setuptools)))
    (home-page "http://ecogenomics.github.io/BamM/")
    (synopsis "Metagenomics-focused BAM file manipulation")
    (description
     "BamM is a c library, wrapped in python, that parses BAM files. The code is
intended to provide a faster, more stable interface to parsing BAM files than
PySam, but doesn't implement all/any of PySam's features. Do you want all the
links that join two contigs in a BAM? Do you need to get coverage? Would you
like to just work out the insert size and orientation of some mapped reads? Then
BamM is for you!")
    (license license:lgpl3+)))

(define-public seqtk ; waiting on licensing issues, but seems to work
  (let ((commit "4feb6e81444ab6bc44139dd3a125068f81ae4ad8"))
    (package
      (name "seqtk")
      (version (string-append "sgdp." commit))
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/lh3/seqtk.git")
                     (commit commit)))
               (sha256
                (base32
                 "0wdkz8chkinfm23cg95nrn797lv12n2wxglwb3s2kvf0iv3rrx01"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'build
                                 (lambda* _
                                   (zero? (system* "make"))))
                        (replace 'install
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (let ((bin (string-append
                                               (assoc-ref outputs "out")
                                               "/bin/")))
                                     (mkdir-p bin)
                                     (copy-file "seqtk" (string-append
                                                         bin "seqtk"))
                                     (copy-file "trimadap" (string-append
                                                            bin "trimadap"))))))))
      (native-inputs
       `(("zlib" ,zlib)))
      (home-page "https://github.com/lh3/seqtk")
      (synopsis "Toolkit for processing sequences in FASTA/Q formats")
      (description
       "Seqtk is a fast and lightweight tool for processing sequences in
the FASTA or FASTQ format.  It seamlessly parses both FASTA and FASTQ
files which can also be optionally compressed by gzip.")
      (license (license:non-copyleft
                "file://src/LICENSE"
                "See src/LICENSE in the distribution.")))))

;; (define-public jalview ;;untested, likely doesn't work
;;   (package
;;    (name "jalview")
;;    (version "2.8.2")
;;    (source (origin
;;             (method url-fetch)
;;             (uri (string-append
;;                   "http://www.jalview.org/source/jalview_"
;;                   (regexp-substitute/global
;;                    #f "\\." version 'pre "_" 'post)
;;                   ".tar.gz"))
;;             (file-name (string-append name "-" version ".tar.gz"))
;;             (sha256
;;              (base32
;;               "12z7hqrqq3rccw6rgjc2gl9bnbkq4fnlw37267ax79mgdj15vi49"))))
;;    (build-system gnu-build-system)
;;    (native-inputs
;;     `(("jdk" ,icedtea6 "jdk") ;;TODO: this version of java needed specifically?
;;       ("perl" ,perl)))
;;    (home-page "http://www.jalview.org")
;;    (synopsis "Multiple sequence alignment editing, visualisation and analysis")
;;    (description
;;     "Use it to view and edit sequence alignments, analyse them with
;; phylogenetic trees and principal components analysis (PCA) plots and explore
;; molecular structures and annotation.  Jalview has built in DNA, RNA and protein
;; sequence and structure visualisation and analysis capabilities.  It uses Jmol to
;; view 3D structures, and VARNA to display RNA secondary structure.")
;;    (license license:gpl3+))) ;; TODO: check what version of GPL

(define-public python2-numexpr-1.4.1
  (package
    (inherit python2-numexpr)
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/"
                           "n/numexpr/numexpr-" version ".tar.gz"))
       (sha256
        (base32
         "0yvjmrf72lmr9dfnyla21aa5ckakl3wrpy3w6152k62w5b7g3ki7"))))))

(define-public mxscarna ; not free software (research only). Also fails to compile.
  (package
    (name "mxscarna")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.ncrna.org/software/mxscarna/mxscarna_ver"
                    version "_060309.tar.gz"))
              (sha256
               (base32
                "1ihg3s63hb1zshqpbyggbh9l64rj8yw19q2qvy2d9q75fvnql33q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (mkdir-p bin)
                      (copy-file "program/mxscarna"
                                 (string-append bin "mxscarna")))
                    #t)))))
    (home-page "http://www.ncrna.org/software/mxscarna")
    (synopsis
     "Fast structural multiple alignment of RNA sequences")
    (description
     "MXSCARNA (Multiplex Stem Candidate Aligner for RNAs) is a tool for fast
structural multiple alignment of RNA sequences using progressive alignment based
on pairwise structural alignment algorithm of SCARNA.")
    (license license:expat))) ; TODO: not free software, non-profit only, so meh.

(define-public trnascan-se ; unfinished
  (package
    (name "trnascan-se")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri "http://lowelab.ucsc.edu/software/tRNAscan-SE.tar.gz")
              (file-name (string-append name "-" version ".tgz"))
              (sha256
               (base32
                "05pkh8i6hn0qbybrxv7mdy4xdiw6rpa4fbx03c8iqga5d7c28ac6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build ; didn't try beyond here. Need to modify Makefile etc.
                  (lambda* (#:key input #:allow-other-keys)
                    (zero? (system* "gcc"
                                    "-O3"
                                    "-ffast-math"
                                    "-finline-functions"
                                    "-o"
                                    "aragorn"
                                    (string-append
                                     "aragorn" ,version ".c")))))
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append
                               (assoc-ref outputs "out") "/bin"))
                         (man (string-append
                               (assoc-ref outputs "out") "share/man/man1")))                   
                     (mkdir-p bin)
                     (copy-file "aragorn"
                                (string-append bin "/aragorn"))
                     (mkdir-p man)
                     (copy-file "aragorn.1"
                                (string-append man "/aragorn.1")))
                   #t)))))
    (home-page "http://lowelab.ucsc.edu/tRNAscan-SE/")
    (synopsis
     "")
    (description
     "")
   (license license:gpl2+)))

(define-public python-nesoni
  (package
    (name "python-nesoni")
    (version "0.130")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/n/nesoni/nesoni-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0lf94fxr1rcrysgldpiiq837x8mgpbphmg7zx3j67fg2lnhn489x"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2)) ; only Python 2 is supported
    (inputs
     `(("python-setuptools" ,python2-setuptools)
       ;; R Depends: limma, edgeR, Matrix, parallel
       ("r" ,r)
       
       ))
    (home-page
     "http://bioinformatics.net.au/software.nesoni.shtml")
    (synopsis
     "Tools for processing high-throughput sequencing data, with an emphasis on
bacterial data.")
    (description
     "Nesoni focusses on analysing the alignment of reads to a reference genome.
Use of the SHRiMP and Bowtie2 read aligners is automated by nesoni.  We use
SHRiMP as it is able to detect small insertions and deletions in addition to
SNPs.  Output from other aligners may be imported in SAM format.  Nesoni can
call a consensus of read alignments, taking care to indicate ambiguity.  This
can then be used in various ways: to determine the protein level changes
resulting from SNPs and indels, to find differences between multiple strains, or
to produce n-way comparison data suitable for phylogenetic analysis in
SplitsTree4.")
    (license license:gpl2+)))

(define-public shrimp
  (package
    (name "shrimp")
    (version "2.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://compbio.cs.toronto.edu/shrimp/releases/SHRiMP_"
                    (regexp-substitute/global
                     #f "\\." version 'pre "_" 'post)
                    ".src.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0flr32krdllyvrsx0ny5ibllvjjwvmbw1i2p6rdx52n9m230srk1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no make check
       #:make-flags (list ; as per BUILDING in src directory for gcc
                     "CXX=g++"
                     "CXXFLAGS=-O3 -mmmx -msse -msse2 -fopenmp")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin")))
                      (mkdir-p bin)
                      (copy-recursively "bin/" bin))
                    #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://compbio.cs.toronto.edu/shrimp/")
    (synopsis "A software package for aligning genomic reads against a target genome.")
    (description
     "SHRiMP2 is a software package for mapping reads from a donor genome
against a target (reference) genome.  SHRiMP2 was primarily developed to work
with short reads produced by Next Generation Sequencing (NGS) machines.")
   (license license:expat)))

(define-public spades ; possibly might compile, but there is bundled dependencies
  (package
    (name "spades")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://spades.bioinf.spbau.ru/release"
                                  version "/SPAdes-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a8kvrf7hpycxfnyspbyp4x47k8zshqv0b469jsfblwa0rfya13c"))))
    (build-system cmake-build-system)
    (inputs `(("zlib" ,zlib)))
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
                  (lambda* (#:key outputs #:allow-other-keys)
                    (setenv "PREFIX" (assoc-ref outputs "out"))
                    (zero? (system* "sh" "spades_compile.sh"))))
         (delete 'install))))
    (home-page "http://bioinf.spbau.ru/en/spades")
    (synopsis "A single-cell and isolate genome assembler")
    (description
     "SPAdes – St. Petersburg genome assembler – is intended for both standard
isolates and single-cell multiple displacement amplification (MDA) bacteria
assemblies.")
    (license license:gpl2)))

(define-public graftm
  (package
    (name "graftm")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/g/graftm/graftm-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wy4w2jvh6ip6ari0m55zvkyg3vnvsyn2l93n85d1d2xndbgns2v"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; python-2 only
       #:phases
       (modify-phases %standard-phases
         ;; current test in setup.py does not work so use nose to run tests
         ;; instead for now.
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)
       ("python-nose" ,python2-nose)))
    (inputs
     `(("python-biopython" ,python2-biopython)
       ("python-subprocess32" ,python2-subprocess32)
       ("python-biom-format" ,python2-biom-format)
       ("python-extern" ,python2-extern)
       ("python-scikit-bio" ,python2-scikit-bi)
       ("python-h5py" ,python2-h5py)
       ("python-tempdir" ,python2-tempdir)))
    (propagated-inputs
       ("orfm" ,orfm)
       ("hmmer" ,hmmer)
       ("diamond" ,diamond)
       ("fxtract" ,fxtract)
       ("fasttree" ,fasttree)
       ("krona-tools" ,krona-tools)
       ("pplacer" ,pplacer)
       ("seqmagick" ,seqmagick)
       ("taxtastic" ,taxtastic)
       ("mafft" ,mafft))
    (home-page "http://geronimp.github.com/graftM")
    (synopsis "Identify and classify metagenomic marker gene reads")
    (description
     "GraftM is a pipeline used for identifying and classifying marker gene
reads from large metagenomic shotgun sequence datasets.  It is able to find
marker genes using hidden Markov models or sequence similarity search, and
classify these reads by placement into phylogenetic trees")
    (license license:gpl3+)))

(define-public python-tempdir
  (package
    (name "python-tempdir")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tempdir" version))
       (sha256
        (base32
         "13msyyxqbicr111a294x7fsqbkl6a31fyrqflx3q7k547gnq15k8"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page
     "https://bitbucket.org/another_thomas/tempdir")
    (synopsis
     "Tempdirs are temporary directories, based on tempfile.mkdtemp")
    (description
     "Tempdirs are temporary directories, based on tempfile.mkdtemp")
    (license expat)
    (properties `((python2-variant . ,(delay python2-pytest-cache))))))

(define-public python2-tempdir
  (package-with-python2 (strip-python2-variant python-tempdir)))

(define-public taxtastic
  (package
    (name "taxtastic")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/taxtastic/taxtastic-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1g7fgnl367njdsk2xify9qh20dy63xzamf6w3bi74isgbhykq00h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python-sqlalchemy" ,python2-sqlalchemy)
       ("python-decorator" ,python2-decorator)
       ("python-biopython" ,python2-biopython)
       ("python-xlrd" ,python2-xlrd)))
    (inputs
     `(("python-setuptools" ,python2-setuptools)))
    (home-page "https://github.com/fhcrc/taxtastic")
    (synopsis
     "Tools for taxonomic naming and annotation")
    (description
     "Tools for taxonomic naming and annotation")
    (license license:gpl3)))

(define-public python2-extern ; could be sent to the mailing list. Does it work
                                        ; with python3 though? Probably, but
                                        ; would need to test the software. Also,
                                        ; does it run the tests at build time?
  (package
    (name "python2-extern")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/e/extern/extern-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0fc5s17nsz9dzgknkn18j6ib4w1cqhxw7m3vqqq0xv9w89gvfyj2"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; current test in setup.py does not work as of 0.9.4,
         ;; so use nose to run tests instead for now.
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)
       ("python-nose" ,python2-nose)))
    (home-page "https://github.com/wwood/extern")
    (synopsis "Subprocess-related functions for ease of use")
    (description "Extern is an opinionated version of Python's subprocess, making
it more convenient to run shell commands from within Python code.  For instance,
exceptions raised by an non-zero exit status include the STDOUT and STDERR in
the description of the error.")
    (license license:expat)))

(define-public python-pytest-timeout
  (package
    (name "python-pytest-timeout")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pytest-timeout/pytest-timeout-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wq6h4w7wdpahlga8wv6zx1qj1ni4vpdycx4lq750hwb2l342ay4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page
     "http://bitbucket.org/pytest-dev/pytest-timeout/")
    (synopsis
     "py.test plugin to abort hanging tests")
    (description
     "py.test plugin to abort hanging tests")
    (license license:expat)))

(define-public python2-pytest-timeout
  (package-with-python2 python-pytest-timeout))
 
(define-public python2-subprocess32
  (package
  (name "python-subprocess32")
  (version "3.2.6")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/s/subprocess32/subprocess32-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1xi0qb9b70kgwa2ks4d4kkib7dmb9i30rl6zf9rpwb5ys9pd9x6x"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2
     #:tests? #f)) ; no check, and nosetests fails
  (inputs
    `(("python-setuptools" ,python2-setuptools)
      ("python-nose" ,python2-nose)))
  (home-page
    "http://code.google.com/p/python-subprocess32/")
  (synopsis
    "Backport of the subprocess module from Python 3.2/3.3 for use on 2.x.")
  (description
    "Backport of the subprocess module from Python 3.2/3.3 for use on 2.x.")
  (license license:psfl)))

(define-public newick-utils ; seems to work for the C based tools, but appears
                            ; to be a dead project so I won't submit to guix
                            ; proper? Also need to test bindings to be a full
                            ; package definition. I don't use those though.
  (let ((commit "acb33ebdf")
        (revision "1"))
    (package
      (name "newick-utils")
      (version (string-append "1.6." revision "." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tjunier/newick_utils.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1rg71ffj4swb23y80bkm5jyvkr6p2v38n28xkwqidinvlqpjacbx"))))
    (build-system gnu-build-system)
    (arguments
     ;; disable lua components as they don't appear to compile.  See
     ;; https://github.com/tjunier/newick_utils/issues/13
     `(#:configure-flags '("--without-lua")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
                    (lambda _ (zero? (system* "autoreconf" "-vif")))))))
    (inputs
     `(;;("lua" ,lua-5.1)
       ("libxml2" ,libxml2)))
    ;; ("guile" ,guile-2.0))) ; TODO: get it to build the guile
                              ; editor. Currently fails to detect libguile.h
                              ; during configuration. Does this happen in
                              ; Ubuntu also?
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("flex" ,flex)
       ("bison" ,bison)))
    (synopsis "Programs for working with Newick-formatted phylogenetic trees")
    (description
     "A suite of utilities for processing phylogenetic trees in Newick format.
Functions include re-rooting, extracting subtrees, trimming, pruning,
condensing, drawing (ASCII graphics or SVG).")
    (home-page "https://github.com/tjunier/newick_utils")
    (license license:bsd-3))))


(define-public idba
  (package
    (name "idba")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/loneknightpy/idba/releases/download/"
                    version "/idba-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1220iy4rhcv7nhryq4x4zdcw7grxil4vz4k8lqihy0vw3j73p3mn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/")))
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin))
                         (find-files "bin" (lambda (file stat)
                                             (executable-file? file))))))))))
    (home-page "http://i.cs.hku.hk/~alse/hkubrg/projects/idba_ud/")
    (synopsis "Basic iterative de Bruijn graph assembler")
    (description
     "IDBA is an iterative de Bruijn graph assember for second generation
sequencing reads.  IDBA-UD, an extension of IDBA, is designed to utilize
paired-end reads to assemble low-depth regions and use progressive depth on
contigs to reduce errors in high-depth regions.  It is a generic purpose
assembler and especially good for single-cell and metagenomic sequencing
data. IDBA-Hybrid is another update version of IDBA-UD, which can make use of
a similar reference genome to improve assembly result.  IDBA-Tran is an
iterative de Bruijn graph assembler for RNA-Seq data.")
    (license license:gpl2+)))

(define-public maxbin ;; Works except for the heatmap functions. Requires r-gplots
  (package
    (name "maxbin")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/maxbin2/"
                       "MaxBin-" version ".tar.gz"))
       (sha256
        (base32 "1vs9267zi37ighhxiw390kl647mj2nn4p924dq20rn861mr6h6ad"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-script
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "run_MaxBin.pl"
              ;; remove unneeded include
              (("^use LWP::Simple;") "")
              ;; fix perl includes
              (("^require\\(\"\\$Bin\\\\/")
               "require(\"")
              ;; specify full dependency paths
              (("^my \\$BOWTIE2BUILD = \"bowtie2-build\";")
               (string-append "my $BOWTIE2BUILD = \""
                              (assoc-ref inputs "bowtie")
                              "/bin/bowtie2-build\";"))
              (("^my \\$BOWTIE2 = \"bowtie2\";")
               (string-append "my $BOWTIE2 = \""
                              (assoc-ref inputs "bowtie")
                              "/bin/bowtie2\";"))
              (("^my \\$HMMSEARCH = \"hmmsearch\";")
               (string-append "my $HMMSEARCH = \""
                              (assoc-ref inputs "hmmer")
                              "/bin/hmmsearch\";"))
              (("^my \\$RUNFRAG = \"run_FragGeneScan.pl\";")
               (string-append "my $RUNFRAG = \""
                              (assoc-ref inputs "fraggenescan")
                              "/bin/run_FragGeneScan.pl\";"))
              (("^my \\$IDBA_UD = \"idba_ud\";")
               (string-append "my $IDBA_UD = \""
                              (assoc-ref inputs "idba")
                              "/bin/idba_ud\";"))
              ;; fix paths to internals HMMs
              (("^my \\$MARKERHMM = \"\\$Bin/marker.hmm")
               (string-append "my $MARKERHMM = \""
                              (assoc-ref outputs "out")
                              "/share/MaxBin/marker.hmm"))
              (("\\$MARKERHMM = \"\\$Bin/bacar_marker.hmm\";")
               (string-append "$MARKERHMM = \""
                              (assoc-ref outputs "out")
                              "/share/MaxBin/bacar_marker.hmm\";"))
              ;; fix paths to internal programs
              (("^my \\$HEATMAP_R = \"\\$Bin\\\\/")
               (string-append "my $HEATMAP_R = \""
                              (assoc-ref outputs "out")
                              "/share/MaxBin/"))
              (("^my \\$MAXBIN = \"\\$Bin\\\\/src\\\\/")
               (string-append "my $MAXBIN = \""
                              (assoc-ref outputs "out")
                              "/bin/"))
              ;; remove ability to use settings file
              (("open\\(FILE, \"<\\$Bin\\\\/\\$SETTING_FILE\"\\);")
               "open(FILE, \"/dev/null\");")
              )
            #t))
         (replace 'build (lambda _ (list (chdir "src")
                                         (system* "make")
                                         (chdir ".."))
                                 #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/"))
                    (perl (string-append out "/lib/perl5/"))
                    (share (string-append out "/share/MaxBin/")))
               (mkdir-p bin)
               (install-file "run_MaxBin.pl" bin)
               (install-file "src/MaxBin" bin) 
               (mkdir-p share)
               (install-file "marker.hmm" share)
               (install-file "bacar_marker.hmm" share)
               (for-each (lambda (file) (install-file file perl))
                         (find-files "." ".*pm")))))
       (add-after 'install 'post-install-check
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* (string-append (assoc-ref outputs "out")
                                          "/bin/run_MaxBin.pl"))))))))
    ;; (propagated-inputs
    ;;  `(("r-gplots" ,r-gplots)))
    (inputs
     `(("fraggenescan" ,fraggenescan)
       ("bowtie" ,bowtie)
       ("hmmer" ,hmmer)
       ("idba" ,idba)
       ("perl" ,perl)))
    (home-page
     "http://downloads.jbei.org/data/microbial_communities/MaxBin/MaxBin.html")
    (synopsis "Binning metagenomic contigs by Expectation-Maximization")
    (description
     "MaxBin is software for binning assembled metagenomic sequences based on
an Expectation-Maximization algorithm.  Users can understand the underlying
bins (genomes) of the microbes in their metagenomes by simply providing
assembled metagenomic sequences and the reads coverage information or
sequencing reads.  For users' convenience MaxBin will report genome-related
statistics, including estimated completeness, GC content and genome size in
the binning summary page.")
    (license license:bsd-3)))


(define-public ruby-ds
  (package
    (name "ruby-ds")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ds" version))
       (sha256
        (base32
         "0d230mgyiyr0rc5jcr2dsxsn7vhh9y5vilsibb93yxw55f7ay8xa"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
     (synopsis
      "Data structures (lists,stacks, trees, heaps, graphs..) in pure Ruby.")
     (description
      "Data structures (lists,stacks, trees, heaps, graphs..) in pure Ruby.")
     (home-page "https://github.com/knife/ds")
     (license #f)))

(define-public ruby-bio-kseq
  (package
    (name "ruby-bio-kseq")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-kseq" version))
       (sha256
        (base32
         "1xyaha46khb5jc6wzkbf7040jagac49jbimn0vcrzid0j8jdikrz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (inputs
     `(("zlib" ,zlib)))
    (synopsis "Ruby bindings for the kseq.h FASTA/Q parser")
    (description
     "@code{Bio::Kseq} provides ruby bindings to the @code{kseq.h} FASTA and
FASTQ parsing code.  It provides a fast iterator over sequences and their
quality scores.")
    (home-page "https://github.com/gusevfe/bio-kseq")
    (license license:expat)))
