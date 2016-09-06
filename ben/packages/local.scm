(define-module (ben packages local)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
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
  #:use-module (gnu packages less)
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

(define-public spades ; there is bundled dependencies
  (package
    (name "spades")
    (version "3.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://spades.bioinf.spbau.ru/release"
                                  version "/SPAdes-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0agdf9mf0p350mi8cp55q0vlk7wsjrj38hrbh96mia2sjk2nlhvp"))))
    (build-system cmake-build-system)
    (inputs ;If you wish to use Lucigen NxSeq® Long Mate Pair reads, you will need Python regex library
     `(("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("python-wrapper" ,python-wrapper))) ; native-input ?
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
                    (setenv "PREFIX" (assoc-ref outputs "out"))
                    (zero? (system* "sh" "spades_compile.sh"))))
         (delete 'install)
         (delete 'check)
         (add-after 'install 'post-install-check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system*
                     (string-append (assoc-ref outputs "out") "/bin/spades.py")
                     "--test")))))))
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
    (version "0.9.5-dev")
    (source
     (local-file "/home/ben/git/graftM" #:recursive? #t))
     ;; (origin
     ;;   (method url-fetch)
     ;;   (uri (string-append
     ;;         "https://pypi.python.org/packages/source/g/graftm/graftm-"
     ;;         version
     ;;         ".tar.gz"))
     ;;   (sha256
     ;;    (base32
     ;;     "0wy4w2jvh6ip6ari0m55zvkyg3vnvsyn2l93n85d1d2xndbgns2v"))))
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
       ("python-h5py" ,python2-h5py)
       ("python-tempdir" ,python2-tempdir)))
    (propagated-inputs
     `(("orfm" ,orfm)
       ("hmmer" ,hmmer)
       ("diamond" ,diamond)
       ("fxtract" ,fxtract)
       ("fasttree" ,fasttree)
       ("krona-tools" ,krona-tools)
       ("pplacer" ,pplacer)
       ("seqmagick" ,seqmagick)
       ("taxtastic" ,taxtastic)
       ("mafft" ,mafft)))
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

;; This probably works, but I now realise I don't need to use it so I do not
;; wish to maintain it.
(define-public pagan
  (package
    (name "pagan")
    (version "20150723")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://wasabiapp.org/download/pagan/pagan.src."
                    version ".tgz"))
              (sha256
               (base32
                "0gy81zrlmmkkkp27lhq2k6jq05nfa7w9g80bnwrakd1vixargpk8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "-f" "Makefile.no_Qt") 
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bundled-boost
           (lambda _
             (delete-file-recursively "boost")
             #t))
         (delete 'configure)
         (add-before 'build 'setup-build
           (lambda _
             (chdir "src")
             (substitute* "Makefile.no_Qt"
               (("/usr/bin/g\\+\\+") "g++"))
             #t))
         (replace 'check
           (lambda _
             ;; There are no tests, instead just run one of the examples.
             (zero? (system* "./pagan" "--ref-seqfile"
                             "../examples/protein_placement/reference_aa.fas"
                             "--ref-treefile"
                             "../examples/protein_placement/reference_tree.nhx"
                             "--queryfile"
                             "../examples/protein_placement/input_aa_frags.fas"
                             "--outfile" "aa_frags_alignment" "--guided"
                             "--fragments"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "pagan" (string-append
                                    (assoc-ref outputs "out") "/bin"))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)))
    (propagated-inputs 
     ;; TODO: make these regular inputs by patching the source code, likely by
     ;; ensuring the test methods always return true, and the system call uses
     ;; the full path to the executable.
     `(("exonerate" ,exonerate)
       ("mafft" ,mafft)
       ("raxml" ,raxml)
       ("bppsuite" ,bpp-suite)))
    (home-page "")
    (synopsis "Probabilistic multiple sequence alignment program")
    (description
     "PAGAN is a general-purpose method for the alignment of sequence
graphs. PAGAN is based on the phylogeny-aware progressive alignment algorithm
and uses graphs to describe the uncertainty in the presence of characters at
certain sequence positions.  However, graphs also allow describing the
uncertainty in input sequences and modelling e.g. homopolymer errors in Roche
454 reads, or representing inferred ancestral sequences against which other
sequences can then be aligned.")
    ;; According to 
    (license license:gpl3+)))

(define-public r-permute
  (package
   (name "r-permute")
   (version "0.9-0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "permute" version))
     (sha256
      (base32
       "0w68cqw6s4pixix8bh1qzsy1pm64jqh1cjznw74h82ygp8sj7p73"))))
   (build-system r-build-system)
   ;(propagated-inputs `(("r-stats" ,r-stats)))
   (home-page
    "https://github.com/gavinsimpson/permute")
   (synopsis
    "Functions for Generating Restricted Permutations of Data")
   (description
    "This package provides a set of restricted permutation designs for freely exchangeable, line transects (time series), and spatial grid designs plus permutation of blocks (groups of samples) is provided. 'permute' also allows split-plot designs, in which the whole-plots or split-plots or both can be freely-exchangeable or one of the restricted designs.  The 'permute' package is modelled after the permutation schemes of 'Canoco 3.1' (and later) by Cajo ter Braak.")
   (license license:gpl2+)))

(define-public mummer ; potentially works, except that all the files need to be
                                        ; moved to the output directory before
                                        ; making, I think. Need also to remove
                                        ; crud afterwards, I guess. Gah. Have a
                                        ; look at the debian package, they seem
                                        ; to steal the code from somewhere else.
  
  (package
    (name "mummer")
    (version "3.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mummer/MUMmer"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bv6mwqg6imgyxga24xm1cb3mfs56zba485kxgmdiq6fv3vx9yhy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-paths
           (lambda _
             (substitute* "Makefile"
               ((" /bin/sh") (string-append " " (which "sh"))))
             (substitute* "scripts/Makefile"
               ((" /bin/sh") (string-append " " (which "sh")))))))))
               
    (inputs
     `(("perl" ,perl) ; dunno which level these are required at
       ("tcsh" ,tcsh)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:gpl3+))) ;fixme

(define-public kraken ; only half-complete
  (package
   (name "kraken")
   (version "0.10.5-beta")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/DerrickWood/kraken/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32
              "0bv6mwqg6imgyxga24xm1cb3mfs56zba485kxgmdiq6fv3vx9yhy"))))
   (build-system gnu-build-system)
   (home-page "")
   (synopsis "")
   (description
    "")
   (license license:gpl3+))) ;fixme

(define-public e-mem
  (package
    (name "e-mem")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri "http://www.csd.uwo.ca/%7Eilie/E-MEM/e-mem.zip")
              (sha256
               (base32
                "0cj6lf601y82an1rs9qvryad2q70kzz2wgjrf3rpyyirvlzqzkyw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "BIN_DIR=" %output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enter-source-directory
           (lambda _ (chdir "../e-mem_2") #t))
         (add-before 'build 'create-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             #t))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (symlink
              (string-append (assoc-ref outputs "out") "/bin/e-mem")
              "e-mem")
             (zero? (system* "./run_example"))))
         (delete 'install))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("boost" ,boost)))
    (home-page "http://www.csd.uwo.ca/%7Eilie/E-MEM/")
    (synopsis "Efficient computation of genomic maximal exact matches")
    (description
     "E-MEM is a C++/OpenMP program designed to efficiently compute @dfn{Maximal
Exact Matches} MEMs between large genomes.  It can be used as a stand alone
application or a drop in replacement for MUMmer3.")
    (license license:gpl3+)))


(define-public glimmerhmm
  (package
    (name "glimmerhmm")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ccb.jhu.edu/pub/software/glimmerhmm/GlimmerHMM-"
                    version ".tar.gz"))
              (sha256
               (base32
                "09q7cp8ccdyczdi5r7nbvis7fx7vs7fyijslh7bs6jcz5dwj3qs3"))))
>>>>>>> refs/remotes/origin/master
    (build-system gnu-build-system)
    (home-page "")
    (synopsis "")
    (description
     "")
<<<<<<< HEAD
    (license license:gpl3+))) ;fixme
=======
    (license license:gpl3+))) ;?


(define-public panphlan
  ;; The newest release 1.2 is far out of date, so we package from a new
  ;; changeset.
  (let ((changeset "e113395491e6e9d353aa157333659f60e98de168"))
    (package
      (name "panphlan")
      (version (string-append "1.2-1." (string-take changeset 7)))
      (source (origin
                (method url-fetch)
                ;; Use the direct download rather than hg-download so that
                ;; sub-repositories are not downloaded.
                (uri (string-append
                      "https://bitbucket.org/CibioCM/panphlan/get/"
                      (string-take changeset 12) ".zip"))
                (file-name (string-append name "-" version ".zip"))
                (sha256
                 (base32
                  "0d51q5ls8igddwjf67cf0fw62aaxylm57s3c5lb98v3bn48p8xw0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'check
             ;; There are no tests, so we run a quick sanity check.
             (lambda _
               (zero? (length (filter (lambda (file)
                                        (not (zero? (system*
                                                     (string-append "./" file)
                                                     "-h"))))
                                      (find-files "." ".*py"))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (bin  (string-append out "/bin"))
                      (path (getenv "PATH"))
                      (pythonpath (getenv "PYTHONPATH")))
                 (for-each (lambda (file)
                             (install-file file bin)
                             (wrap-program (string-append bin "/" file)
                               `("PATH" ":" prefix (,path)))
                             (wrap-program (string-append bin "/" file)
                               `("PYTHONPATH" ":" prefix (,pythonpath))))
                           (find-files "." ".*py")))
               #t)))))
      (native-inputs
       `(("unzip" ,unzip)))
      (inputs
       `(("python" ,python-wrapper)
         ("bowtie" ,bowtie)
         ("samtools" ,samtools)
         ("python-numpy" ,python-numpy)
         ("python-biopython" ,python-biopython)))
      (home-page "http://segatalab.cibio.unitn.it/tools/panphlan/")
      (synopsis "Strain-level metagenomic profiling tool")
      (description
       "PanPhlAn is a strain-level metagenomic profiling tool for identifying
the gene composition and in-vivo transcriptional activity of individual strains
in metagenomic samples.  Its strain-tracking and functional analysis of unknown
pathogens makes it useful for culture-free infectious outbreak epidemiology and
microbial population studies.")
      (license license:expat))))


(define-public barrnap
  (package
   (name "barrnap")
   (version "0.7")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/tseemann/barrnap/archive/"
            version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "16y040np76my3y82hgk4yy790smbsk4h8d60d5swlv7ha3i768gg"))
      (modules '((guix build utils)))
      ;; Remove pre-built binaries.
      (snippet '(begin
                  (delete-file-recursively "binaries")
                  #t))))
   (build-system gnu-build-system)
   (arguments
    `(#:test-target "test"
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-nhmer-path
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "bin/barrnap"
              (("^my \\$NHMMER = .*")
               (string-append "my $NHMMER = '"
                              (assoc-ref inputs "hmmer")
                              "/bin/nhmmer';\n")))
            #t))
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (bin  (string-append out "/bin"))
                   (path (getenv "PATH"))
                   (file "barrnap"))
              (install-file (string-append "bin/" file) bin)
              (wrap-program (string-append bin "/" file)
                            `("PATH" ":" prefix (,path)))
              (copy-recursively "db" out))
            #t)))))
   (inputs
    `(("perl" ,perl)
      ("hmmer" ,hmmer)))
   (home-page "https://github.com/tseemann/barrnap")
   (synopsis "Bacterial ribosomal RNA predictor")
   (description
    "Barrnap predicts the location of ribosomal RNA genes in genomes.  It
supports bacteria (5S, 23S, 16S), archaea (5S,5.8S,23S,16S), mitochondria (12S,
16S) and eukaryotes (5S, 5.8S, 28S, 18S).  It takes FASTA DNA sequence as input,
and write GFF3 as output.  It uses the NHMMER tool that comes with HMMER 3.1 for
HMM searching in RNA:DNA style.")
   (license (list license:gpl3
                  ;; The Rfam HMMs are under cc0, and the SILVA-derived HMMs are
                  ;; academic-only.
                  license:cc0
                  (license:non-copyleft
                   "file:///LICENSE.SILVA"
                   "See LICENSE.SILVA in the distribution.")))))

;;; Cannot be included in Guix proper as only a binary is distributed.
(define-public tbl2asn
  (package
    (name "tbl2asn")
    (version "25.0") ; The version can be found by running through "tbl2asn -".
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.ncbi.nih.gov/toolbox/ncbi_tools/converters"
             "/by_program/tbl2asn/linux64.tbl2asn.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0n4fajcqpz235d23razs691l50njbsqn8amlis8fnarbhz1cd58g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (copy-file source "tbl2asn.gz")
             (system* "gunzip" "tbl2asn.gz")
             (chmod "tbl2asn" #o555)
             #t))
         (delete 'configure)
         (delete 'build)
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((so (string-append
                       (assoc-ref inputs "libc")
                       "/lib/ld-"
                       ,(package-version glibc)
                       ".so")))
               (display so)(display "\n")
               (and
                (zero? (system* "patchelf" "--set-interpreter" so "tbl2asn"))
                (zero? (system* "./tbl2asn" "-"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file
              "tbl2asn"
              (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (native-inputs
     `(("patchelf" ,patchelf)))
    ;; Binaries are available for other systems, but only x86_64 is packaged
    ;; here.
    (supported-systems '("x86_64-linux"))
    (home-page "http://www.ncbi.nlm.nih.gov/genbank/tbl2asn2/")
    (synopsis "Submission creator for GenBank")
    (description
     "Tbl2asn is a command-line program that automates the creation of sequence
records for submission to GenBank.  It uses many of the same functions as Sequin
but is driven generally by data files.  Tbl2asn generates .sqn files for
submission to GenBank.  Additional manual editing is not required before
submission.")
    (license license:public-domain))) ; This may be incorrect as I cannot find
                                      ; any direct license information.

(define-public prokka
  ;; There has been many commits since the last released version 1.11 so we
  ;; package from git.
  (let ((commit "460a152abb219d6e2b72f625a8547f2658f68fce"))
    (package
      (name "prokka")
      (version (string-append "1.11-1." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tseemann/prokka.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1jfk7w1jvzjq9qmhraxh4mdzjk3canqb41q4gpnray2lrn0mz4zj"))
         (modules '((guix build utils)))
         ;; Remove bundled code.
         (snippet '(begin
                     (delete-file-recursively "binaries")
                     (delete-file-recursively "perl5")
                     #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               (zero? (system* "bin/prokka" "--setupdb"))))
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys)
               (zero? (system* "bin/prokka"
                               "--noanno"
                               "--outdir" "example-out"
                               (assoc-ref inputs "example-genome")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (path (getenv "PATH"))
                      (perl5lib (getenv "PERL5LIB")))
                 (copy-recursively "db" (string-append out "/db"))
                 (copy-recursively "bin" (string-append out "/bin"))
                 (for-each
                  (lambda (prog)
                    (let ((binary (string-append out "/" prog)))
                      (wrap-program binary
                        `("PERL5LIB" ":" prefix
                          (,(string-append perl5lib ":" out
                                           "/lib/perl5/site_perl"))))
                      (wrap-program binary
                        `("PATH" ":" prefix
                          (,(string-append path ":" out "/bin")))))
                    #t)
                  (find-files "bin" ".*")))
               #t)))))
      (native-inputs
       `(("example-genome"
          ,(origin
             (method url-fetch)
             (uri "http://www.ebi.ac.uk/ena/data/view/CP002565&display=fasta")
             (file-name (string-append "ena-genome-CP002565.fasta"))
             (sha256
              (base32
               "0dv3m29kgyssjc96zbmb5khkrk7cy7a66bsjk2ricwc302g5hgfy"))))))
      (inputs
       `(("perl" ,perl)
         ("bioperl" ,bioperl-minimal)
         ("blast+" ,blast+)
         ("hmmer" ,hmmer)
         ("aragorn" ,aragorn)
         ("prodigal" ,prodigal)
         ("parallel" ,parallel)
         ("infernal" ,infernal)
         ("barrnap" ,barrnap)
         ("minced" ,minced)
         ("tbl2asn" ,tbl2asn)
         ("grep" ,grep)
         ("sed" ,sed)
         ("less" ,less)
         ("perl-time-piece" ,perl-time-piece)
         ("perl-xml-simple" ,perl-xml-simple)
         ("perl-digest-md5" ,perl-digest-md5)))
      (home-page "http://www.vicbioinformatics.com/software.prokka.shtml")
      (synopsis "Rapid prokaryotic genome annotation")
      (description
       "Prokka is a software tool for the rapid annotation of prokaryotic
genomes.  It produces GFF3, GBK and SQN files that are ready for editing in
Sequin and ultimately submitted to Genbank/DDJB/ENA. ")
      (license (list license:gpl2
                     ;; Available under various licenses.
                     (license:non-copyleft
                      "file://doc"
                      "See license files in the doc directory."))))))

(define-public perl-time-piece
  (package
    (name "perl-time-piece")
    (version "1.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ES/ESAYM/Time-Piece-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1fb7s5y9f3j80h2dfsgplmdcrhp96ccqs0qqabmckkkgvhj40205"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Time-Piece")
    (synopsis "Object Oriented time objects")
    (description "This module replaces the standard @code{localtime} and
@code{gmtime} Perl functions with implementations that return objects.  It does
so in a backwards compatible manner, so that using
@code{localtime}/@code{gmtime} in the way documented in perlfunc will still
return what you expect.")
    (license (package-license perl))))

(define-public perl-digest-md5
  (package
    (name "perl-digest-md5")
    (version "2.55")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/G/GA/GAAS/Digest-MD5-"
             version ".tar.gz"))
       (sha256
        (base32
         "0g0fklbrm2krswc1xhp4iwn1dhqq71fqh2p5wm8xj9a4s6i9ic83"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Digest-MD5")
    (synopsis "Perl interface to the MD5 algorithm")
    (description "The @code{Digest::MD5} module allows you to use the RSA Data
Security Inc. MD5 Message Digest algorithm from within Perl programs.  The
algorithm takes as input a message of arbitrary length and produces as output a
128-bit 'fingerprint' or 'message digest' of the input.")
    (license (package-license perl))))

(define-public python2-mgkit
  (package
    (name "python2-mgkit")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mgkit" version))
       (sha256
        (base32
         "1y7j4s1x59z6j0lfkd99psf44rrlkvyrmkli68fapbx7ankmxcdw"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2)) ; Ony Python 2 is supported.
    (native-inputs
     `(("python-nose" ,python2-nose)
       ("python-yanc" ,python2-yanc)))
    (inputs
     `(("htseq" ,htseq)
       ("python-enum34" ,python2-enum34)
       ("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("python-scipy" ,python2-scipy)
       ("python-setuptools" ,python2-setuptools)
       ;("python-semiidbm" ,python2-semiidbm) ; TODO: package semiidbm.
       ("python-pymongo" ,python2-pymongo)
       ("python-rpy2" ,python2-rpy2)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-msgpack" ,python2-msgpack)
       ("python-pandas" ,python2-pandas)))
    (home-page "https://bitbucket.org/setsuna80/mgkit/")
    (synopsis "Metagenomics Framework")
    (description "provide a series of useful modules and packages to make it
easier to build custom pipelines for metagenomics or any kind of bioinformatics
analysis.  It integrates other well known python libraries in bioinformatics,
like HTSeq, pysam, numpy and scipy.")
    (license license:gpl2+)))

(define-public python-yanc
  (package
    (name "python-yanc")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yanc" version))
       (sha256
        (base32
         "0z35bkk9phs40lf5061k1plhjdl5fskm0dmdikrsqi1bjihnxp8w"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/0compute/yanc")
    (synopsis "Yet another nose colorer")
    (description "YANC is a color output plugin for nose that plays nicely with
other python packages.  To enable the plugin pass @code{--with-yanc} to
@code{nosetests}.")
    (license license:gpl3+)))

(define-public python2-yanc
  (package-with-python2 python-yanc))
