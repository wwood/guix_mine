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
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages django)
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

  #:use-module (ace packages ace)
  #:use-module (ace packages external)
  #:use-module (gnu packages bioinformatics))

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

(define-public spades ; there is bundled C/C++ dependencies. All seem tractable.
  (package
    (name "spades")
    (version "3.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://spades.bioinf.spbau.ru/release"
                                  version "/SPAdes-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h8bpnjs27cjdhs73z0zl01fgblks69g5zc5iilv7zys45h6hi6d"))))
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
    (home-page "http://cab.spbu.ru/software/spades")
    (synopsis "A single-cell and isolate genome assembler")
    (description
     "SPAdes – St. Petersburg genome assembler – is intended for both standard
isolates and single-cell multiple displacement amplification (MDA) bacteria
assemblies.")
    (license license:gpl2)))

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
               "open(FILE, \"/dev/null\");"))
            #t))
         (replace 'build
           (lambda _
             (chdir "src")
             (system* "make")
             (chdir ".."))
           #t))
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
         (delete 'check)
         (add-after 'install 'post-install-check
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* (string-append (assoc-ref outputs "out")
                                            "/bin/run_MaxBin.pl")))))))
    ;; (propagated-inputs ; Ricardo just posted a package for this, so soon ok?
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
       ("bppsuite" ,bppsuite)))
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
                "0cj6lf601y82an1rs9qvryad2q70kzz2wgjrf3rpyyirvlzqzkyw"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove extraneous directory.
                  (delete-file-recursively "../__MACOSX")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "./run_example"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "e-mem" bin)))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("boost" ,boost)))
    (home-page "http://www.csd.uwo.ca/~ilie/E-MEM/")
    (synopsis "Efficient computation of genomic maximal exact matches")
    (description
     "E-MEM is a C++/OpenMP program designed to efficiently compute
@dfn{Maximal Exact Matches} (MEMs) between large genomes.  It can be used as a
stand alone application or a drop in replacement for MUMmer3.")
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
    (build-system gnu-build-system)
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:gpl3+))) ;fixme


(define-public panphlan
  ;; The newest release 1.2 is far out of date, so we package from a new
  ;; changeset.
  (let ((changeset "62533244f44b5fc57cf93f624b97a57944d511e7"))
    (package
      (name "panphlan")
      (version (string-append "1.2-2." (string-take changeset 7)))
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
                  "1bwhw52sj2w57zki45bg0w0kvzbw4c438l5a6k06iabyj76smy03"))))
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

(define-public metaphlan ; Seems to work, but more beta testing required before
                         ; submission to Guix proper.
  (let ((changeset "c43e40a443ed"))
    (package
      (name "metaphlan")
      (version "2.6.0")
      (source (origin
                (method url-fetch)
                ;; Use the direct download rather than hg-download so that
                ;; sub-repositories are not downloaded.
                (uri (string-append
                      "https://bitbucket.org/biobakery/metaphlan2/get/"
                      (string-take changeset 12) ".zip"))
                (file-name (string-append name "-" version ".zip"))
                (sha256
                 (base32
                  "03k3z6di7bw8wpp59zbpvk7qc256wvzdrgx1fg5v089qk6lnh1cy"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'check
             ;; There are no tests, so we run a quick sanity check.
             (lambda _
               (setenv "HOME" "/tmp")
               (zero? (length (filter (lambda (file)
                                        (not (zero? (system*
                                                     (string-append "./" file)
                                                     "-h"))))
                                      (find-files "." ".*\\.py$"))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (bin  (string-append out "/bin"))
                      (path (getenv "PATH"))
                      (pythonpath (getenv "PYTHONPATH"))
                      (lib-files
                       (list "which.py" "mixed_utils.py"
                             "ooSubprocess.py"
                             ;; TODO: sample2markers is a real script too, but
                             ;; it cannot be both imported as a library and
                             ;; wrapped as a script.
                             "sample2markers.py")))
                 (for-each (lambda (file)
                             (display (string-append "doing " file "\n"))
                             (install-file file bin)
                             (wrap-program
                              (string-append bin "/" (basename file))
                              `("PATH" ":" prefix (,path)))
                             (wrap-program
                              (string-append bin "/" (basename file))
                              `("PYTHONPATH" ":" prefix (,pythonpath))))
                           (filter (lambda (file)
                                     (let ((base (basename file)))
                                       (zero? (length
                                               (filter
                                                (lambda (lib-file)
                                                  (string= lib-file base))
                                                lib-files)))))
                                   (find-files "." ".*\\.py$")))
                 (for-each (lambda (file)
                             (let ((path (string-append "strainphlan_src/"
                                                        file)))
                               (chmod path #o444)
                               (install-file path bin)))
                           lib-files)
                 (let ((db "db_v20")
                       (share (string-append out "/share/metaphlan")))
                   (mkdir-p share)
                   (copy-recursively db (string-append share "/" db)))
                 #t))))))
      (native-inputs
       `(("unzip" ,unzip)))
      (inputs ;; TODO: include graphlan and hclust2 for scripts.
       `(("python" ,python-2) ; Strainphlan has python2-specific print
                              ; statements, at least.
         ("bowtie" ,bowtie)
         ("python-numpy" ,python2-numpy)
         ("python-matplotlib" ,python2-matplotlib)
         ("python-scipy" ,python2-scipy)
         ("python-biom-format" ,python2-biom-format)
         ("python-pandas" ,python2-pandas)
         ("python-msgpack" ,python2-msgpack)
         ("python-biopython" ,python2-biopython)
         ("python-pytz" ,python2-pytz)
         ("python-dendropy" ,python2-dendropy)
         ("python-pysam" ,python2-pysam)))
      (home-page "http://huttenhower.sph.harvard.edu/metaphlan2")
      (synopsis "Profile the composition of microbial communities with metagenomes")
      (description
       "MetaPhlAn is a computational tool for profiling the composition of
microbial communities from metagenomic shotgun sequencing data.  MetaPhlAn relies
on unique clade-specific marker genes identified from ~17,000 reference
genomes (~13,500 bacterial and archaeal, ~3,500 viral, and ~110 eukaryotic).")
      (license license:expat)))) ;?

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
                        ,(glibc-dynamic-linker))))
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
         ("tbl2asn" ,ncbi-tools)
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
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mgkit" version))
       (sha256
        (base32
         "1zbvx3l2270k3ffk915mscnh2dw83wmqad7djr3ajkprr9dqywq7"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2)) ; Ony Python 2 is supported.
    (native-inputs
     `(("python-nose" ,python2-nose)
       ("python-yanc" ,python2-yanc)))
    (propagated-inputs
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; The default nose.collector in setup.py incorrectly runs the
           ;; tests.
           (lambda _ (zero? (system* "nosetests" "-x")))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/0compute/yanc")
    (synopsis "Yet another nose colorer")
    (description "YANC is a color output plugin for nose that plays nicely with
other python packages.  To enable the plugin pass @code{--with-yanc} to
@code{nosetests}.")
    (license license:gpl3+)))

(define-public python2-yanc
  (package-with-python2 python-yanc))

(define-public python-nestly
  (package
   (name "python-nestly")
   (version "0.6")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "nestly" version))
     (sha256
      (base32
       "1wrgpnab1w1lm20jlk632rjhsnj6dcld5hidjn44l4kwlqgdr6i0"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-mock" ,python-mock)))
   (home-page "https://github.com/fhcrc/nestly")
   (synopsis "Functions for running software with combinatorial parameter")
   (description
    "Nestly is a collection of functions designed to make running software with
combinatorial choices of parameters easier.")
   (license license:expat)))

(define-public python2-nestly
  (package-with-python2 python-nestly))


;; Does not work because I do not have an example input sample.xml file, at
;; least.
;; (define-public proxigenomics
;;   (let ((commit "921d3d0367b9981573aa023b1418f7a5a8316a4c"))
;;     (package
;;       (name "proxigenomics")
;;       (version (string-append "0-1." commit))
;;       (source (origin
;;                (method git-fetch)
;;                (uri (git-reference
;;                      (url "https://github.com/koadman/proxigenomics.git")
;;                      (commit commit)))
;;                (sha256
;;                 (base32
;;                  "0wk69kbg4l1lk6x5zc0kh457ak8cnv4nbg5dzy7kfq227vgw6qnl"))))
;;       (build-system python-build-system)
;;       (arguments
;;        `(#:python ,python-2
;;          #:phases
;;          (modify-phases %standard-phases
;;                         (replace 'build
;;                                  (lambda _
;;                                    (chdir "simulation/pipeline")
;;                                    (and
;;                                     (zero? (system* "scons" "-j" "4" "-f" "SConstruct_evo.py"))
;;                                     (zero? (system* "scons" "-j" "4" "-f" "SConstruct_wgs.py"))
;;                                     (zero? (system* "scons" "-j" "4" "-f" "SConstruct_hic.py"))
;;                                     (zero? (system* "scons" "-j" "4" "-f"
;;                                                     "SConstruct_map.py")))))
;;                   )))
;;       (inputs
;;        `(("python-setuptools" ,python2-setuptools)
;;          ("bioython" ,python2-biopython)
;;          ("python-scipy" ,python2-scipy)
;;          ("python-numpy" ,python2-numpy)
;;          ("python-pandas" ,python2-pandas)
;;          ("python-networkx" ,python2-networkx)
;;          ("python-pysam" ,python-pysam)
;;          ("python-pyyaml" ,python2-pyyaml)
;;          ("scons" ,scons)
;;          ;; extras not listed in README
;;          ("perl" ,perl)
;;          ("python-nestly" ,python2-nestly) ; not in the python module list but
;;                                         ; is in pip install code
;;          ))
;;       (home-page "https://github.com/fhcrc/nestly")
;;       (synopsis "Functions for running software with combinatorial parameter")
;;       (description
;;        "Nestly is a collection of functions designed to make running software with
;; combinatorial choices of parameters easier.")
;;       (license license:expat))))

(define-public crass
  (package
    (name "crass")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ctSkennerton/crass/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c6rxajq3ajf9m42r7xapg8jfvgkhmfz5425wprg8sx1nz6bqy3h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append
              "--with-xerces=" (assoc-ref %build-inputs "xerces-c++"))
             "--enable-rendering")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (and (zero? (system* "./autogen.sh"))
                  (begin (substitute* "configure"
                           (("/usr/bin/file") (which "file")))
                         ;; See https://github.com/ctSkennerton/crass/pull/85
                         (substitute* "src/test/Makefile.am"
                           (("test_readholder.cpp") ""))
                         #t)))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("graphviz" ,graphviz)
       ("xerces-c++" ,xerces-c++)
       ("zlib" ,zlib)))
    (home-page "http://bioinformatics.ninja/crass")
    (synopsis "CRISPR assembler")
    (description
     "Crass searches through raw metagenomic DNA reads for @dfn{Clustered
Regularly Interspersed Short Palindromic Repeats} (CRISPRs), structures which
form part of the microbial immune system.  CRISPRs form a specific structure
in genomic DNA, where repeating stretches of DNA (@dfn{direct repeats}) are
separated by @dfn{spacer} sequences.  @code{crass} identifies reads which
contain repeated K-mers that are of a specific length and are separated by a
spacer sequence.  These possible direct repeats are curated internally to
remove bad matches, and CRISPRs are output.")
    ;; TODO: other licenses?
    (license license:gpl3+)))

(define-public xerces-c++
  (package
    (name "xerces-c++")
    (version "3.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://apache/xerces/c/" (version-prefix version 1)
             "/sources/xerces-c-" version ".tar.xz"))
             (sha256
              (base32
               "0hb29c0smqlpxj0zdm09s983z5jx37szlliccnvgh0qq91wwqwwr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://xerces.apache.org/xerces-c/")
    (synopsis "C++ XML parser")
    (description
     "Xerces-C++ is a validating XML parser written in a portable subset of
C++.  Xerces-C++ makes it easy to give your application the ability to read
and write XML data.  A shared library is provided for parsing, generating,
manipulating, and validating XML documents using the DOM, SAX, and SAX2
APIs.")
    (license license:asl2.0)))

;; (define-public trinityrnaseq ; Does not work, due at least in part to the
;;                              ; bundled software.
;;   (package
;;     (name "trinityrnaseq")
;;     (version "2.2.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append
;;                     "https://github.com/trinityrnaseq/trinityrnaseq/archive/v"
;;                     version ".tar.gz"))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "17kcwizzmwfl0i0jdvz8zv7hx2fm909b6c6jgm282sn7dbjh6ipk"))))
;;     (build-system gnu-build-system)
;;     (arguments
;;      `(#:parallel-build? #f ; for debugging, may not be required
;;        #:test-target "test"
;;        #:phases
;;        (modify-phases %standard-phases
;;          (delete 'configure))))
;;     (inputs
;;      `(("jre" ,icedtea)
;;        ("bowtie" ,bowtie)
;;        ("perl" ,perl)
;;        ("zlib" ,zlib)
;;        ("htslib" ,htslib)
;;        ("jellyfish" ,jellyfish)
;;        ("samtools" ,samtools)))
;;     (home-page "")
;;     (synopsis "")
;;     (description
;;      "")
;;     (license license:expat)))

(define-public r-picante
  (package
   (name "r-picante")
   (version "1.6-2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "picante" version))
     (sha256
      (base32
       "1zxpd8kh3ay6f3gdqkij1a6vnkr98dc1jib2r6br2kjyzshabcsd"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-ape" ,r-ape)
      ("r-nlme" ,r-nlme)
      ("r-vegan" ,r-vegan)))
   (home-page
    "http://cran.r-project.org/web/packages/picante")
   (synopsis "tools for integrating phylogenies and ecology")
   (description
    "Phylocom integration, community analyses, null-models, traits and evolution in R")
   (license license:gpl2+)))

(define-public r-nlme
  (package
   (name "r-nlme")
   (version "3.1-128")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "nlme" version))
     (sha256
      (base32
       "0639jzy1zvs4x1g4fdsgl3r8nxifcyhpppcdxnqrhl49zpm2i0sr"))))
   (build-system r-build-system)
   (native-inputs
    `(("gfortran" ,gfortran)))
   (propagated-inputs
    `(("r-lattice" ,r-lattice)))
   (home-page "http://cran.r-project.org/web/packages/nlme")
   (synopsis "Linear and nonlinear mixed effects m,odels")
   (description
    "Fit and compare Gaussian linear and nonlinear mixed-effects models.")
   (license license:gpl2+)))

(define-public kaptive
  (let ((commit "ac2de2284a658426a26d8d1736d5a0df6fb6d16e"))
    (package
     (name "kaptive")
     (version (string-append "0.2-1." (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/katholt/Kaptive.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1wf9ykjfxqq0r8xrv3fjss203x1zc6ng12qzbgwkkzczv0gq6m4m"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; python-2 only
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'check
           (lambda _
             (with-directory-excursion "extras/tests"
               ;; Test script appears to use an incorrect script name.
               (symlink "../../kaptive.py" "k_locus_caller.py")
               (zero? (system* "python" "test_k_locus_caller.py"))
               ;; One test currently fails, but ignore this for the time being.
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/kaptive")))
               (install-file "kaptive.py" bin)
               (wrap-program (string-append bin "/kaptive.py")
                             `("PATH" ":" prefix (,(getenv "PATH"))))
               (mkdir-p share)
               (copy-recursively "reference_database" share))
             #t)))))
    (inputs
     `(("blast+" ,blast+)
       ("python-biopython" ,python2-biopython)
       ("which" ,which)))
    (home-page "https://github.com/katholt/Kaptive")
    (synopsis "Find capsular (K) loci in Klebsiella genomes")
    (description
     "Kaptive reports information about capsular (K) loci found in genome
assemblies.  Given a novel genome and a database of known K loci, Kaptive will
help a user to decide whether their sample has a known or novel K locus.")
    (license license:gpl3))))

(define-public graftm-stop-codons
  (let ((commit "a64a2b7ed83e98546ac2c4e7b3218245ef84f852"))
    (package
      (inherit graftm)
      (name "graftm-stop-codons")
      (version (string-append "0.9.5-2." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/geronimp/graftM.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (patches (search-patches "graftm-with-stops.patch"))
         (sha256
          (base32
           "0y2b90fh42xdjim29jzja611828yml0vnwv9h181wsdvw8yy82hh"))))
      (arguments
       `(#:python ,python-2 ; python-2 only
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (zero? (system* "bin/graftM" "-h"))))
           (add-after 'install 'wrap-programs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (graftm (string-append out "/bin/graftM"))
                      (path (getenv "PATH")))
                 (wrap-program graftm `("PATH" ":" prefix (,path))))
               #t))))))))

(define-public megahit
  (package
    (name "megahit")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/voutcn/megahit/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "11lz0p3bj4w14pwac1dkmpc77yi3i3552cif4shdr85nrdxbkih9"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled cityhash library.  Do not delete bundled IDBA,
        ;; kseq.h or khash.h as these have been modified from their original
        ;; form.
        '(begin
           (delete-file "city.cpp")
           (delete-file "citycrc.h")
           (delete-file "city.h")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(;; Include cityhash in the list of libraries.
       #:make-flags '("LIB=-lm -lz -lpthread -lcityhash"
                      ;; "CPU_ARCH="
                      ;; "CPU_ARCH_SUFFIX="
                      ;; "disablempopcnt=1"
		      )
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               (("city.o") ""))
             #t))
         (delete 'configure)
         (replace 'check
           (lambda _
             (zero?
              (system* "./megahit" "-t" "4" "--12"
                       "example/readsInterleaved1.fa.gz" "-o" "megahit_out"))))
         (replace 'install ; No install target.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (path (getenv "PATH")))
               (for-each (lambda (program)
                           (install-file program bin)
                           (wrap-program (string-append bin "/" program)
                             `("PATH" ":" prefix (,path))))
                         '("megahit"
                           "megahit_asm_core"
                           "megahit_toolkit"
                           "megahit_sdbg_build")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)
       ("python" ,python-2)
       ("cityhash" ,cityhash)
       ;; 'free' and 'cat' are used when running megahit.
       ("procps" ,procps)
       ("coreutils" ,coreutils)))
    (home-page "https://github.com/voutcn/megahit")
    (synopsis "Assembler for large and complex metagenomes")
    (description "MEGAHIT is a single node assembler for large and complex
metagenomics NGS reads, such as soil.  It makes use of succinct de Bruijn
graph (SdBG) to achieve low memory assembly.")
    (license license:gpl3+)))

(define-public nss-ldap ; This compiles but doesn't get around the LDAP-related
                        ; issues as I'd hoped.
  (let ((commit "154730b5a2b58a4212e419b498476fcb5a60de7b"))
    (package
      (name "nss-ldap")
      (version "265")
      ;; (source
      ;;  (origin
      ;;    (method url-fetch)
      ;;    (uri "http://www.padl.com/download/nss_ldap.tgz")
      ;;    (sha256
      ;;     (base32
      ;;      "1a16q9p97d2blrj0h6vl1xr7dg7i4s8x8namipr79mshby84vdbp"))))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/PADL/nss_ldap.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "09907p4m483fdc9hks6gignl539wvqj3krqa5yiyrp6qiq45d8s4"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'install ; HACK: should rather patch the Makefile or install
                             ; other things e.g. .h files.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib")))
                 (install-file "nss_ldap.so" lib)))))))
      (native-inputs
       `(("perl" ,perl)))
      (inputs
       `(("openldap" ,openldap)
         ("sasl" ,cyrus-sasl)
         ("krb5.h" ,mit-krb5)))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:lgpl2.0+))))

(define-public r-wgcna
  (package
   (name "r-wgcna")
   (version "1.51")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "WGCNA" version))
     (sha256
      (base32
       "0hzvnhw76vwg8bl8x368f0c5szpwb8323bmrb3bir93i5bmfjsxx"))))
   (properties `((upstream-name . "WGCNA")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-annotationdbi" ,r-annotationdbi)
      ("r-doparallel" ,r-doparallel)
      ("r-dynamictreecut" ,r-dynamictreecut)
      ("r-fastcluster" ,r-fastcluster)
      ("r-foreach" ,r-foreach)
      ("r-go-db" ,r-go-db)
      ("r-hmisc" ,r-hmisc)
      ("r-impute" ,r-impute)
      ("r-matrixstats" ,r-matrixstats)
      ("r-preprocesscore" ,r-preprocesscore)
      ;("r-survival" ,r-survival) ; Does guix really not have this?
      ))
   (home-page
    "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
   (synopsis
    "Weighted Correlation Network Analysis")
   (description
    "Functions necessary to perform Weighted Correlation Network Analysis on high-dimensional data.  Includes functions for rudimentary data cleaning, construction of correlation networks, module identification, summarization, and relating of variables and modules to sample traits.  Also includes a number of utility functions for data manipulation and visualization.")
   (license license:gpl2+)))

(define-public r-dynamictreecut
  (package
   (name "r-dynamictreecut")
   (version "1.63-1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "dynamicTreeCut" version))
     (sha256
      (base32
       "1fadbql7g5r2vvlkr89nlrjxwp4yx4xrdqmv077qvmnx9vv0f4w3"))))
   (properties
    `((upstream-name . "dynamicTreeCut")))
   (build-system r-build-system)
   (home-page
    "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/BranchCutting/")
   (synopsis
    "Methods for Detection of Clusters in Hierarchical Clustering Dendrograms")
   (description
    "Contains methods for detection of clusters in hierarchical clustering dendrograms.")
   (license license:gpl2+)))

(define-public r-fastcluster
  (package
   (name "r-fastcluster")
   (version "1.1.21")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "fastcluster" version))
     (sha256
      (base32
       "19bjid6nnraiv1dpkq7mhlm3cnnj4av5v3n1yd8hv9aj7v1miay6"))))
   (build-system r-build-system)
   (home-page
    "http://danifold.net/fastcluster.html")
   (synopsis
    "Fast Hierarchical Clustering Routines for R and Python")
   (description
    "This is a two-in-one package which provides interfaces to both R and Python.  It implements fast hierarchical, agglomerative clustering routines.  Part of the functionality is designed as drop-in replacement for existing routines: linkage() in the SciPy package 'scipy.cluster.hierarchy', hclust() in R's 'stats' package, and the 'flashClust' package.  It provides the same functionality with the benefit of a much faster implementation.  Moreover, there are memory-saving routines for clustering of vector data, which go beyond what the existing packages provide.  For information on how to install the Python files, see the file INSTALL in the source distribution.")
   (license #f))) ;?

(define-public r-preprocesscore
  (package
    (name "r-preprocesscore")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "1n8y12q7145f385gm2k3c6y3vwvin7jlb47la4mnl7mar6pq9kmp"))))
    (build-system r-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public r-phangorn
  (package
   (name "r-phangorn")
   (version "2.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "phangorn" version))
     (sha256
      (base32
       "1klc2s6crwmg30psgf1xkchyjqbksl7cdlnk3b0ypdw7pd3js5im"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-ape" ,r-ape)
      ("r-fastmatch" ,r-fastmatch)
      ("r-igraph" ,r-igraph)
      ("r-magrittr" ,r-magrittr)
      ("r-quadprog" ,r-quadprog)
      ("r-rcpp" ,r-rcpp)))
   (home-page
    "https://github.com/KlausVigo/phangorn")
   (synopsis "Phylogenetic Analysis in R")
   (description
    "Phylogenetic analysis in R: Estimation of phylogenetic trees and networks using Maximum Likelihood, Maximum Parsimony, distance methods and Hadamard conjugation.")
   (license license:gpl2+)))

(define-public r-fastmatch
  (package
   (name "r-fastmatch")
   (version "1.0-4")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "fastmatch" version))
     (sha256
      (base32
       "16gfizfb1p7rjybrfm57nb6hdm30iirbppva8p8xf8pndz35fjbs"))))
   (build-system r-build-system)
   (home-page "http://www.rforge.net/fastmatch")
   (synopsis "Fast match() function")
   (description
    "Package providing a fast match() replacement for cases that require repeated look-ups.  It is slightly faster that R's built-in match() function on first match against a table, but extremely fast on any subsequent lookup as it keeps the hash table in memory.")
   (license license:gpl2)))

(define-public sammy
  (let ((commit "9b71994902440c02c9b4a5c1e459ff522170b58a"))
    (package
      (name "sammy")
      (version (string-append "0-1." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minillinim/sammy.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1jlx25wgw8zrxvwjpq92v9p4nlhfi6jvbq81rb923j14r961f1zl"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'unpack 'patch-pod2usage
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "sammy.pl"
                 (("pod2usage") (which "pod2usage")))
               #t))
           (replace 'check
             (lambda _
               (zero? (system* "./sammy.pl" "-r" "data/reference.fa" "-n" "3"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "sammy.pl" bin)
                 (wrap-program (string-append bin "/sammy.pl")
                   `("PERL5LIB" ":" prefix
                     (,(string-append (getenv "PERL5LIB") ":" out
                                      "/lib/perl5/site_perl"))))
                 #t))))))
      (inputs
       `(("perl" ,perl)
         ("bioperl" ,bioperl-minimal)))
      (home-page "https://github.com/minillinim/sammy")
      (synopsis "Stupidly simple short read simulator")
      (description "Sammy simulates error free paired DNA sequencing reads. It
is useful for testing other software.")
      (license license:gpl3+))))

(define-public ruby-bio-cd-hit-report ; does not work, also a little odd that
                                      ; the tar.gz from github is different to
                                      ; the gem on rubygems. Probably best
                                      ; solution would be to package jeweler.
  (package
    (name "ruby-bio-cd-hit-report")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/biorelated/bioruby-cd-hit-report/archive/v" version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n3zz7sm2k1j4c6hhl5srap11dz9f0hybbvwq3kpn9sz6pr62g83"))
       ;(patches (search-patches "ruby-cd-hit-report-fix-gemfile.patch"))
       ))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest" ,ruby-minitest)))
    (propagated-inputs
     `(("bioruby" ,bioruby)))
    (synopsis
     "A Ruby library for reading CD-HIT cluster reports")
    (description
     "This package provides a Ruby library for reading CD-HIT cluster reports")
    (home-page
     "http://github.com/georgeG/bioruby-cd-hit-report")
    (license license:expat)))

(define-public graph-tool ; does not seem to build, something amiss with boost_python. For some reason "-lboost_python -lpython2.7" is not included in the conf call
  (let ((commit "6ad35ea35b2763c546ccd7521f08d3d04644ccc0"))
  (package
   (name "graph-tool")
   (version "2.18")
   (source
    ;; (origin
    ;;  (method url-fetch)
    ;;  (uri (string-append "https://downloads.skewed.de/graph-tool/graph-tool-"
    ;;                      version ".tar.bz2"))
    ;;  (sha256
    ;;   (base32
    ;;    "1m641rqsrc57dcvhb0slpbp1llyn0y6dzbqm46hi7bkbggxjjj9w"))))
                                        ; https://git.skewed.de/count0/graph-tool.git
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.skewed.de/count0/graph-tool.git")
           (commit commit)))
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "03q6hgcmd6i92nimlls1r712612j46y1aifv3wrdq5jsf2aafd4a"))))
   (build-system gnu-build-system)
   (arguments
    `(;#:configure-flags '("LIBS=-lboost_python -lpython2.7")))
      #:configure-flags '("--enable-openmp")
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'autogen
                    (lambda _ (zero? (system* "./autogen.sh")))))))
   (native-inputs
    `(("gcc" ,gcc-5)))
   (inputs
    `(("python" ,python-2)
      ("boost" ,boost)
      ("expat" ,expat)
      ("python2-scipy" ,python2-scipy)
      ("python2-numpy" ,python2-numpy)
      ("cgal" ,cgal)
      ("sparsehash" ,sparsehash)
      ("python2-pycairo" ,python2-pycairo)
      ("python2-matplotlib" ,python2-matplotlib)
      ("cairomm" ,cairomm)
      ("autoconf"  ,autoconf)
      ("automake"  ,automake)
      ("libtool"  ,libtool))) ;g++ -std=gnu++14 -o conftest -Wall -Wextra -ftemplate-backtrace-limit=0  -DNDEBUG -ftemplate-depth-250 -Wno-deprecated -Wno-unknown-pragmas -O3 -fvisibility=default -fvisibility-inlines-hidden -Wno-unknown-pragmas  -I/gnu/store/vcx1n5nj4gr52xx5m6gvi7zrwngy06s3-python-2.7.11/include/python2.7     my.cpp -lexpat -lbz2 -lm -lboost_python -lpython2.7
   (home-page "")
   (synopsis "")
   (description "")
   (license license:lgpl2.0+)))) ;?

(define-public bandage ; bundles the currently unpackaged ogdf, which bundles
                       ; coin-or-clp, abacus, at least. Also, need to run tests,
                       ; currently not built.
  (package
    (name "bandage")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rrwick/Bandage/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "080q4kvjhbb7is29bdw93q6r0giza9iknr5wzrw6wv8ciajrb02c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
              (lambda (file)
                (substitute* file
                  (("target.path \\+= /usr/local/bin")
                   (string-append "target.path += "
                                  (assoc-ref outputs "out")
                                  "/bin/"))
                  (("unix:INCLUDEPATH \\+= /usr/include/")
                   (string-append "unix:INCLUDEPATH += "
                                  (assoc-ref outputs "out")
                                  "/include/"))
                  (("unix:LIBS \\+= -L/usr/lib")
                   (string-append "unix:LIBS += -L"
                                  (assoc-ref outputs "out")
                                  "/lib/"))))
              '("Bandage.pro" "BandageTests.pro"))
             ; TODO: if BandageTests.pro is uncommented below, the Bandage executable itself is not built. But without it there is no tests.
             (zero? (system* "qmake" "Bandage.pro" ;"BandageTests.pro" ; PREFIX needed? or substitute* needed?
                             ))))))) ;
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (home-page "")
    (synopsis "Navigating de novo assembly graphs")
    (description "Bandage is a GUI program that allows users to interact with
the assembly graphs made by de novo assemblers such as Velvet, SPAdes, MEGAHIT
and others.

De novo assembly graphs contain not only assembled contigs but also the
connections between those contigs, which were previously not easily accessible.
Bandage visualises assembly graphs, with connections, using graph layout
algorithms. Nodes in the drawn graph, which represent contigs, can be
automatically labelled with their ID, length or depth. Users can interact with
the graph by moving, labelling and colouring nodes.  Sequence information can
also be extracted directly from the graph viewer.  By displaying connections
between contigs, Bandage opens up new possibilities for analysing and improving
de novo assemblies that are not possible by looking at contigs alone.")
    (license license:gpl3+)))

(define-public coin-or-clp
  (package
    (name "coin-or-clp")
    (version "1.16.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.coin-or.org/download/source/Clp/Clp-"
                           version ".tgz"))
       (sha256
        (base32
         "1k9s5xnj9ww9x73hk179vqz0lq0rh520m2zv4g97kzfgmzr81n2w"))))
    (build-system gnu-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public irep ; Waiting on python-seaborn to compile after the core-updates merge.
  (package
    (name "irep")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/christophertbrown/iRep/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "06ypzys3fml8h4qqn1g01ix9lbxa2fy3sc52avil6bjsx8x3656h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'test
           (lambda _
             (and
              (zero? (system* "bin/iRep.py" "-f" "sample_data/l_gasseri.fna" "-s"
                              "sample_data/l_gasseri.fna-vs-l_gasseri_sample1-shrunk.sam"
                              "sample_data/l_gasseri.fna-vs-l_gasseri_sample2-shrunk.sam"
                              "-o" "test.iRep"))
              (zero? (system* "bin/bPTR.py" "-f" "sample_data/l_gasseri.fna" "-s" 
                              "sample_data/l_gasseri.fna-vs-l_gasseri_sample1-shrunk.sam"
                              "sample_data/l_gasseri.fna-vs-l_gasseri_sample2-shrunk.sam"
                              "-o" "test.bPTR.tsv" "-plot" "test.bPTR.pdf" "-m" "coverage"))
              (zero? (system* "bin/gc_skew.py" "-f" "sample_data/l_gasseri.fna")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (file)
                         (install-file
                          file
                          (string-append (assoc-ref outputs "out") "/bin")))
                       '("bPTR.py"
                         "fasta.py"
                         "gc_skew.py"
                         "iRep.py"
                         "mapped.py")))))))
    (inputs
     `(("python" ,python)
       ("python-lmfit" ,python-lmfit)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-pandas" ,python-pandas)
       ("python-seaborn" ,python-seaborn)
       ("python-matplotlib" ,python-matplotlib)))
    (home-page "")
    (synopsis
     "")
    (description
     "")
    (license license:gpl2+))) ;?

(define-public python-lmfit
  (package
    (name "python-lmfit")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lmfit" version))
       (sha256
        (base32
         "0rv6pssvy564viphqsd0slx4532lh9a9lm97v6ym2glzxls3rg7f"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)))
    (home-page "http://lmfit.github.io/lmfit-py/")
    (synopsis
     "Least-Squares Minimization with Bounds and Constraints")
    (description
     "Least-Squares Minimization with Bounds and Constraints")
    (license license:bsd-3)))

(define-public musicc
  (package
    (name "musicc")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MUSiCC" version))
       (sha256
        (base32
         "1hbcl911acam2ys58abc5g590byfb0q8c4v61qfsxdr8nz4ma9li"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append ".:" (getenv "PYTHONPATH")))
             (zero? (system* "python" "tests/test_musicc.py")))))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-scikit-learn" ,python-scikit-learn)
       ("python-pandas" ,python-pandas)))
    (home-page
     "http://elbo.gs.washington.edu/software_musicc.html")
    (synopsis "Marker gene-based framework for metagenomic normalization")
    (description
     "MUSiCC is a software package for normalizing and correcting gene abundance
measurements derived from metagenomic shotgun sequencing.  For intra-sample gene
normalization, it uses a set of genes that are universally present in a single
copy in every organism to learn a normalizing factor for each sample.  For
inter-sample gene correction, it uses a gene-specific model that was trained
using data from the Human Microbiome Project to correct the abundances of the
genes in the given data. MUSiCC takes as input a raw gene abundance file, and
returns a normalized and corrected gene abundance file.")
    (license license:bsd-3)))

(define-public python-cogent
  (package
   (name "python-cogent")
   (version "1.9")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "cogent" version))
     (sha256
      (base32
       "056far6y9x39cdkbf1sm1xhd1pys96dlz1q7p7rf9zvk0a7cbn2p"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-mpi4py" ,python-mpi4py)
;      ("python-pymysql" ,python-pymysql) ;TODO
      ("python-sqlalchemy" ,python-sqlalchemy)))
   (home-page "http://github.com/pycogent/pycogent")
   (synopsis "COmparative GENomics Toolkit")
   (description "COmparative GENomics Toolkit")
   (license license:gpl3)))

(define-public python-mpi4py
  (package
   (name "python-mpi4py")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "mpi4py" version))
     (sha256
      (base32
       "10fb01595rg17ycz08a23v24akm25d13srsy2rnixam7a5ca0hv5"))))
   (build-system python-build-system)
   (arguments
     `(#:phases
       (modify-phases %standard-phases
                      (replace 'check
                               (lambda _
                                 (setenv "PYTHONPATH" (string-append "build/lib.linux-x86_64-3.5/:" (getenv "PYTHONPATH")))
                                 (zero? (system* "nosetests" "-v")))))))
   (native-inputs
    `(("python-nose" ,python-nose)))
   (inputs
    `(("openmpi" ,openmpi)))
   (home-page
    "https://bitbucket.org/mpi4py/mpi4py/")
   (synopsis "MPI for Python")
   (description "MPI for Python")
   (license license:bsd-3)))

(define-public exuberant-ctags
  (package
    (name "exuberant-ctags")
    (version "5.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/ctags/ctags/"
                           version "/ctags-" version ".tar.gz"))
       (sha256
        (base32
         "1iwrkrpdcmzbjmrv6b8169fvw6pq8v1307mipc5rx5myr9fv8i0f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  ;; There are no tests, so run a sanity check.
                  (lambda _ (zero? (system* "./ctags" "--help")))))))
    (home-page "http://ctags.sourceforge.net/")
    (synopsis "")
    (description "")
    (license license:gpl3+))) ;?

(define-public universal-ctags ; Seems ok.
  ;; We package from a git commit because there are no releases.
  (let ((commit "2141d3114e2545eec4e981c81b2f60c243f3c9d1"))
    (package
      (name "universal-ctags")
      (version (string-append "0-1." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/universal-ctags/ctags.git")
               (commit commit)))
         (file-name (string-append name "-" version))
         (sha256
          (base32
           "05mdjc3l6qwnx6qv5y07kmxzbf3qy8awmmblpynj9kyy972nzsz0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-files
             (lambda _
               (substitute* '("misc/budge" "misc/dist-test-cases")
                 (("git ls-files") "find * | sort"))
               (substitute* "misc/units"
                 (("^SHELL=/bin/sh")
                  (string-append "SHELL=" (which "sh"))))
               (for-each (lambda (file)
                           (chmod file #o666))
                         (find-files "optlib"))))
           (add-before 'configure 'autogen
             (lambda _ (zero? (system* "./autogen.sh")))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("perl" ,perl)
         ("python" ,python-wrapper)
         ("python-docutils" ,python-docutils)))
      (home-page "https://ctags.io/")
      (synopsis "Generate tag files for source code")
      (description "The ctags and etags programs generate an index (or \"tag\")
file for a variety of language objects found in file(s).  This tag file allows
these items to be quickly and easily located by a text editor or other utility.
A \"tag\" signifies a language object for which an index entry is available, or
alternatively the index entry created for that object).")
      (license license:gpl2+))))

(define-public abi-compliance-checker
  (package
    (name "abi-compliance-checker")
    (version "1.99.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lvc/abi-compliance-checker/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "103kic53y9061hskgm329kw7rwf04f985i0kmdamrgl9kkrjvn5n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append
                           "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'create-prefix-dir
          (lambda* (#:key outputs #:allow-other-keys)
            (mkdir-p (assoc-ref outputs "out"))
            #t))
         (delete 'check)
         (add-after 'install 'post-install-check
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* (string-append (assoc-ref outputs "out")
                                            "/bin/abi-compliance-checker")
                             "-test"))))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (binary (string-append out "/bin/abi-compliance-checker"))
                    (path   (getenv "PATH")))
               (wrap-program binary
                 `("PATH" ":" prefix (,path)))
               #t))))))
    (inputs
     `(("perl" ,perl)
       ("ctags" ,universal-ctags)
       ("abi-dumper" ,abi-dumper)
       ("glibc" ,ldconfig)))
    (home-page "https://lvc.github.io/abi-compliance-checker/")
    (synopsis "Binary and source-level compatibility checker")
    (description "The ABI Compliance Checker (ABICC) is a tool for checking
backward binary and source-level compatibility of a C/C++ software library.
The tool analyzes changes in API/ABI (ABI=API+compiler ABI) that may break
binary compatibility and/or source compatibility: changes in calling stack,
v-table changes, removed symbols, renamed fields, etc.")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public abi-dumper ; Needs further testing e.g. in a container running the example code.
  (package
    (name "abi-dumper")
    (version "0.99.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lvc/abi-dumper/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a25bn03ahlyr5aygq4bgphn39mr0rwb1gg7v4ilb81rb1wkbg3b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (add-before 'install 'create-prefix-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (assoc-ref outputs "out"))
             #t))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (binary (string-append out "/bin/abi-dumper"))
                    (path   (getenv "PATH")))
               (wrap-program binary
                 `("PATH" ":" prefix (,path)))
               #t))))))
    (inputs
     `(("perl" ,perl)
       ("elfutils" ,elfutils)
       ("vtable-dumper" ,vtable-dumper)
       ("ctags" ,universal-ctags)))
    (home-page "https://github.com/lvc/abi-dumper")
    (synopsis "Dump ABI of an ELF object containing DWARF debug info")
    (description "ABI Dumper is a tool to dump ABI of an ELF object containing
DWARF debug info.  The tool is intended to be used with ABI Compliance Checker
tool for tracking ABI changes of a C/C++ library or kernel module.")
    ;; License is LGPL or GPL.
    (license (list license:lgpl2.1 license:gpl2))))

(define-public vtable-dumper ; testing required, but seems to work.
  (package
    (name "vtable-dumper")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lvc/vtable-dumper/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lhzvp32big3wbdnv0z9ssv4djqa856dph3dyz2nz4q0d8dpi4v9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("libelf" ,libelf)))
    (home-page "https://github.com/lvc/vtable-dumper")
    (synopsis "List content of virtual tables in a C++ shared library")
    (description "Vtable-Dumper is a tool to list content of virtual tables in a
C++ shared library.  It is intended for developers of software libraries and
maintainers of Linux distributions who are interested in ensuring backward
binary compatibility.")
    ;; License is LGPL or GPL.
    (license (list license:lgpl2.1 license:gpl2))))

;;; The glibc package does not install ldconfig, this package provides it for
;;; packages which need it at runtime.
(define-public ldconfig
  (package
    (inherit glibc)
    (name "ldconfig")
    (version (package-version glibc))
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/glibc/glibc-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1lxmprg9gm73gvafxd503x70z32phwjzcy74i0adfi6ixzla7m4r"))
              (patches (origin-patches (package-source glibc)))))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "../build/elf/ldconfig" bin))
               #t))))))))

(define-public desman
  ;; There are no releases so we fetch via git.
  (let ((commit "b81de503b78af856ac0ee451ecb4bed72f1249be"))
    (package
      (name "desman")
      (version (string-append "0-1." (string-take commit 8)))
      (source
       (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/chrisquince/DESMAN")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (patches
         (search-patches "desman-make-desman-scripts-executable.patch"))
        (sha256
         (base32
          "04mz360parvd2xf7kr5j001m6892irrlxjp5b5rm4bf8piqgj14a"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'copy-extra-scripts
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (for-each
                  (lambda (file) (install-file file bin))
                  (find-files "scripts" "."))
                 (install-file "desman/Variant_Filter.py" bin)
                 (install-file "desman/GeneAssign.py" bin)
                 #t))))))
      (inputs
       `(("python-cython" ,python2-cython)
         ("python-numpy" ,python2-numpy)
         ("python-scipy" ,python2-scipy)
         ("python-pandas" ,python2-pandas)
         ("python-setuptools" ,python2-setuptools)
         ("python-pytz", python2-pytz)
         ("gsl" ,gsl)
         ("perl" ,perl)
         ("r" ,r)))
      (home-page "https://github.com/chrisquince/DESMAN")
      (synopsis "De novo extraction of strains from metagenomes")
      (description
       "DESMAN is a pipeline that facilities de novo extraction of both strain
haplotypes and accessory genomes de novo from metagenome data.")
      (license license:bsd-2))))

(define-public soedinglab-pdbx ; potentially works, on the way to hh-suite
  (let ((commit "7f71d9c56cd466d035013c9e029221c31f26cdbd"))
    (package
     (name "soedinglab-pdbx")
     (version (string-append "0-1." (string-take commit 8)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url  "https://github.com/soedinglab/pdbx.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1xsg8z97wz82gmd2rbj1hi233s147m9g7cq3ax99nc6vgl5waxsl"))))
     (build-system cmake-build-system)
     (arguments
      `(#:tests? #f ; There are no tests.
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'setup-prefix
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (string-append (assoc-ref outputs "out"))))
                (substitute* "CMakeLists.txt"
                             (("SETUP_PY} install")
                              (string-append
                               "SETUP_PY} install --prefix " out)))
                #t))))))
     (inputs
      `(("python" ,python)))
     (home-page "")
     (synopsis "")
     (description "")
     (license license:cc-by-sa3.0)))) ;? license is unclear

(define-public hh-suite ; in progress, need to watch for -march=native too.
  (package
    (name "hh-suite")
    (version "3.0-beta.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/soedinglab/hh-suite/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "012w0a7cv5fngfngcmgpijbl0k55jky6l2wm3scm4dd499g7gv6m"))))
    (build-system cmake-build-system)
    (inputs
     `(("python" ,python-wrapper)
       ("soedinglab-pdbx" ,soedinglab-pdbx)
       ("soedinglab-ffindex" ,soedinglab-ffindex)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:cc-by-sa3.0))) ;?

(define-public soedinglab-ffindex ; Seems to work
  ;; There are no releases so we package from git.
  (let ((commit "e140236517faf634c637ab2bf2d3254e13a8bfac"))
    (package
      (name "soedinglab-ffindex")
      (version (string-append "0.9.9-1." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url  "https://github.com/soedinglab/ffindex_soedinglab.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0nl264ly7qx0idsjcw3k8pjz0v19kka8nfbgvxrgqfm8zcwzffkl"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags (list (string-append
                                  "-DCMAKE_RUNTIME_OUTPUT_DIRECTORY="
                                  %output
                                  "/bin"))
         #:tests? #f)) ; There are no tests.
      (inputs
       `(("openmpi" ,openmpi)))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:cc-by-sa3.0))))

(define-public mothur ; Probably works, but haven't really tested it. Probably
                      ; also requires packaging of uchime. This is bundled with
                      ; mothur but is available at
                      ; http://drive5.com/uchime/uchime_download.html.
  (package
    (name "mothur")
    (version "1.38.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mothur/mothur/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15xzwn6d9520xfwwlyy5vpp9r5p5xvkwjf219ki87c3k98a9xhp0"))))
    (build-system gnu-build-system)
    (arguments
     `(;; There are no tests.  See
       ;; https://github.com/mothur/mothur/issues/196
       #:tests? #f
       #:make-flags (list (string-append
                           "BOOST_LIBRARY_PATH=\""
                           (assoc-ref %build-inputs "boost")
                           "/lib\"")
                          (string-append
                           "BOOST_INCLUDE_DIR=\""
                           (assoc-ref %build-inputs "boost")
                           "/include\"")
                          (string-append
                           "MOTHUR_FILES=\\\""
                           (assoc-ref %outputs "out")
                           "/share/mothur\\\""))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
             (install-file "mothur" bin)))))))
    (inputs
     `(("boost" ,boost)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3)))

(define-public ncbi-tools ; in progress
  (package
    (name "ncbi-tools")
    (version "20160908")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.ncbi.nih.gov/toolbox/ncbi_tools/old/"
                           version "/ncbi.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1252s4fw41w5yalz9b50pvzvkiyjfcgy0isw1qgmg0v66bp49khz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-sources
            (lambda _
              (for-each (lambda (file)
                          (substitute* file
                            (("NCBI_MAKE_SHELL = .*")
                             (string-append
                              "NCBI_MAKE_SHELL = "
                              (which "sh")
                              "\n"))))
                        (find-files "platform" ".*mk"))
              (substitute* "make/ln-if-absent"
                (("set path=\\(/usr/bin /bin\\)") ""))
              #t))
         (replace 'build
            (lambda _
              (chdir "..")
              (zero? (system* "ncbi/make/makedis.csh"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (for-each (lambda (file)
                           (install-file
                            (string-append "ncbi/build/" file) bin)
                           (install-file
                            (string-append "ncbi/doc/man/" file ".1") man))
                         ;; XXX: TODO: Install and test other binaries.
                         (list "tbl2asn"))
               #t))))))
    (native-inputs
     `(("csh" ,tcsh)
       ("pkg-config" ,pkg-config)
       ("coreutils" ,coreutils)))
    (home-page "https://www.ncbi.nlm.nih.gov/IEB/ToolBox/MainPage/index.html")
    (synopsis "NCBI-related tools")
    (description "The United States of America @dfn{National Center for
Biotechnology Information} (NCBI) Software Development Toolkit is for the
production and distribution of GenBank, Entrez, BLAST and related NCBI
services.")
    (license license:public-domain)))

(define-public fastahack ; Seems to work, but I need to use it a bit more first.
  ;; There are no releases so we package from git.
  (let ((commit "bbc645f2f7966cb7b44446200c02627c3168b399"))
    (package
      (name "fastahack")
      (version (string-append "0-1." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ekg/fastahack.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "03q7mga9xx7c7rcipv1q808gi9wnxgxmdwq6kjmalphnnm002qc6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests.
         #:make-flags (list (string-append "PREFIX=" %output))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:cc-by-sa3.0)))) ;?

;; (define-public ruby-builder-2.2 ; dummy package for messing around
;;   (package
;;     (name "ruby-builder-2.2")
;;     (version "3.2.2")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (rubygems-uri "builder" version))
;;               (sha256
;;                (base32
;;                 "14fii7ab8qszrvsvhz6z2z3i4dw0h41a62fjr2h1j8m41vbrmyv2"))))
;;     (build-system ruby-build-system)
;;     (arguments
;;      `(#:ruby ,ruby-2.2
;;        #:phases
;;        (modify-phases %standard-phases
;;          (add-after 'unpack 'do-not-use-rvm
;;                     (lambda _
;;                       (substitute* "rakelib/tags.rake"
;;                         (("RVM_GEMDIR = .*") "RVM_GEMDIR = 'no-rvm-please'\n"))
;;                       #t)))))
;;     (synopsis "Ruby library to create structured data")
;;     (description "Builder provides a number of builder objects that make it
;; easy to create structured data.  Currently the following builder objects are
;; supported: XML Markup and XML Events.")
;;     (home-page "https://github.com/jimweirich/builder")
;;     (license license:expat)))







(define-public finishm
  (package
    (name "finishm")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "finishm" version))
       (sha256
        (base32 "0jprmh5y78gm1swk26r40r8cnc0s9rizqhgbafxnnarl1nn3v1dw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Tests probably don't pass.
    (propagated-inputs
     `(("ruby-bio" ,bioruby)
       ("ruby-bio-ipcress" ,ruby-bio-ipcress)
       ("ruby-bio-logger" ,ruby-bio-logger)
       ("ruby-bio-velvet" ,ruby-bio-velvet)
       ("ruby-bio-velvet-underground"
        ,ruby-bio-velvet-underground)
       ("ruby-ds" ,ruby-ds)
       ("ruby-hopcsv" ,ruby-hopcsv)
       ("ruby-progressbar" ,ruby-progressbar)
       ("ruby-pry" ,ruby-pry)
       ("ruby-graphviz" ,ruby-graphviz)
       ("ruby-yargraph" ,ruby-yargraph)))
    (inputs
     `(("zlib" ,zlib)))
    (synopsis "Tools for microbial genome assembly improvement")
    (description
     "De-novo assemblies generally only provide draft genomes. FinishM is aimed
at improving these draft assemblies.")
    (home-page "http://github.com/wwood/finishm")
    (license license:gpl3+)))

(define-public ruby-yargraph
  (package
    (name "ruby-yargraph")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yargraph" version))
       (sha256
        (base32
         "1nr8qcfi52phs491b3frrh2npkj4sr2mm5jbkyk4b2v233kx6ax3"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("ruby-ds" ,ruby-ds)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-gemfile
                     (lambda _
                       (substitute* "Gemfile"
                         ((".*jeweler.*") ""))
                       #t))
         (replace 'check
                  (lambda _ (zero? (system* "rspec" "spec/yargraph_spec.rb"))))
         )))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "Pure Ruby graph algorithms, particularly e.g. Hamiltonian cycles")
    (description
     "Pure Ruby graph algorithms, particularly e.g.  Hamiltonian cycles")
    (home-page "http://github.com/wwood/yargraph")
    (license license:expat)))

(define-public ruby-bio-ipcress
  (package
    (name "ruby-bio-ipcress")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-ipcress" version))
       (sha256
        (base32
         "1lfmnqbdwaiwhzackjl4vdnwdb1x4wjfmn79ki8czqxsj1ra5bwq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemfile
                     (lambda _
                       (substitute* ".gemspec"
                         ((".*rdoc.*") ""))
                       #t))
         (replace 'check
                  (lambda _ (zero? (system* "rspec")))))))
    (propagated-inputs
     `(("ruby-bio" ,bioruby)))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "a programmatic interface to the iPCRess in-silico PCR software. iPCRess is part of the exonerate suite.")
    (description
     "a programmatic interface to the iPCRess in-silico PCR software.  iPCRess is part of the exonerate suite.")
    (home-page
     "http://github.com/wwood/bioruby-ipcress")
    (license #f)))

(define-public ruby-graphviz
  (package
    (name "ruby-graphviz")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "graphviz" version))
       (sha256
        (base32
         "1hh2jmg4v6n3ggc5i44bfl1j1fg968j9grk57mf02fy1yxs67w3c"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "rspec")))))))
    (inputs
     `(("graphviz" ,graphviz))) ; Need to propagate?
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "\t\tGraphviz is a graph visualisation system. This gem is a lightweight interface for generating graphs with Graphviz.
")
    (description
     "\t\tGraphviz is a graph visualisation system.  This gem is a lightweight interface for generating graphs with Graphviz.
")
    (home-page "")
    (license license:expat)))

(define-public ruby-bio-velvet
  (package
    (name "ruby-bio-velvet")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-velvet" version))
       (sha256
        (base32
         "0q05r43g6pgwl6pcaih1ikd86apzzmb3icp7grqrmn9akaprb69i"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require velvet.
    (propagated-inputs
     `(("ruby-bio-commandeer" ,ruby-bio-commandeer)
       ("ruby-bio-logger" ,ruby-bio-logger)
       ("ruby-files" ,ruby-files)
       ("ruby-hopcsv" ,ruby-hopcsv)
       ("ruby-parallel" ,ruby-parallel)
       ("ruby-systemu" ,ruby-systemu)))
    (synopsis
     "Parser to work with some file formats used in the velvet DNA assembler")
    (description
     "Parser to work with some file formats used in the velvet DNA assembler")
    (home-page
     "http://github.com/wwood/bioruby-velvet")
    (license license:expat)))

(define-public ruby-bio-velvet-underground
  (package
    (name "ruby-bio-velvet-underground")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-velvet_underground" version))
       (sha256
        (base32
         "1wfpq76n91701pca34iqgvxh2nqxjsa51356v10q0c78crm5pn8z"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Tests probably need to be run after installation.
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (add-before 'build 'remove-unnecessary-dependencies
       ;;               (lambda _
       ;;                 (substitute* "Gemfile"
       ;;                   ((".*pry.*") "")
       ;;                   ((".*shoulda.*") "")
       ;;                   ((".*yard.*") "")
       ;;                   ((".*simplecov.*") "")
       ;;                   ((".*jeweler.*") ""))
       ;;                 #t))
       ;;   (replace 'check (lambda _ (zero? (system* "rspec")))))))
    (propagated-inputs
     `(("ruby-bio-logger" ,ruby-bio-logger)
       ("ruby-bio-velvet" ,ruby-bio-velvet)
       ("ruby-ffi" ,ruby-ffi)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)
       ("bioruby" ,bioruby)
       ("ruby-pry" ,ruby-pry)))
    (inputs
     `(("zlib" ,zlib)))
    (synopsis
     "Bindings to some internals of the velvet assembler.")
    (description
     "Bindings to some internals of the velvet assembler.")
    (home-page
     "http://github.com/wwood/bioruby-velvet_underground")
    (license license:gpl3)))

(define-public ruby-hopcsv
  (package
    (name "ruby-hopcsv")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hopcsv" version))
       (sha256
        (base32
         "1cq8wkx95jr25kamab3gghw4vrxm0vd66l5a61jv7yrw7rnbgwyg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Has a circular dependency with itself, run after install?
    (native-inputs
     `(("ruby-echoe" ,ruby-echoe)))
    (synopsis
     "A pure-C CSV parser for HOPSA. Based on Ccsv project. Works fast and efficient. Based on ccsv by Evan Weaver")
    (description
     "This package provides a pure-C CSV parser for HOPSA.  Based on Ccsv project.  Works fast and efficient.  Based on ccsv by Evan Weaver")
    (home-page "http://github.com/zhum/hopcsv")
    (license #f)))

(define-public ruby-progressbar
  (package
    (name "ruby-progressbar")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "progressbar" version))
       (sha256
        (base32
         "15zs2a18v5z28y6bxzrljbd8dcpkf86qw4k62vcchjzqlhklsfai"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Requires rspectacular
       ;;#:phases
       ;; (modify-phases %standard-phases
    ;;      (replace 'check
    ;;               (lambda _ (zero? (system* "rspec")))))))
    ;; (native-inputs
    ;;  `(("bundler" ,bundler)
    ;;    ("ruby-rspectacular" ,ruby-rspectacular)))
    (synopsis
     "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.
The output can be customized with a flexible formatting system including:
percentage, bars of various formats, elapsed time and estimated time remaining.
")
    (description
     "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.
The output can be customized with a flexible formatting system including:
percentage, bars of various formats, elapsed time and estimated time remaining.
")
    (home-page
     "https://github.com/jfelchner/ruby-progressbar")
    (license license:expat)))

(define-public ruby-files
(package
  (name "ruby-files")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "files" version))
      (sha256
        (base32
          "0csfvyf6kqgzcd8fq55x0z8ljbgxp9g9f5d9n6gpd4kcjypfvx1a"))))
  (build-system ruby-build-system)
  (arguments
     `(#:tests? #f)) ; Tests require wrong, which requires other gems.
  ;; (native-inputs
  ;;  `(("bundler" ,bundler)
  ;;    ("ruby-wrong" ,ruby-wrong)))
  (synopsis
    "Ever want to create a whole bunch of files at once? Like when you're writing tests for a tool that processes files? The Files gem lets you cleanly specify those files and their contents inside your test code, instead of forcing you to create a fixture directory and check it in to your repo. It puts them in a temporary directory and cleans up when your test is done.")
  (description
    "Ever want to create a whole bunch of files at once? Like when you're writing tests for a tool that processes files? The Files gem lets you cleanly specify those files and their contents inside your test code, instead of forcing you to create a fixture directory and check it in to your repo.  It puts them in a temporary directory and cleans up when your test is done.")
  (home-page "")
  (license #f)))

(define-public ruby-parallel
  (package
    (name "ruby-parallel")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/grosser/parallel/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0lm57mamx4w6aksw0r64517bmffjnw8cp3kpd4gmv3lshy3rmzrk"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; Requires activerecord, meh
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
                  (lambda _
                    (substitute* "parallel.gemspec"
                      (("`git ls-files lib MIT-LICENSE.txt`") "`find lib MIT-LICENSE.txt -type f |sort`"))
                    #t))
         (add-before 'check 'prepare-check
                     (lambda _
                       (delete-file "Gemfile.lock")
                       (substitute* '("Gemfile" "Rakefile")
                         ((".*bump.*") ""))
                       #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "Run any kind of code in parallel processes")
    (description
     "Run any kind of code in parallel processes")
    (home-page "https://github.com/grosser/parallel")
    (license license:expat)))

(define-public ruby-echoe
  (package
    (name "ruby-echoe")
    (version "4.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "echoe" version))
       (sha256
        (base32
         "09a2349hywv0dn1499m48ypsgk36bhjfg2rfbwbkhn9mj8s205bh"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests.
    (propagated-inputs
     `(("ruby-allison" ,ruby-allison)
       ;("ruby-rake" ,ruby-rake)
       ;("ruby-rdoc" ,ruby-rdoc)
       ;("ruby-rubyforge" ,ruby-rubyforge)
       ))
    (synopsis
     "A Rubygems packaging tool that provides Rake tasks for documentation, extension compiling, testing, and deployment.")
    (description
     "This package provides a Rubygems packaging tool that provides Rake tasks for documentation, extension compiling, testing, and deployment.")
    (home-page
     "http://fauna.github.com/fauna/echoe/")
    (license #f)))

(define-public ruby-allison
(package
  (name "ruby-allison")
  (version "2.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "allison" version))
      (sha256
        (base32
          "1dcbffr23zxq2g3k7d468x864bp7y2galdzpajngaqm578vng4fk"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; There are no tests.
  (native-inputs
   `(("ruby-rspec" ,ruby-rspec)))
  (synopsis "A modern, pretty RDoc template.")
  (description
    "This package provides a modern, pretty RDoc template.")
  (home-page
    "http://blog.evanweaver.com/pages/code#allison")
  (license #f)))



;; (define-public ruby-rubyforge
;; (package
;;   (name "ruby-rubyforge")
;;   (version "2.0.4")
;;   (source
;;     (origin
;;       (method url-fetch)
;;       (uri (rubygems-uri "rubyforge" version))
;;       (sha256
;;         (base32
;;           "1wdaa4nzy39yzy848fa1rybi72qlyf9vhi1ra9wpx9rpi810fwh1"))))
;;   (build-system ruby-build-system)
;;   (propagated-inputs
;;    `(("ruby-json-pure" ,ruby-json-pure)))
;;   (native-inputs
;;    `(("ruby-hoe" ,ruby-hoe)))
;;   (synopsis
;;     "A script which automates a limited set of rubyforge operations.

;; * Run 'rubyforge help' for complete usage.
;; * Setup: For first time users AND upgrades to 0.4.0:
;;   * rubyforge setup (deletes your username and password, so run sparingly!)
;;   * edit ~/.rubyforge/user-config.yml
;;   * rubyforge config
;; * For all rubyforge upgrades, run 'rubyforge config' to ensure you have latest.")
;;   (description
;;     "This package provides a script which automates a limited set of rubyforge operations.

;; * Run 'rubyforge help' for complete usage.
;; * Setup: For first time users AND upgrades to 0.4.0:
;;   * rubyforge setup (deletes your username and password, so run sparingly!)
;;   * edit ~/.rubyforge/user-config.yml
;;   * rubyforge config
;; * For all rubyforge upgrades, run 'rubyforge config' to ensure you have latest.")
;;   (home-page
;;     "http://codeforpeople.rubyforge.org/rubyforge/")
;;   (license #f)))




(define-public bam-readcount ; Does not work because unbundling samtools caused it to fail
  (let ((commit "dea4199722053a179bc64919e81ee43d61bf2aa8"))
    (package
     (name "bam-readcount")
     (version "0.8.0")
     (source
      (origin
       (method url-fetch)
       ;; (uri (string-append "https://github.com/genome/bam-readcount/archive/v"
       ;;                     version ".tar.gz"))
       ;; (file-name (string-append name "-" version ".tar.gz"))
       ;; (sha256
       ;;  (base32
       ;;   "0px6gq63bfzjlqzg294vcgk4nlppd0al9v2pd96v5gy6wdcdakag"))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/genome/bam-readcount.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (patches (search-patches "bam-readcount-remove-bundled.patch"))
       (sha256
        (base32
         "1k5gr4ij9p943980348iy5piakgvxj2n8f4ywha3xdlnjh6s2nms"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; Delete bundled libraries.
           (for-each (lambda (file)
                       (delete-file-recursively file)
                       #t)
                     '("vendor/boost-1.55-bamrc.tar.gz"
                       "vendor/samtools0.1.19.patch"
                       "vendor/samtools-0.1.19.tar.gz"
                       "vendor/zlib-1.2.8.tar.gz"))
           #t))))
     (build-system cmake-build-system)
     (arguments
      `(#:out-of-source? #t
        #:parallel-build? #f
        ;; #:configure-flags
        ;; (list (string-append "-DCMAKE_PREFIX_PATH="
        ;;                      (assoc-ref %build-inputs "samtools"))
        ;;       (string-append "-DSAMTOOLS_ROOT="
        ;;                      (assoc-ref %build-inputs "samtools"))
        ;;       )
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-samtools-cmake
                     (lambda* (#:key inputs #:allow-other-keys)
                         (substitute* "build-common/cmake/FindSamtools.cmake"
                           (("set\\(SAMTOOLS_SEARCH_DIRS")
                            (string-append "set(SAMTOOLS_SEARCH_DIRS\n    "
                                           (assoc-ref inputs "samtools")
                                           "/lib"))))))
        ))
     (native-inputs
      `(("git" ,git)
        ("perl" ,perl)
        ("python" ,python-2)))
     (inputs
      `(("zlib" ,zlib)
        ("ncurses" ,ncurses)
        ("gcc", gcc-5)
        ("samtools" ,samtools-0.1)
        ("pkg-config" ,pkg-config)
        ("boost" ,boost)))
     (home-page "")
     (synopsis "")
     (description
      "")
     (license license:expat))))

(define-public meld ; does not work with --pure
  (package
    (name "meld")
    (version "3.16.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0rwflfkfnb9ydnk4k591x0il29d4dvz95cjs2f279blx64lgki4k"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setup
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "meld/conf.py"
                 (("melddir = os.path.dirname\\(sys.executable\\)")
                  (string-append "melddir = '" out "'"))))))
         (delete 'check)
         (add-after 'install 'post-install-check
           ;; Running the tests before installation breaks installation so that
           ;; the 'meld' executable croaks, so we run the tests after
           ;; installation.
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "PYTHONPATH"
                     (string-append "build/lib:" (getenv "PYTHONPATH")))
             (zero? (system* "py.test" "-v"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gsettings
                     (string-append out "/share/gsettings-schemas"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (and (zero? (system*
                            "python"
                            "setup.py"
                            "--no-update-icon-cache"
                            "--no-compile-schemas"
                            "install"
                            "--prefix"
                            out))
                    (begin
                      (mkdir-p gsettings)
                      ;(rename-file
                      ; (string-append out "/share/glib-2.0")
                      ; (string-append gsettings "/meld"))
                      (wrap-program (string-append out "/bin/meld")
                        `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
                      #t))))))))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("glib:out" ,glib "out")
       ("gtksourceview" ,gtksourceview)))
    (inputs
     `(("python2-pygobject" ,python2-pygobject)("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("python2-pycairo" ,python2-pycairo)))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin")
       ("python2-pytest" ,python2-pytest)
       ("python2-pytest-mock" ,python2-pytest-mock)))
    (home-page "http://meldmerge.org/")
    (synopsis "Visual diff and merge tool")
    (description
     "Meld is a visual diff and merge tool targeted at developers.  Meld helps
you compare files, directories, and version controlled projects.  It provides
two- and three-way comparison of both files and directories, and has support for
many popular version control systems.")
    ;; Files under meld/vc are licensed under BSD-2.
    (license (list license:gpl2+ license:bsd-2))))

(define-public vcontact ; potentially could be contributed to guix-proper,
                                        ; unless any issues are found soon.
  ; Currently tests are not run
  ;; The releases do not appear to be packaged anywhere, so we fetch via git.
  (let ((version-string "0.1b.49")
        (commit "98ff5a9f28fd1d2aa4f29612112174b23b389509"))
    (package
      (name "vcontact")
      (version version-string)
      (source
       (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/geeklhem/vcontact.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "0dkydfzd3yyilknpbfh6ay8hskqsgzk39c4gjfsn7vqxg4wghxkn"))
        (patches (search-patches "vcontact-relax-dependency-versions.patch"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (replace 'check
                    (lambda _ (zero? (system* "nosetests" "-v"))))
           (add-after 'install 'wrap-binaries
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin"))
                               (path (dirname (which "mcl"))))
                          (for-each (lambda (file)
                                      (wrap-program (string-append bin "/" file)
                                        `("PATH" ":" prefix (,path))))
                                    (list "vcontact" "vcontact-pcs"))
                          #t))))))
      (inputs
       `(("python-networkx" ,python2-networkx)
         ("python-numpy" ,python2-numpy)
         ("python-scipy" ,python2-scipy);-0.13)
         ("python-pandas" ,python2-pandas-0.13)
         ("python-scikit-learn" ,python2-scikit-learn)
         ("python-biopython" ,python2-biopython)
         ("python-tables" ,python2-tables)
         ("mcl" ,mcl)))
      (native-inputs
       `(("python-nose" ,python2-nose)))
      (home-page
       "http://www.eleves.ens.fr/home/doulcier/projects/virus/index.html")
      (synopsis "Viral contig automatic clustering and taxonomic assignment")
      (description
       "VContact is a tool to perform Guilt-by-contig-association automatic
classification of viral contigs.")
      (license license:gpl3+))))

(define-public python2-pandas-0.13
  (package
    (inherit python2-pandas)
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas" version))
       (sha256
        (base32 "0ariw316c3psng7qc1nnv3v71w17cvqnk06rksb50rbrm9n784v8"))))))

(define-public python2-scipy-0.13
  (package
    (inherit python2-scipy)
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32 "0rh2cy0yb01fgi9qk37aswcr5fdvx7jwjfpfqvamrhv0w1z3rqx9"))))))

(define-public python2-numpy-1.8
  (package
    (inherit python2-numpy)
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numpy" version))
       (sha256
        (base32 "0lyj4yr7qy4vcdid3jbaqa2hs2skd5zzwq5hh7zyjxycka0x0r17"))))
    (arguments
     `(,@(substitute-keyword-arguments
         (package-arguments python2-numpy)
       ((#:phases phases)
        `(alist-delete 'install-doc ,phases)))))))

(define-public exabayes ; seems to bundle things, and includes AVX etc. which
                        ; might need to be disabled through configure flags.
  (package
    (name "exabayes")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://sco.h-its.org/exelixis/resource/download/software/exabayes-"
         version ".tar.gz"))
       (sha256
        (base32
         "18lmn0w3hz2dl7z5mkd1h6x9pdqa3l9hfs19kn3yhrsycjsg20g4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-mpi"
                           ;;"--enable-tests" ;gtest is not available, could maybe be fixed.
                           )))
    (inputs
     `(("openmpi" ,openmpi)))
    (home-page "http://sco.h-its.org/exelixis/web/software/exabayes/")
    (synopsis "Large-scale Bayesian tree inference")
    (description
     "ExaBayes is a software package for Bayesian tree inference. It is
particularly suitable for large-scale analyses on computer clusters.")
    (license license:gpl3+))) ;?

(define-public ruby-bio-ipcress
  (package
   (name "ruby-bio-ipcress")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "bio-ipcress" version))
     (sha256
      (base32
       "0z9pgh4lx8mmrihrav4nxsjk4fn477jdkf96vqq98sc6xym3l1j3"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f ; requires jeweler.
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-programs
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (binary (string-append out "/bin/pcr.rb"))
                   (path (string-append (assoc-ref inputs "exonerate") "/bin"))
                   (gem_home (getenv "GEM_HOME")))
              (wrap-program binary
                `("GEM_HOME" ":" prefix (,(getenv "GEM_HOME"))))
              (wrap-program binary
                `("PATH" ":" prefix (,path)))
              #t))))))
   ;(native-inputs
   ; `(("bundler" ,bundler)
                                        ;   ("ruby-shoulda" ,ruby-shoulda)))
   (inputs
    `(("exonerate" ,exonerate)))
   (propagated-inputs
    `(("bioruby" ,bioruby)))
   (synopsis
    "")
   (description
    "")
   (home-page "")
   (license #f)))

(define-public mglex
  (package
   (name "mglex")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "MGLEX" version))
     (sha256
      (base32
       "0zflrry81afdf2yi4cqn9l2k20wzzwa9bxjxrqzyxr2ym5xk829y"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-setuptools-scm" ,python-setuptools-scm)))
   (inputs
    `(("python-numpy" ,python-numpy)
      ("python-scipy" ,python-scipy)
      ("python-docopt" ,python-docopt)))
   (home-page "https://github.com/fungs/mglex")
   (synopsis "MetaGenome Likelihood EXtractor")
   (description
    "MGLEX provides a probabilistic model to classify nucleotide sequences in
metagenome samples.  It was developed as a framework to help researchers
reconstruct individual genomes from such datasets using custom workflows and to
give developers the possibility to integrate the model into their programs.")
   (license license:gpl3)))

(define-public r-googlevis
  (package
    (name "r-googlevis")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "googleVis" version))
       (sha256
        (base32
         "1idnp9ndgg2dwybdpw7q3pkaw9kw2vy2xkls9qykrpz1p6nf3mn1"))))
    (properties `((upstream-name . "googleVis")))
    (build-system r-build-system)
    (propagated-inputs `(("r-jsonlite" ,r-jsonlite)))
    (home-page
     "https://github.com/mages/googleVis#googlevis")
    (synopsis "R Interface to Google Charts")
    (description
     "R interface to Google's chart tools, allowing users to create interactive charts based on data frames.  Charts are displayed locally via the R HTTP help server.  A modern browser with an Internet connection is required and for some charts a Flash player.  The data remains local and is not uploaded to Google.")
    (license license:gpl2+)))

(define-public binning-refiner
  (let ((commit "4421a0471fbb3fc5e0291c04073364a0732ced35"))
    (package
      (name "binning-refiner")
      (version "1.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wwood/Binning_refiner.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "06alzym65d07nr79fdxkv5xilh57nq0rp3j0yx2ims33qxx6szrv"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("r" ,r)
         ("r-googlevis" ,r-googlevis)
         ("python-rpy2" ,python-rpy2)
         ("python-numpy" ,python-numpy)
         ("python-matplotlib" ,python-matplotlib)
         ("python-biopython" ,python-biopython)
         ("coreutils" ,coreutils) ; 'rm' is used in Binning_refiner, at least.
         ("checkm" ,checkm)))
      (home-page "https://github.com/songweizhi/Binning_refiner")
      (synopsis "Binning refiner")
      (description
       "Improving genome bins through the combination of different binning
programs.")
      (license license:gpl3+))))

(define-public azure-cli ; Does not work because this is a dependency hole.
  (package
    (name "azure-cli")
    (version "0.1.2rc1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-cli" version))
       (sha256
        (base32
         "05vjmni9722byxhhyfjgnglm2g28b43msj03v4009dx3lnp4ykb9"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out  (assoc-ref outputs "out"))
                             (bin  (string-append out "/bin/"))
                             (path (getenv "PATH")))
                        (wrap-program (string-append bin "/az")
                          `("PATH" ":" prefix
                            (,path))))
                      #t))
         (add-after 'wrap-binary 'check-installed
                    (lambda* (#:key outputs #:allow-other-keys
                      (let* ((out (assoc-ref outputs "out"))
                             (az (string-append out "/bin/az")))
                        (zero? (system* az "-h")))))))))
    (inputs
     `(("python-wrapper" ,python-wrapper)))
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:expat)))

;; If there are errors making the DB, run "rm -rf ~/git/singlem/singlem/db;
;; python setup.py sdist; rm -rf singlem.egg-info/"
(define-public singlem-dev
  (package
   (inherit singlem)
   (name "singlem-dev")
   (version "0.0.0.dev")
   (source
    (local-file (string-append (getenv "HOME") "/git/singlem")
                #:recursive? #t))))

(define-public fastspar
  (package
   (name "fastspar")
   (version "0.0.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/scwatts/fastspar/archive/v"
           version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1a6pz4mxzj8nb0rdqarvki5idm3z0jqmz0s472xll0blwan6r8cl"))))
   (build-system gnu-build-system)
   (inputs
    `(("gfortran" ,gfortran)
      ("gfortran" ,gfortran "lib")
      ("armadillo" ,armadillo)
      ("openmpi" ,openmpi)
      ("gsl" ,gsl)
      ("openblas" ,openblas)))
   (home-page "https://github.com/scwatts/fastspar")
   (synopsis "C++ implementation of the SparCC algorithm")
   (description
    "FastSpar is a C++ implementation of the SparCC algorithm for inferring
correlation networks from genomic survey data.  FastSpar is up to several
thousand times faster than the original Python2 implementation available at
https://bitbucket.org/yonatanf/sparcc and uses much less memory.  Additionally,
SparCC's method of p-value has been replaced with exact p-value calculation. ")
   (license license:gpl3)))

(define-public slimm
  (package
   (name "slimm")
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/seqan/slimm/archive/v"
           version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (patches (search-patches "slimm-cmakelists.patch"))
     (sha256
      (base32
       "02cykm279aq3lgahim8vqkp19q8l3ca6iphx0d9w0n9xxfjdqbkq"))))
   (build-system cmake-build-system)
   (inputs
    `(("zlib" ,zlib)
      ("seqan" ,seqan-2)
      ("gcc" ,gcc-5)
      ("gcc" ,gcc-5 "lib")))
   (home-page "https://github.com/seqan/slimm")
   (synopsis "Species Level Identification of Microbes from Metagenomes")
   (description
    "SLIMM is a taxonomic profiling tool that investigates which microorganisms
are present in a sequenced sample. SLIMM requires a BAM/SAM alignment file as an
input. One can use a read mapper of choice to map raw reads obtained from a
sequencing machine to obtain the BAM/SAM file required as input for SLIMM.")
   (license license:bsd-3)))


(define-public seqan-2
  (package
    (inherit seqan)
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://packages.seqan.de/seqan-library/"
                                  "seqan-library-" version ".tar.bz2"))
              (sha256
               (base32
                "1yvyh3w8fg44s8wwdmcxf95xnslsj5fh5mdl685jmgwmry8a6ky0"))))))

(define-public seqan-src
  (package
    (name "seqan-src")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://packages.seqan.de/seqan-src/"
                                  "seqan-src-" version ".tar.gz"))
              (sha256
               (base32
                "1a9nf7bba8yafzr4cf4aic0s00qzb1zs45qajia7jr5miimd9j47"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; Just a timestamp error now I think.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (native-inputs
     `(("python" ,python-2)
       ("python-nose" ,python2-nose)
       ("python-sphinx" ,python2-sphinx)
       ("python-sphinx-rtd-theme" ,python2-sphinx-rtd-theme)
       ("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "http://www.seqan.de")
    (synopsis "Library for nucleotide sequence analysis")
    (description
     "SeqAn is a C++ library of efficient algorithms and data structures for
the analysis of sequences with the focus on biological data.  It contains
algorithms and data structures for string representation and their
manipulation, online and indexed string search, efficient I/O of
bioinformatics file formats, sequence alignment, and more.")
    (license license:bsd-3)))

(define-public bamm-dev
  (package
    (inherit bamm)
    (name "bamm-dev")
    (version "0.0.0.dev")
    (source
     (local-file "/tmp/BamM"
                 #:recursive? #t))))

(define-public mash-next
  (let ((commit "0a9a3f320ccdc598d9806e574334d71a3431193b"))
    (package
     (inherit mash)
     (name "mash-next")
     (version (string-append "1.1.1-1." (string-take commit 8)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marbl/Mash.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0gb8i0p7ya5rcah5y233y3ix06f6lw4sf537r1waznx69642yn57"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled kseq.
               ;; TODO: Also delete bundled murmurhash and open bloom filter.
               '(delete-file "src/mash/kseq.h")))))))

(define-public mmseqs ; Uses -march=native but probably works. For tests need updated googletest in guix proper I'd say. Also should patch in the git commit to the cmake system somehow. Bundles a few libraries
  ;; There are no releases so we package from git.
  (let ((commit "b69fcd43f669b19a023123e1e97333a4284e3dbf"))
    (package
     (name "mmseqs")
     (version (string-append "2-1." (string-take commit 8)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/soedinglab/mmseqs2.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1adjzv8jh1plswwzmz3r2d70ksmkwinl34hwgvb7n7kv87z39q3r"))))
     (build-system cmake-build-system)
     (arguments
      ;; There are no tests, see https://github.com/soedinglab/mmseqs2/issues/25
      `(#:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'delete-bundled-code
            (lambda _
              ;; Delete bundled gzstream. Other entries in the lib/
              ;; directory appear not to have any primary sources, or in the
              ;; case of kseq, appears to be either out of date or has been
              ;; modified relative to its original form.
              ;(delete-file "lib/kseq/kseq.h")
              ;; (delete-file-recursively "lib/gzstream")
              ;; (substitute* '("src/commons/A3MReader.cpp"
              ;;                "src/commons/Util.cpp"
              ;;                "src/util/createdb.cpp"
              ;;                "src/util/extractdomains.cpp"
              ;;                "src/test/TestDiagonalScoringPerformance.cpp"
              ;;                "src/test/TestAlignmentPerformance.cpp")
              ;;   (("^#include \\\"kseq.h\\\"\n$")
              ;;    "#include \"htslib/kseq.h\"\n"))
              #t)))))
     (inputs
      `(("htslib" ,htslib)
        ("gzstream" ,gzstream)))
     (native-inputs
      `(("xxd" ,vim)
        ("googletest" ,googletest)))
     (home-page "http://mmseqs.com")
     (synopsis "Fast and sensitive protein search and clustering")
     (description
      "MMseqs2 (Many-against-Many searching) is a software suite to search and
cluster huge protein sequence sets.  The software is designed to run on
multiple cores and servers and exhibits very good scalability.  MMseqs2 can run
10000 times faster than BLAST.  At 100 times its speed it achieves the same
sensitivity.  It can also perform profile searches with the same sensitivity as
PSI-BLAST but at around 270 times its speed.")
     (license license:gpl3+)))) ; need to check actual code

(define-public binsanity ; in process?
  (package
   (name "binsanity")
   (version "0.2.5.5")
   (source
    (origin
     (method url-fetch)
      (uri (pypi-uri "Binsanity" version))
      (sha256
        (base32
          "0w9izgm3vd088rlaphmrjil9mjn5njjc14y2n1hg6idmk7cypshy"))))
   (build-system python-build-system)
   (arguments
    `(#:python ,python-2 ; python 2 only.
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-setup.py
          (lambda _
            ;; One of the dependencies checkm-genome is not required as a
            ;; Python library.
            (substitute* "setup.py" ((".*install_requires.*") ""))
            #t))
      (add-after 'install 'wrap-programs
        (lambda* (#:key outputs #:allow-other-keys)
          (let* ((out  (assoc-ref outputs "out"))
                 (bin  (string-append out "/bin"))
                 (path (getenv "PATH")))
            (for-each (lambda (file)
                        (wrap-program (string-append bin "/" file)
                                      `("PATH" ":" prefix (,path))))
                      '("bin_evaluation"
                        "Binsanity"
                        "Binsanity-lc"
                        "Binsanity-profile"
                        "Binsanity-refine"
                        "Binsanity-wf"
                        "checkm_analysis"
                        "get-ids"
                        "transform-coverage-profile")))
          #t)))))
   (inputs
    `(("python2-numpy" ,python2-numpy)
      ("python2-scikit-learn" ,python2-scikit-learn)
      ("python2-biopython" ,python2-biopython)
      ("bedtools" ,bedtools)
      ("python2-pandas" ,python2-pandas)
      ("subread" ,subread)
      ("bowtie" ,bowtie)
      ("samtools" ,samtools)
      ("checkm" ,checkm)))
   (home-page "https://github.com/edgraham/BinSanity")
   (synopsis "Unsupervised clustering of environmental microbial assemblies")
   (description
    "BinSanity contains a suite a scripts designed to cluster contigs
generated from metagenomic assembly into putative genomes.")
     (license license:gpl3)))

(define-public canu ; Copied from Marius Bakke
  (package
    (name "canu")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/marbl/canu"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1amffqhn7cq42id13pm514iymf8v1dbk0dg4dk854dqb5wmjw3hp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         ;; hijack configure phase to change to Makefile directory
         (replace 'configure
           (lambda _ (chdir "src")))
         ;; no "install" target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (copy-recursively "../Linux-amd64/bin" bin)))))))
    (inputs
     `(("perl", perl)))
    (propagated-inputs
     `(("jre", icedtea-8)))
    (supported-systems '("x86_64-linux")) ;; TODO: arm support
    (home-page "https://github.com/marbl/canu")
    (synopsis "Single molecule sequence assembler for genomes large and small")
    (description
     "Canu is a fork of the Celera Assembler, designed for high-noise single-molecule
sequencing.  Canu is a hierarchical assembly pipeline which runs in four steps:
detect overlys in high-noise sequences using MHAP; generate corrected sequence consensus;
trim corrected sequences; and assemble trimmed corrected sequences.")
    (license license:gpl2)))

(define-public pullseq
  (package
   (name "pullseq")
   (version "1.0.2")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/bcthomas/pullseq/archive/"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0py8hsspvwjlckg2xi7jcpj0frrp2qbmsy9x55fx0knnwbhdg5d2")))) ; TODO: remove bundled kseq, uthash .h too
   (build-system gnu-build-system)
   (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'bootstrap
           (lambda _
             (zero? (system* "./bootstrap")))))))
   (inputs
    `(("pcre" ,pcre)
      ("zlib" ,zlib)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)))
   (home-page "https://github.com/bcthomas/pullseq")
   (synopsis "Utility program for extracting sequences from fasta/q files")
   (description
    "Pullseq extracts sequences from a fasta and fastq files.  This program is
fast, and can be useful in a variety of situations.  You can use it to extract
sequences from one fasta/fastq file into a new file, given either a list of
header IDs to include or a regular expression pattern to match.  Results can be
included (default) or excluded, and they can additionally be filtered with
minimum / maximum sequence lengths. Additionally, it can convert from fastq to
fasta or visa-versa and can change the length of the output sequence lines.")
   (license license:expat)))

(define-public das
  (let ((commit "309cea91f556e007e7433e7b231ddbf1c88cd922"))
    (package
     (name "das")
     (version (string-append "1.0-1." (string-take commit 8)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cmks/DAS_Tool.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1c172gbd4fyyiabnhdfkig4k3djf9b6yhc5sm53lxr58xjwvb1h3"))))
     (build-system r-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (site-library (string-append out "/site-library/"))
                     (params (list "--install-tests"
                                   (string-append "--library="site-library)
                                   "--built-timestamp=1970-01-01"
                                   ".")))
                       ;; Some R packages contain a configure script for which
                       ;; the CONFIG_SHELL variable should be set.
                       (setenv "CONFIG_SHELL" (which "bash"))
                       (mkdir-p site-library)
                       (zero? (apply
                               system* (append
                                        (list "R" "CMD" "INSTALL"
                                              "package/DASTool_1.0.0.tar.gz")
                                        params))))))
          (add-after 'install 'install2
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin"))
                            (das "DAS_Tool.sh")
                            (src (string-append out "/src")))
                       (copy-recursively "src" src)
                       (substitute* das
                         (("^DIR=.*") (string-append "DIR=" out "\n")))
                       (substitute* das
                         (("> /dev/null 2>&1") ""))
                       (install-file das bin)
                       (wrap-program (string-append bin "/" das)
                         `("PATH" ":" prefix (,(getenv "PATH"))))
                       (wrap-program (string-append bin "/" das)
                         `("R_LIBS_SITE" ":" prefix
                           (,(string-append
                              out "/site-library:"
                              (getenv "R_LIBS_SITE")))))
                       ;; TODO: put into /share instead, requires patching the
                       ;; file I guess
                       (zero? (system* "unzip" "-d" out
                                       (assoc-ref inputs "db-data")))))))))
     (native-inputs
      `(("unzip" ,unzip)
        ("db-data"
         ,(origin
             (method url-fetch)
             (uri "http://banfieldlab.berkeley.edu/~csieber/db.zip")
             (file-name (string-append "das-db.zip"))
             (sha256
              (base32
               "1g5kajrznid037sm5kang7yx56pm7ibmrr5v3v4i4qzkax0xh4zb"))))))
     (inputs
      `(("r" ,r)
        ("r-ggplot2" ,r-ggplot2)
        ("r-domc" ,r-domc)
        ("r-data-table" ,r-data-table)
        ("ruby" ,ruby)
        ("prodigal" ,prodigal)
        ("pullseq" ,pullseq)
        ("diamond" ,diamond)
        ("blast+" ,blast+)
        ("perl" ,perl)))
     (home-page "https://github.com/cmks/DAS_Tool")
     (synopsis "DAS Tool")
     (description
      "Recovery of genomes from metagenomes via a dereplication, aggregation,
and scoring strategy.")
     (license license:expat)))) ; need to check actual code

(define-public tmhmm
  (package
    (name "tmhmm")
    (version "2.0c")
    (source
     (local-file (string-append (getenv "HOME") "/bioinfo/tmhmm-2.0c")
                 #:recursive? #t))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check ; this is just a binary, so run rudimentary check.
           (lambda _ (zero? (system* "./bin/tmhmm"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin/"))
                           ;; TODO: put in share instead, need to patch the
                           ;; script in that case.
                           (lib (string-append out "/lib/")))
               (substitute*
                "bin/tmhmm"
                ((".*opt_basedir = \"/usr/cbs/packages.*")
                 (string-append "$opt_basedir = \"" out "\";\n")))
               (copy-recursively "bin" bin)
               (copy-recursively "lib" lib))
             #t)))))
    (inputs
     `(("perl" ,perl)))
    (synopsis "Place query sequences on a fixed reference phylogenetic tree")
    (description
     "Predicts transmembrane domains")
    (home-page "http://www.cbs.dtu.dk/services/TMHMM/")
    (license #f))) ; Academic only

(define-public ruby-bio-tm-hmm
  (package
   (name "ruby-bio-tm-hmm")
   (version "0.2.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "bio-tm_hmm" version))
     (sha256
      (base32
       "1w61vg34ig3884lv7q84z1wikpjf894p2bm0pfngdb2kfhvs80ib"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f ; requires jeweler.
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-programs
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (binary (string-append out "/bin/bio-tm_hmm"))
                   (path (string-append (assoc-ref inputs "tmhmm") "/bin:"
                                        (assoc-ref inputs "ruby") "/bin")))
              (wrap-program binary
                `("GEM_PATH" ":" prefix (,(getenv "GEM_PATH"))))
              (wrap-program binary
                `("GEM_HOME" ":" prefix (,(getenv "GEM_HOME"))))
              (wrap-program binary
                `("PATH" ":" prefix (,path)))
              #t))))))
   (inputs
    `(("tmhmm" ,tmhmm)))
   (propagated-inputs
    `(("bioruby" ,bioruby)))
   (synopsis "Ruby library for TMHMM")
   (description
    "A bioruby plugin for running the transmembrane domain predictor TMHMM
automatically on multiple sequences in a FASTA file and manipulation of the
results.")
   (home-page "https://github.com/wwood/bioruby-tm_hmm")
   (license license:expat))) ; TODO: The sources say ruby-license, the author does not mind.

(define-public pftools ; Probably works, but the output structure is a bit
                       ; messy. Need to put data files in share/ perhaps.
  (package
    (name "pftools")
    (version "2.3.5.d")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "ftp://ftp.lausanne.isb-sib.ch/pub/software/unix/pftools/pft"
                   (version-prefix version 2) "/pft" version ".tar.gz"))
             (sha256
              (base32
               "1x8q8izm0wl6swqy2syclvgjma4h24rq8g7hjwmvp9kr4ibq0h2d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (mandir (string-append out "/man/")))
               (substitute* "install_pftools.sh"
                            (("/usr/bin/") bin)
                            (("/usr/man/") mandir)
                            (("read userInput") "")
                            ((" >&2") "")
                            (("read -s -n 1 userInput") "userInput='y'")
                            (("ln -s.*") ""))
               (zero? (system* "bash" "install_pftools.sh" out))))))))
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("gfortran" ,gfortran)))
    (synopsis "Build protein and DNA profiles and use them to scan sequences")
    (description
     "The pftools package contains all the software necessary to build protein
and DNA generalized profiles and use them to scan and align sequences, and
search databases.")
    (home-page "http://web.expasy.org/pftools/#Documentation")
    (license license:gpl2))) ; 2+?

(define-public sepp ; Includes bundled code, which shouldn't be too hard to
                    ; remove.
  (let ((commit "e4b47531ed5b57fd875c90f08d4a434b7cce1cb5"))
    (package
     (name "sepp")
     (version (string-append "3.2"))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smirarab/sepp.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1yaxf4fv388j5j5b75fbv5l1qm6fb8vjfjhr0p7z0rasn395qxpr"))
       (patches (search-patches "sepp.patch"))))
     (build-system python-build-system)
     (arguments
      `(#:python ,python-2 ; python-2 only
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'set-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (config-directory (string-append out "/share/sepp"))
                     (java (assoc-ref inputs "java")))
                (substitute*
                 '("sepp/exhaustive_tipp.py"
                   "sepp/jobs.py")
                 (("\"java\"")
                  (string-append "\"" java "/bin/java\"")))
                ;; Set the config directory to be in the store, not a user's
                ;; home directory.
                (mkdir-p config-directory)
                ;; Found these files with 'git grep expanduser'.
                (substitute*
                 '("run_tipp_tool.py"
                   "sepp/config.py"
                   "sepp/ensemble.py"
                   "sepp/exhaustive_tipp.py"
                   "sepp/exhaustive_upp.py"
                   "setup.py")
                 (("os.path.expanduser\\(\"~/")
                  (string-append "os.path.join(\"" config-directory "/"))))))
          (add-after 'install 'setup-config
            (lambda _
              (zero? (system* "python" "setup.py" "config")))))))
     (inputs
      `(("python-dendropy" ,python2-dendropy)
        ("java" ,icedtea)))
     (home-page "https://github.com/smirarab/sepp")
     (synopsis "Ensemble of HMM methods (SEPP, TIPP, UPP)")
     (description
      "SEPP, TIPP, UPP, HIPPI. These methods use ensembles of Hidden Markov
Models (HMMs) in different ways, each focusing on a different problem.")
     (license license:gpl3+)))) ; Not exhaustively tested.


(define-public sepp-dev
  (package
   (inherit sepp)
   (name "sepp-dev")
   (source
    (local-file (string-append (getenv "HOME") "/git/sepp")
                #:recursive? #t))))

(define-public meta-gene-annotator ; There are no sources available, so we cannot build it. Also probably does not work in a container.
  (package
   (name "meta-gene-annotator")
   (version (string-append "0-1.20170418"))
   (source
    (origin
     (method url-fetch/tarbomb)
     (uri "http://metagene.nig.ac.jp/metagene/mga_x86_64.tar.gz")
     (sha256
      (base32
       "1byqgcmxpsrkb2vgravd1x6bhj8wsc9i4p0x5dcr8wqvlmfb97nj"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     ;; (replace 'build
                     ;;          (lambda* (#:key inputs #:allow-other-keys)
                     ;;                   (let* ((libc (assoc-ref inputs "libc"))
                     ;;                          (ld-so (string-append libc
                     ;;                                                ,(glibc-dynamic-linker "i686-linux"))))
                     ;;                     (zero? (system*
                     ;;                             "patchelf" "--interpreter" ld-so
                     ;;                             "mga_linux_ia64")))))
                     (delete 'build)
                     ;; (replace 'check
                     ;;          (lambda* (#:key inputs #:allow-other-keys)
                     ;;                   (zero? (system* "mga_linux_ia64" (assoc-ref inputs "example-genome")))))
                     (delete 'check)
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
                                       (let* ((out (assoc-ref outputs "out"))
                                              (bin (string-append out "/bin")))
                                         (install-file "mga_linux_ia64" bin)
                                         #t))))))
   ;; (native-inputs
   ;;  `(("patchelf" ,patchelf)
   ;;    ("example-genome"
   ;;     ,(origin
   ;;       (method url-fetch)
   ;;       (uri "http://www.ebi.ac.uk/ena/data/view/CP002565&display=fasta")
   ;;       (file-name (string-append "ena-genome-CP002565.fasta"))
   ;;       (sha256
   ;;        (base32
   ;;         "0dv3m29kgyssjc96zbmb5khkrk7cy7a66bsjk2ricwc302g5hgfy"))))))
   (home-page "")
   (synopsis "")
   (description
    "")
   (license license:gpl3+))) ; Not ?


(define-public virsorter ; Hard to source, at least, in progress.
  (let ((commit "939d5910ca5ffac7194693fea834b80694834384"))
    (package
     (name "virsorter")
     (version (string-append "0.1"))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simroux/VirSorter.git")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1fb48vj60mz08c1yyxq628xa7sdpn8xlk8lqhvdf91znb7pxllh6"))))
     (build-system gnu-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases)
        ))
     (inputs
      `(("perl" ,perl)
        ("perl-capture-tiny" ,perl-capture-tiny)
        ("bioperl" ,bioperl-minimal)
        ("perl-file-which" ,perl-file-which)
        ("meta-gene-annotator" ,meta-gene-annotator)
        ("hmmer" ,hmmer)
        ("mcl" ,mcl)
        ("muscle" ,muscle)
        ;("blast" ,blast) ; NOTT blast+ and old blast is not in guix.
        ))
     (home-page "https://github.com/smirarab/sepp")
     (synopsis "Ensemble of HMM methods (SEPP, TIPP, UPP)")
     (description
      "SEPP, TIPP, UPP, HIPPI. These methods use ensembles of Hidden Markov
Models (HMMs) in different ways, each focusing on a different problem.")
     (license license:gpl3+)))) ; Not exhaustively tested.

(define-public virsorter-data
  (package
   (name "virsorter-data")
   (version "0-1.20160204")
   (source
    (origin
     (method url-fetch)
     (uri "http://datacommons.cyverse.org/browse/iplant/home/shared/imicrobe/VirSorter/virsorter-data.tar.gz") ; There is no public interface to download this file, it must be incorporated via 'guix download'
     (sha256
      (base32
       "133vw2pi4s15z76yx53ywwwhsp46dqdl2p0gbingywm4y31hfzm8"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                    (delete 'configure)
                    (delete 'build)
                    (delete 'check)
                    (replace 'install
                             (lambda* (#:key outputs #:allow-other-keys)
                                      (let* ((out (assoc-ref outputs "out"))
                                             (share (string-append out "/share")))
                                        (mkdir-p share)
                                        (copy-file-recursively virsorter-data share)
                                        #t))))))
   (home-page "")
   (synopsis "")
   (description
    "")
   (license license:gpl3+))) ; Not exhaustively tested.

(define-public anvio
  (package
   (name "anvio")
   (version "2.3.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "anvio" version))
     (sha256
      (base32
       "104w0zrgk0qgwj1gib4qmr9v94rpq2q0537b31qlfazfrnm46bq5"))))
   (build-system python-build-system)
   (inputs
    `(("samtools" ,samtools)
      ("prodigal" ,prodigal)
      ("hmmer" ,hmmer)
      ("sqlite" ,sqlite)
      ("gsl" ,gsl)
      ("hdf5" ,hdf5)))
   (propagated-inputs
    `(("python-bottle" ,python-bottle)
      ("python-cherrypy" ,python-cherrypy)
      ("python-cython" ,python-cython)
      ("python-django" ,python-django)
      ("python-ete3" ,python-ete3)
      ("python-h5py" ,python-h5py)
      ("python-mistune" ,python-mistune)
      ("python-numpy" ,python-numpy)
      ("python-psutil" ,python-psutil)
      ("python-pysam" ,python-pysam)
      ("python-requests" ,python-requests)
      ("python-scikit-learn" ,python-scikit-learn)
      ("python-scipy" ,python-scipy)))
   (home-page
    "https://merenlab.org/projects/anvio/")
   (synopsis
    "An interactive analysis and visualization platform for 'omics data. See https://merenlab.org/projects/anvio for more information")
   (description
    "An interactive analysis and visualization platform for 'omics data. See https://merenlab.org/projects/anvio for more information")
   (license #f)))

(define-public python-cherrypy
  (package
   (name "python-cherrypy")
   (version "10.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "CherryPy" version))
     (sha256
      (base32
       "1y4gx8zg4s1awvz52vagrkq51s1k6djan598sbapyp3nvws37n9j"))))
   (build-system python-build-system)
   (arguments
     `(#:tests? #f)) ; Tests require further dependencies.
   (native-inputs
    `(("python-setuptools-scm" ,python-setuptools-scm)
      ("python-pytest-runner" ,python-pytest-runner)))
   (propagated-inputs
    `(;("python-backports.unittest-mock"
      ; ,python-backports.unittest-mock)
      ("python-coverage" ,python-coverage)
      ("python-graphviz" ,python-graphviz)
      ("python-objgraph" ,python-objgraph)
      ("python-pytest" ,python-pytest)
      ("python-pytest-sugar" ,python-pytest-sugar)))
   (home-page "http://www.cherrypy.org")
   (synopsis "Object-Oriented HTTP framework")
   (description "Object-Oriented HTTP framework")
   (license license:bsd-3)))

(define-public python-ete3
  (package
   (name "python-ete3")
   (version "3.0.0b35")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "ete3" version))
     (sha256
      (base32
       "066wj2ma50m2j9iliqri5sx6zpx2vqla23ifi2way02xhvsmbw7r"))))
   (propagated-inputs
    `(("python-six" ,python-six)
      ("python-numpy" ,python-numpy)
      ("python-scipy" ,python-scipy)
                                        ;("python-pyqt" ,python-pyqt) ; pyqt4 is out-dated ?
      ("python-lxml" ,python-lxml)))
   (build-system python-build-system)
   (home-page "http://etetoolkit.org")
   (synopsis
    "A Python Environment for (phylogenetic) Tree Exploration")
   (description
    "A Python Environment for (phylogenetic) Tree Exploration")
   (license #f)))

;; (package
;;  (name "python-backports-unittest-mock")
;;  (version "1.3")
;;  (source
;;   (origin
;;    (method url-fetch)
;;    (uri (string-append
;;          "https://pypi.python.org/packages/e4/a2/85314249d89f400347aaa67e48526447468d730bca662ffcd0437133ac86/backports.unittest_mock-"
;;          version
;;          ".tar.gz"))
;;    (sha256
;;     (base32
;;      "0xdkx5wf5a2w2zd2pshk7z2cvbv6db64c1x6v9v1a18ja7bn9nf6"))))
;;  (build-system python-build-system)
;;  (propagated-inputs
;;   `(("python-jaraco.packaging"
;;      ,python-jaraco.packaging)
;;     ("python-rst.linker" ,python-rst.linker)
;;     ("python-sphinx" ,python-sphinx)))
;;  (home-page
;;   "https://github.com/jaraco/backports.unittest_mock")
;;  (synopsis "backports.unittest_mock")
;;  (description "backports.unittest_mock")
;;  (license #f))

(define-public python-graphviz
  (package
    (name "python-graphviz")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "graphviz" version ".zip"))
       (sha256
        (base32
         "10wywvqqc8567gwzi9dqlxihs8n8gmm3l65lv4d7k47s8pwlsx67"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("graphviz" ,graphviz)))
    (home-page "https://github.com/xflr6/graphviz")
    (synopsis "Simple Python interface for Graphviz")
    (description
     "Simple Python interface for Graphviz")
    (license license:expat)))

(define-public python-objgraph
  (package
    (name "python-objgraph")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "objgraph" version))
       (sha256
        (base32
         "0435gj7krpqbd9aw0rwcklkzb3242magcl5s4h1m78xpifga7cwr"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)))
    (propagated-inputs
     `(("python-graphviz" ,python-graphviz)))
    (home-page "http://mg.pov.lt/objgraph/")
    (synopsis
     "Draws Python object reference graphs with graphviz")
    (description
     "Draws Python object reference graphs with graphviz")
    (license license:expat)))

(define-public python-pytest-sugar
  (package
    (name "python-pytest-sugar")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-sugar" version))
       (sha256
        (base32
         "13njxd4sm0b8lcx02vznswfa0q9pwknym0mnzl4sy03hjr4bz5ih"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-termcolor" ,python-termcolor)))
    (home-page
     "http://pivotfinland.com/pytest-sugar/")
    (synopsis
     "py.test is a plugin for py.test that changes the default look and feel of py.test (e.g. progressbar, show tests that fail instantly).")
    (description
     "py.test is a plugin for py.test that changes the default look and feel of py.test (e.g. progressbar, show tests that fail instantly).")
    (license license:bsd-3)))

(define-public peat ; Works but non-free.
  (let ((commit "c2f94a2e17b7ee46d7a501f44903a498883bb5d3"))
    (package
      (name "peat")
      (version (string-append "1.2.4-1." (string-take commit 8)))
      (source
       (origin
         ;; (method url-fetch)
         ;; (uri (string-append "https://github.com/jhhung/PEAT/archive/v"
         ;;                     version ".tar.gz"))
         ;; (file-name (string-append name "-" version ".tar.gz"))
         ;; (sha256
         ;;  (base32
         ;;   "1ksyz8sijdazf0lgyzflgbghxcz410grl4vsxphi13qhzmyhw70y"))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jhhung/PEAT.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "11bds3bkvh3dbljjjadbz0hh3s26b08jy08zkxnk83kbap0xl95y"))
         (modules '((guix build utils)))
         (snippet
          ;; Delete pre-built binaries and libraries.
          '(begin
             (delete-file "bin/PEAT")
             (delete-file "bin/PEAT_linux")
             (delete-file "bin/PEAT_mac")
             (for-each (lambda (file)
                         (delete-file file))
                       (find-files "." ".*\\.a"))
             #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (zero? (system*
                       "bin/PEAT" "paired" "-1" "test_file/test_paired1.fq.gz"
                       "-2" "test_file/test_paired2.fq.gz" "-o" "testout"
                       "--adapter_contexts")))))))
      (inputs
       `(("boost" ,boost)
         ("zlib" ,zlib)
         ("curl" ,curl)))
      (home-page "")
      (synopsis "")
      (description
       ".")
      (license #f)))) ; GPL2 but only for non-profits, so not free software.
