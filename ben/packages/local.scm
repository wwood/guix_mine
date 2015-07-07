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
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
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

(define-public fxtract ; dont think this works. Writing a proper configure.ac etc is probably a good idea, it would simplify things here a lot.
  ;(let ((commit ))
    (package
      (name "fxtract")
      (version "1.2-dev")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;;(url "https://github.com/ctSkennerton/fxtract.git")
		      (url "file:///home/ben/git/fxtract")
                      (commit "a90a6a84f")
                      (recursive? #t)))
                (sha256
                 (base32
                  "1zg6l4v7icv5nk0gylncnji13733a4622hs4pl3jjhfxm411j6sq"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'install
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (let ((bin (string-append (assoc-ref
                                                              outputs "out")
                                                             "/bin")))
                                     (mkdir-p bin)
                                     (copy-file "fxtract"
                                                (string-append bin "/fxtract"))
                                     #t)))
                        (replace 'check
                                 (lambda* _
                                   (zero? (system* "make"
                                                   "test_fxtract"))))
                        )))
      (inputs ;;BOOST?
       `(("pcre" ,pcre)
	 ("bzip2" ,bzip2)
	 ("zlib" ,zlib)))
      (home-page "https://github.com/ctSkennerton/fxtract")
      (synopsis "Extract sequences from a fastx file given a subsequence or identifier")
      (description
       "Extract sequences from a fastx (fasta or fastq) file given a subsequence.
Currently uses a variety of search algorithms depending on the task.  Currently
searches using a simple substring search for basic tasks but can change to using
POSIX regular expressions, PCRE, hash lookups or multi-pattern searching as
required.  By default will look in the sequence of each record but can be told
to look in the header, comment or quality sections of a record.")
    (license license:gpl3+)))

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

(define-public yaggo ; patch sent to guix-devel mailing list
  (package
   (name "yaggo")
   (version "1.5.4")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/gmarcais/yaggo/archive/v"
                   version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1mxfvrim03xg80agws9zdpk00r0kjpqhw3xbli0w8wvsnsa274y3"))))
   (build-system ruby-build-system)
   (arguments
    `(
      ;; No rake test, and Makefile in test/ appears malformed.
      ;; See https://github.com/gmarcais/yaggo/issues/3
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
        (replace 'build (lambda* _ (zero? (system* "rake" "gem")))))))
   (synopsis "Generate C++ command line parsers using getopt_long")
   (description "Yaggo is a tool to generate command line parsers for C++.
Yaggo stands for 'Yet Another GenGetOpt' and is inspired by GNU Gengetopt.  It
reads a configuration file describing the switches and argument for a C++
program and it generates one header file that parses the command line using
getopt_long(3).")
   (home-page "https://github.com/gmarcais/yaggo")
   (license license:gpl3+)))

(define-public jellyfish ;;currently install works fine but make check fails
  (package
    (name "jellyfish")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              ;;              (uri (string-append "https://github.com/gmarcais/Jellyfish/archive/v"
              ;;                                version ".tar.gz"))
              (uri "file:///home/ben/t/Jellyfish-2.2.1.tar.gz")
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02mjfabcjjlp25qi222w4zbghz75idsac3d1wmr2vs8vvyc5aq4i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
                      (add-after
                       'unpack 'autoreconf
                       (lambda* _
                         (and (zero? (system*
                                      "autoreconf" "-i"))
                              ;; Makefile.in currently hard codes /bin/sh
                              ;; but expects bash. Fixed in upstream but not
                              ;; released as of 2.2.1
                              (substitute* "Makefile.in"
                                           (("SH_LOG_COMPILER = /bin/sh")
                                            (string-append
                                             "SH_LOG_COMPILER = "
                                             (which "bash"))))))))))
    (native-inputs
     `(("yaggo" ,yaggo)
       ("ruby" ,ruby)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("file" ,file)
       ("gzip" ,gzip) ;; deps for make check from here down: gunzip, time, echo, date, bc
       ("bc" ,bc)
       ("coreutils" ,coreutils)
       ))
    (home-page "http://www.genome.umd.edu/jellyfish.html")
    (synopsis "A fast multi-threaded k-mer counter")
    (description
     "Jellyfish is a tool for fast, memory-efficient counting of
k-mers in DNA.  A k-mer is a substring of length k, and counting the
occurrences of all such substrings is a central step in many analyses
of DNA sequence.  Jellyfish can count k-mers using an order of
magnitude less memory and an order of magnitude faster than other
k-mer counting packages by using an efficient encoding of a hash table
and by exploiting the 'compare-and-swap' CPU instruction to increase
parallelism.")
    (license (license:non-copyleft "file://src/LICENSE"
                                   "See src/LICENSE in the distribution."))))

(define-public krona-tools ; mostly works? database cannot be used though. See http://sourceforge.net/p/krona/tickets/10/
  (package
   (name "krona-tools")
   (version "2.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://sourceforge.net/projects/krona/files/KronaTools"
                  "%20%28Mac%2C%20Linux%29/KronaTools-"
                  version ".tar/download"))
            (file-name (string-append name "-" version ".tar"))
            (sha256
             (base32
              "12ps0y7p19d3cdhc7pp3xlak5k9qq5w246861bngfg8mkfa083qa"))))
   (build-system perl-build-system)
   (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin   (string-append (assoc-ref outputs "out") "/bin"))
                          (perl  (string-append (assoc-ref outputs "out")
                                                "/lib/perl5/site_perl"))
                          (share (string-append (assoc-ref outputs "out") "/share/krona-tools")))
                      (mkdir-p bin)
                      (for-each (lambda (script)
                                  (let* ((executable (string-append "scripts/" script ".pl")))
                                    (substitute* executable
                                      (("use lib (`ktGetLibPath`);") ""))
                                    (copy-file executable
                                               (string-append bin "/kt" script))))
                                '("ClassifyBLAST"
                                  "GetContigMagnitudes"
                                  "GetTaxIDFromGI"
                                  "ImportBLAST"
                                  "ImportDiskUsage"
                                  "ImportEC"
                                  "ImportFCP"
                                  "ImportGalaxy"
                                  "ImportMETAREP-BLAST"
                                  "ImportMETAREP-EC"
                                  "ImportMGRAST"
                                  "ImportPhymmBL"
                                  "ImportRDP"
                                  "ImportRDPComparison"
                                  "ImportTaxonomy"
                                  "ImportText"
                                  "ImportXML"))
                      (mkdir-p share)
                      (copy-recursively "data" (string-append share "/data"))
                      (copy-recursively "img" (string-append share "/img"))
                      (copy-recursively "taxonomy" (string-append share "/taxonomy"))
                      (mkdir-p perl)
                      ;;$libPath/../img/
                      ;;'img/hidden.png';
                      (substitute* '("lib/KronaTools.pm")
                        (("taxonomyDir = \".libPath/../taxonomy\"")
                         (string-append "taxonomyDir = \"" share "/taxonomy\"")))
                      (copy-file "lib/KronaTools.pm" (string-append perl "/KronaTools.pm")))))
         (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; TODO? Make sure scripts find all regular perl inputs at runtime.
                      (let* ((out (assoc-ref outputs "out"))
                             (path (getenv "PERL5LIB")))
                        (for-each (lambda (executable)
                                    (wrap-program executable
                                      `("PERL5LIB" ":" prefix
                                        (,(string-append out
                                                         "/lib/perl5/site_perl")))))
                                  (find-files (string-append out "/bin/") ".*"))))))))
   (inputs
    `(("perl" ,perl)))
   (home-page "http://sourceforge.net/projects/krona")
   (synopsis "Hierarchical data exploration with zoomable HTML5 pie charts")
   (description
    "Krona is a flexible tool for exploring the relative proportions of
hierarchical data, such as metagenomic classifications, using a radial,
space-filling display.  It is implemented using HTML5 and JavaScript, allowing
charts to be explored locally or served over the Internet, requiring only a
current version of any major web browser.")
   (license (license:non-copyleft "file://src/LICENSE"
                                  "See src/LICENSE in the distribution."))))


(define-public jalview ;;untested, likely doesn't work
  (package
   (name "jalview")
   (version "2.8.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.jalview.org/source/jalview_"
                  (regexp-substitute/global
                   #f "\\." version 'pre "_" 'post)
                  ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "12z7hqrqq3rccw6rgjc2gl9bnbkq4fnlw37267ax79mgdj15vi49"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("jdk" ,icedtea6 "jdk") ;;TODO: this version of java needed specifically?
      ("perl" ,perl)))
   (home-page "http://www.jalview.org")
   (synopsis "Multiple sequence alignment editing, visualisation and analysis")
   (description
    "Use it to view and edit sequence alignments, analyse them with
phylogenetic trees and principal components analysis (PCA) plots and explore
molecular structures and annotation.  Jalview has built in DNA, RNA and protein
sequence and structure visualisation and analysis capabilities.  It uses Jmol to
view 3D structures, and VARNA to display RNA secondary structure.")
   (license license:gpl3+))) ;; TODO: check what version of GPL


(define-public python2-seqmagick ; newer version would be better, see https://github.com/fhcrc/seqmagick/issues/51
  (package
    (name "python2-seqmagick")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/seqmagick/seqmagick-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06qi5534apbr61kd01d1zfvmwfjvnfqlvdjmkb1ip1xbvc9v4jsw"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (inputs
     `(("python2-setuptools" ,python2-setuptools)
       ("python2-biopython" ,python2-biopython)))
    (home-page "http://github.com/fhcrc/seqmagick")
    (synopsis
     "Tools for converting and modifying sequence files from the command-line")
    (description
     "Bioinformaticians often have to convert sequence files between formats
and do little manipulations on them, and it's not worth writing scripts for
that.  seqmagick is a kickass little utility to expose the file format
conversion in BioPython in a convenient way.  Instead of having a big mess of
scripts, there is one that takes arguments.")
    (license license:gpl3)))

(define-public metabat ; seems to work, just waiting for preseq merge on official
  (package
    (name "metabat")
    (version "0.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bitbucket.org/berkeleylab/metabat/get/"
                    version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vgrhbaxg4dkxyax2kbigak7w0arhqvw0szwp6gd9wmyilc44kfa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* _
                    (substitute* "SConstruct"
                      (("env.Install.idir_prefix, 'README.md'.")
                       ""))
                    #t))
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (substitute* "mehSConstruct"
                      (("/include/bam/bam.h")
                       "/include/samtools/bam.h"))
                    (substitute* "src/BamUtils.h"
                      (("#include .bam/bam.h.")
                       "#include \"samtools/bam.h\""))
                    (substitute* "src/BamUtils.h"
                      (("#include .bam/sam.h.")
                       "#include \"samtools/sam.h\""))
                    (substitute* "src/KseqReader.h"
                      (("#include \"bam/kseq.h\"")
                       "#include \"samtools/kseq.h\""))
                    (mkdir (assoc-ref outputs "out"))
                    (zero? (system* "scons"
                                    (string-append
                                     "PREFIX="
                                     (assoc-ref outputs "out"))
                                    (string-append
                                     "HTSLIB_DIR="
                                     (assoc-ref inputs "htslib"))
                                    (string-append
                                     "SAMTOOLS_DIR="
                                     (assoc-ref inputs "samtools"))
                                    (string-append
                                     "BOOST_ROOT="
                                     (assoc-ref inputs "boost"))
                                    "install"))))
         (delete 'install))))
    (inputs
     `(("zlib" ,zlib)
       ("perl" ,perl)
       ("samtools" ,samtools)
       ("htslib" ,htslib)))
    (native-inputs
     `(("boost" ,boost)
       ("scons" ,scons)))
    (home-page "")
    (synopsis "Reconstruction of single genomes from complex microbial communities")
    (description
     "Grouping large genomic fragments assembled from shotgun metagenomic
sequences to deconvolute complex microbial communities, or metagenome binning,
enables the study of individual organisms and their interactions.  MetaBAT is an
automated metagenome binning software, which integrates empirical probabilistic
distances of genome abundance and tetranucleotide frequency.")
   (license (license:non-copyleft "file://license.txt"
                                  "See LICENSE in the distribution."))))

(define-public samtools ; required for metabat - includes header files, patch for preseq from Ricardo, see mailing list
  (package
    (name "samtools")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1y5p2hs4gif891b4ik20275a8xf3qrr1zh9wpysp4g8m0g1jckf2"))))
    (build-system gnu-build-system)
    (arguments
     `(;; There are 87 test failures when building on non-64-bit architectures
       ;; due to invalid test data.  This has since been fixed upstream (see
       ;; <https://github.com/samtools/samtools/pull/307>), but as there has
       ;; not been a new release we disable the tests for all non-64-bit
       ;; systems.
       #:tests? ,(string=? (or (%current-system) (%current-target-system))
                           "x86_64-linux")
       #:modules ((ice-9 ftw)
                  (ice-9 regex)
                  (guix build gnu-build-system)
                  (guix build utils))                
       #:make-flags (list "LIBCURSES=-lncurses"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (alist-cons-after
        'unpack
        'patch-tests
        (lambda* (#:key inputs #:allow-other-keys)
          (let ((bash (assoc-ref inputs "bash")))
            (substitute* "test/test.pl"
              ;; The test script calls out to /bin/bash
              (("/bin/bash")
               (string-append bash "/bin/bash"))
              ;; There are two failing tests upstream relating to the "stats"
              ;; subcommand in test_usage_subcommand ("did not have Usage"
              ;; and "usage did not mention samtools stats"), so we disable
              ;; them.
              (("(test_usage_subcommand\\(.*\\);)" cmd)
               (string-append "unless ($subcommand eq 'stats') {" cmd "};")))))
        (alist-cons-after
         'install 'install-library
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
             (mkdir-p lib)
             (copy-file "libbam.a" (string-append lib "/libbam.a"))))
         (alist-cons-after
          'install 'install-headers
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((include (string-append (assoc-ref outputs "out")
                                          "/include/samtools/")))
              (mkdir-p include)
              (for-each (lambda (file)
                          (copy-file file (string-append include
                                                         (basename file))))
                        (scandir "." (lambda (name) (string-match "\\.h$" name))))
              #t))
          (alist-delete 'configure %standard-phases))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)
              ("python" ,python)
              ("zlib" ,zlib)))
    (home-page "http://samtools.sourceforge.net")
    (synopsis "Utilities to efficiently manipulate nucleotide sequence alignments")
    (description
     "Samtools implements various utilities for post-processing nucleotide
sequence alignments in the SAM, BAM, and CRAM formats, including indexing,
variant calling (in conjunction with bcftools), and a simple alignment
viewer.")
    (license license:expat)))

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
