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


(define-public bedtools
  (package
    (name "bedtools")
    (version "2.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arq5x/bedtools2/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lnxrjvs3nnmb4bmskag1wg3h2hd80przz5q3xd0bvs7vyxrvpbl"))
              ;; Fixed in upstream, see
              ;; https://github.com/arq5x/bedtools2/issues/271
              (patches (list (search-patch "bedtools-32bit-compilation.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("python" ,python-2)))
    (inputs `(("samtools" ,samtools)
              ("zlib" ,zlib)))
    (arguments
     '(#:test-target "test"
       #:phases
       (alist-cons-after
        'unpack 'patch-makefile-SHELL-definition
        (lambda _
          ;; patch-makefile-SHELL cannot be used here as it does not
          ;; yet patch definitions with `:='.  Since changes to
          ;; patch-makefile-SHELL result in a full rebuild, features
          ;; of patch-makefile-SHELL are reimplemented here.
          (substitute* "Makefile"
            (("^SHELL := .*$") (string-append "SHELL := " (which "bash") " -e \n"))))
        (alist-delete
         'configure
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
              (mkdir-p bin)
              (for-each (lambda (file)
                          (copy-file file (string-append bin (basename file))))
                        (find-files "bin" ".*"))))
          %standard-phases)))))
    (home-page "https://github.com/arq5x/bedtools2")
    (synopsis "Tools for genome analysis and arithmetic")
    (description
     "Collectively, the bedtools utilities are a swiss-army knife of tools for
a wide-range of genomics analysis tasks.  The most widely-used tools enable
genome arithmetic: that is, set theory on the genome.  For example, bedtools
allows one to intersect, merge, count, complement, and shuffle genomic
intervals from multiple files in widely-used genomic file formats such as BAM,
BED, GFF/GTF, VCF.")
    (license license:gpl2)))


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

(define-public jellyfish ;;currently install works fine but make check fails. Also need to change it so the build process does not query /proc/cpuinfo, for purity purposes.
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
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/g/graftm/graftm-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w95ajlk7xvjl5glzx6wrwx72d6kfbgrj9690m36qz6r26fik3ip"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("krona-tools" ,krona-tools)
       ("orfm" ,orfm)
       ("diamond" ,diamond)
                                        ;    ("fxtract" ,fxtract)
                                        ;    ("fasttree" ,fasttree)
                                        ;    ("python-biopython" ,python2-biopython)
                                        ;    ("pplacer" ,pplacer)
                                        ;    ("seqmagick" ,seqmagick)
                                        ;    ("python-subprocess32" ,python2-subprocess32)
                                        ;    ("taxtastic" ,taxtastic)
       ("python-h5py" ,python2-h5py)
                                        ;    ("python-biom-format" ,python2-biom-format)
                                        ;    ("python-extern" ,extern)
       ("mafft" ,mafft))) 
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://geronimp.github.com/graftM")
    (synopsis
     "Identifies and classifies metagenomic marker gene reads")
    (description
     "GraftM is a pipeline used for identifying and classifying marker gene reads
from large metagenomic shotgun sequence datasets. It is able to find marker
genes using hidden Markov models or sequence similarity search, and classify
these reads by placement into phylogenetic trees")
    (license license:gpl3+)))

(define-public python2-biom-format
  (package
    (name "python2-biom-format")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/b/biom-format/biom-format-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05dssjhk06819xwpvm0yizw6gwxv5rx71h7hp4jdyrc79cwnbd1n"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python2-numpy)
       ("python-pyqi" ,python2-pyqi)
       ("python-scipy" ,python2-scipy)
       ("python-hypy" ,python2-h5py)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://www.biom-format.org")
    (synopsis
     "Biological Observation Matrix (BIOM) format")
    (description
     "Biological Observation Matrix (BIOM) format")
    (license license:bsd-3)))

(define-public python-pyqi
  (package
    (name "python-pyqi")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pyqi/pyqi-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0pyiym07yv5gdyncxd4qwf2jkb6rzqrlh6b2bq44ww3ray1i25wg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)
       ("python-tox" ,python-tox)))
    (home-page "http://bipy.github.io/pyqi")
    (synopsis "pyqi: expose your interface")
    (description "pyqi: expose your interface")
    (license license:bsd-3)))

(define-public python2-pyqi
  (package-with-python2 python-pyqi))

(define-public python-tox
  (package
    (name "python-tox")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/tox/tox-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1vqy9skwx9xs4az1d3mrfdlc38qphba0xbrj2z12ry7nl4ia0fm0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-virtualenv" ,python-virtualenv)
       ("python-py" ,python-py)
       ("python-pluggy" ,python-pluggy)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://tox.testrun.org/")
    (synopsis
     "virtualenv-based automation of test activities")
    (description
     "virtualenv-based automation of test activities")
    (license license:expat)))

(define-public python2-tox
  (package-with-python2 python-tox))

(define-public python-pluggy
  (package
    (name "python-pluggy")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pluggy/pluggy-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "18qfzfm40bgx672lkg8q9x5hdh76n7vax99aank7vh2nw21wg70m"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://pypi.python.org/pypi/pluggy") ;no obvious homepage
    (synopsis
     "plugin and hook calling mechanisms for python")
    (description
     "plugin and hook calling mechanisms for python")
    (license license:expat)))

(define-public python2-pluggy
  (package-with-python2 python-pluggy))

 (define-public pplacer ;; getting this to compile from source is just too hard
  (package
   (name "pplacer")
   (version "1.1")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://matsen.fredhutch.org/pplacer/builds/pplacer-v"
                   version "-Linux.tar.gz"))
             (sha256
              (base32
               "1v8id45bmsd2f99d9s2ki9x8flbas1qfvn4g4dypvgv94h2rqq2n"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; this is only a binary package
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append (assoc-ref outputs "out")
                                             "/bin")))
                     (mkdir-p bin)
                     (copy-file "pplacer"
                                (string-append bin "/pplacer"))
                     #t))))))
   (home-page "http://matsen.fredhutch.org/pplacer/")
   (synopsis "Places query sequences into phylogenetic trees")
   (description
    "Pplacer places query sequences on a fixed reference phylogenetic tree to
maximize phylogenetic likelihood or posterior probability according to a
reference alignment.  Pplacer is designed to be fast, to give useful information
about uncertainty, and to offer advanced visualization and downstream
analysis.")
   (license license:gpl2+)))

(define-public taxtastic ; fails to build because of the setup procedure,
                         ; presumably can be fixed.
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
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/fhcrc/taxtastic")
    (synopsis
     "Tools for taxonomic naming and annotation")
    (description
     "Tools for taxonomic naming and annotation")
    (license license:gpl3)))

(define-public python-xlrd ;sent to mailing list, in process there
  (package
    (name "python-xlrd")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/x/xlrd/xlrd-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wpa55nvidmm5m2qr622dsh3cj46akdk0h3zjgzschcmydck73cf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; current test in setup.py does not work as of 0.9.4,
         ;; so use nose to run tests instead for now.
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)))
    (home-page "http://www.python-excel.org/")
    (synopsis
     "Library for extracting data from Microsoft Excel (tm) files")
    (description
     "Extract data from Excel spreadsheets (.xls and .xlsx, versions 2.0
onwards) on any platform.  It is pure Python (2.6, 2.7, 3.2+), has support for
Excel dates and is Unicode-aware.")
    (license license:bsd-3)))

(define-public python2-xlrd
  (package-with-python2 python-xlrd))

