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
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)

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
                "16aq0w3dmbd0853j32xk9jin4vb6v6fgakfyvrsmsjizzbn3fpfl"))))
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

(define-public bamm
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

(define-public fxtract
  (let ((commit "a90a6a84f"))
    (package
      (name "fxtract")
      (version "1.2-dev")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ctSkennerton/fxtract.git")
                      (commit commit)
                      (recursive? #t)))
                (sha256
                 (base32
                  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa1yck5xv0arrzg83q6v90430"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
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
    (license license:gpl3+))))

(define-public seqtk
  (let ((commit "a90a6a84f"))
    (package
      (name "seqtk")
      (version "20150618.4feb6e8144") ;;TODO: is this the correct format for bleeding edge releases?
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lh3/seqtk.git")
                      (commit commit)))
                (sha256
                 (base32
                  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa1yck5xv0arrzg83q6v90430"))))
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
				  "/bin")))
			(mkdir-p bin)
			(copy-file "seqtk" bin)
			(copy-file "trimadap" bin)))))))
    (home-page "https://github.com/lh3/seqtk")
    (synopsis "Toolkit for processing sequences in FASTA/Q formats")
    (description
     "Seqtk is a fast and lightweight tool for processing sequences in
the FASTA or FASTQ format.  It seamlessly parses both FASTA and FASTQ
files which can also be optionally compressed by gzip.")
    (license (license:non-copyleft "file://src/LICENSE"
				   "See src/LICENSE in the distribution.")))))

(define-public jellyfish
  (package
    (name "jellyfish")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gmarcais/Jellyfish/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16aq0w3dmbd0853j32xk9jin4vb6v6fgakfyvrsmsjizzbn3fpfl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (before 'configure
	       (lambda* _
		 (zero? (system* "autoreconf" "-i"))))))
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
    


(define-public kronatools
  (package
    (name "kronatools")
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
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
	 (delete 'configure)

	 ;;TODO: remove the underscore files in e.g. the scripts directory
	 (delete 'build)
	 ;;	 (replace 'build 
	 ;; (lambda* _
	 ;;   (define (leaf name stat result)
	 ;;     (pretty-print name)(+0))
	 ;;   (file-system-fold #t leaf _ _ _ _ 0 ".")
	 ;;   #t))
	 (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (string-append (assoc-ref outputs "out"))))
		      (mkdir-p out)
		      (copy-recursively "lib" (string-append out "/lib"))
		      (copy-recursively "scripts" (string-append out "/scripts"))
		      (copy-recursively "data" (string-append out "/data"))
		      (copy-recursively "img" (string-append out "/img"))
		      (copy-recursively "src" (string-append out "/src"))
		      (copy-recursively "taxonomy" (string-append out "/taxonomy"))
		      (copy-file "install.pl" (string-append out "/install.pl"))
		      (chdir out)
		      (system* "perl"
			       "install.pl"
			       "--prefix"
			       out)
		      (delete-file "install.pl")))))))
    (inputs
     `(("perl", perl)))
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

