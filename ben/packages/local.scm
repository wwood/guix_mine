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
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
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

(define-public diamond
  (package
    (name "diamond")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/bbuchfink/diamond/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0hfkcfv9f76h5brbyw9fyvmc0l9cmbsxrcdqk0fa9xv82zj47p15"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
		#:phases
		(alist-cons-after
		 'unpack 'enter-source-dir
		 (lambda _ (chdir "src"))
		 (alist-replace
		  'install
		  (lambda* (#:key outputs #:allow-other-keys)
			   (let ((bin (string-append
				       (assoc-ref outputs "out") "/bin")))
			     (mkdir-p bin)
			     (copy-file "../bin/diamond" (string-append bin "/diamond"))))
		  (alist-delete 'configure %standard-phases)))))
    (native-inputs
     `(("bc" ,bc)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "https://github.com/bbuchfink/diamond")
    (synopsis "Accelerated BLAST compatible local sequence aligner")
    (description
     "DIAMOND is a BLAST-compatible local aligner for mapping protein and 
translated DNA query sequences against a protein reference database (BLASTP 
and BLASTX alignment mode). The speedup over BLAST is up to 20,000 on short 
reads at a typical sensitivity of 90-99% relative to BLAST depending on the 
data and settings.")
    (license (license:non-copyleft (string-append "https://github.com/bbuchfink"
						  "/diamond/blob/v"
						  version
						  "/src/COPYING")))))

(define-public bamm
  (package
   (name "bamm")
   (version "1.4.2")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "file:///tmp/bamm.tar.gz"))
		  ;"https://github.com/Ecogenomics/BamM/archive/v"
		  ;version ".tar.gz"))
	    (sha256
	     (base32
	      "1vdnc1r8z151h2r1bryl8ylqxggyah6hi84cvyfqhdkqsabjvcx4"))))
   (build-system python-build-system)
   (arguments `(#:python ,python-2)) ; no Python 3 support
   (inputs
    `(("bwa" ,bwa)
      ("samtools-1" ,samtools-0.1)
      ("python-numpy" ,python-numpy)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)
      ("htslib" ,htslib)
      ("python-nose" ,python2-nose)
      ("python-setuptools" ,python2-setuptools)))
   (home-page "http://ecogenomics.github.io/BamM/")
   (synopsis "Metagenomics-focused BAM file manipulation")
   (description
    "BamM is a c library, wrapped in python, that parses BAM files. The code
is intended to provide a faster, more stable interface to parsing BAM files than
PySam, but doesn't implement all/any of PySam's features. Do you want all the
links that join two contigs in a BAM? Do you need to get coverage? Would you
like to just work out the insert size and orientation of some mapped reads? Then
BamM is for you!")
   (license license:lgpl3+)))
