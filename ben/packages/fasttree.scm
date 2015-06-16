(define-module (ben packages fasttree)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base))

(define-public fasttree
  (package
   (name "fasttree")
   (version "2.1.8")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "http://www.microbesonline.org/fasttree/FastTree-"
		  version
		  ".c"
		  ))
	    (sha256
	     (base32
	      "0dzqc9vr9iiiw21y159xfjl2z90vw0y7r4x6456pcaxiy5hd2wmi"))))
   (build-system trivial-build-system)
   (arguments
    '(#:modules ((guix build utils))
		#:builder
		(begin ;standard-packages
		  (use-modules (guix build utils) (system base compile))
		  (let ((source (assoc-ref %build-inputs "source"))
			(coreutils (assoc-ref %build-inputs "coreutils"))
			(gcc (assoc-ref %build-inputs "gcc"))
			(glibc (assoc-ref %build-inputs "glibc"))
			(binutils (assoc-ref %build-inputs "binutils"))
			(out    (assoc-ref %outputs "out")))
					;gcc -DOPENMP -fopenmp -O3 -finline-functions -funroll-loops -Wall -o FastTreeMP FastTree.c -lm
		    (setenv "PATH" (string-append coreutils "/bin:" gcc "/bin:" binutils "/bin"))
		    (setenv "LIBRARY_PATH" (string-append glibc "/lib"))
		    (setenv "LD_LIBRARY_PATH" (string-append glibc "/lib"))
		    (system* "echo" "seriously")
		    (let ((bin (string-append out "/bin")))
		      (mkdir-p bin)
		      (system* "gcc"
			       "-DOPENMP"
			       "-fopenmp"
			       "-O3"
			       "-finline-functions"
			       "-funroll-loops"
			       "-Wall"
			       "-o"
			       (string-append bin "/FastTreeMP")
			       source
			       "-lm"))))))
   (native-inputs
    `(("coreutils" ,coreutils) ;temporary
      ("gcc", gcc-4.8)
      ("binutils" ,binutils)
      ("glibc" ,glibc)
      ;("bash" ,bash)
      ))
   (home-page "http://www.microbesonline.org/fasttree")
   (synopsis "FastTree infers approximately-maximum-likelihood
phylogenetic trees from alignments of nucleotide or protein sequences")
   (description
    "FastTree can handle alignments with up to a million of sequences in
a reasonable amount of time and memory. For large alignments, FastTree is
100-1,000 times faster than PhyML 3.0 or RAxML 7.")
   (license license:gpl2+)))
