(define-module (ben packages meld)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages image)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages geeqie)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages qt)  ; for libxkbcommon
  #:use-module (gnu packages compression)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages video)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages readline)
  #:use-module (srfi srfi-1))

(define-public meld
  (package
    (name "meld")
    (version "3.14.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0wqq4d3084qqnnny5mi130qbvcsh1vxssrhg93i7lzivrm8hjg2a"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "bin/meld" "-h")))))
       ))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
                                        ;("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("glib" ,glib) ; for glib-compile-schemas, etc.
        ))
    (inputs
     `(("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview) ; needed at runtime?
       ))
    (propagated-inputs
     `(("python-pygobject" ,python2-pygobject)))
    (synopsis "")
    (home-page "http://meldmerge.org/")
    (description
     "")
    (license license:gpl2+)))
