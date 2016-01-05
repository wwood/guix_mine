;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Ben Woodcroft <donttrustben@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ben packages rails-submitted)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby)

  #:use-module (gnu packages ruby)
  )


(define-public ruby-turn
  (package
    (name "ruby-turn")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "turn" version))
       (sha256
        (base32
         "1691rc2sq04cw8mxxh340k2j04ll90kwgcy8ddrp6rligmfrf8fw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests fail because turn changes its environment so can no longer
         ;; find test/unit.  Instead simply test if the executable runs
         ;; without issue.
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "bin/turn" "-h")))))))
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)
       ("ruby-minitest" ,ruby-minitest-4)))
    (synopsis "Alternate set of alternative runners for MiniTest")
    (description
     "TURN provides a set of alternative runners for MiniTest which are both
colorful and informative.  TURN displays each test on a separate line with
failures being displayed immediately instead of at the end of the tests.  Note
that TURN is no longer being maintained.")
    (home-page "http://rubygems.org/gems/turn")
    (license license:expat)))

(define-public ruby-mime-types-data
  (package
    (name "ruby-mime-types-data")
    (version "3.2015.1120")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types-data" version))
       (sha256
        (base32
         "04fzvy02w8d0rrsg8avncn7h58pvwdxj82aps54srb3sam2dkhic"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Registry for information about MIME media type definitions")
    (description
     "@code{mime-types-data} provides a registry for information about MIME
media type definitions.  It can be used with the Ruby mime-types library or
other software to determine defined filename extensions for MIME types, or to
use filename extensions to look up the likely MIME type definitions.")
    (home-page
     "https://github.com/mime-types/mime-types-data/")
    (license license:expat)))

(define-public ruby-mime-types
  (package
    (name "ruby-mime-types")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types" version))
       (sha256
        (base32
         "1snjc38a9vqvy8j41xld1i1byq9prbl955pbjw7dxqcfcirqlzra"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-mime-types-data" ,ruby-mime-types-data)))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-fivemat" ,ruby-fivemat)
       ("ruby-minitest-focus" ,ruby-minitest-focus)
       ("ruby-minitest-rg" ,ruby-minitest-rg)
       ("ruby-minitest-bonus-assertions" ,ruby-minitest-bonus-assertions)))
    (synopsis "Library and registry for MIME content type definitions")
    (description "The mime-types library provides a library and registry for
information about MIME content type definitions.  It can be used to determine
defined filename extensions for MIME types, or to use filename extensions to
look up the likely MIME type definitions.")
    (home-page "https://github.com/mime-types/ruby-mime-types")
    (license license:expat)))

(define-public ruby-fivemat
  (package
    (name "ruby-fivemat")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "fivemat" version))
       (sha256
        (base32
         "1gvw6g4yc96l2pcyvigahyfsjxpdjx21iiwzvf965zippchdh6gk"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Each test file given its own line of dots")
    (description
     "Fivemat is a MiniTest/RSpec/Cucumber formatter that gives each test file
its own line of dots during testing.  It aims to provide test output that is
neither too verbose nor too minimal.")
    (home-page "https://github.com/tpope/fivemat")
    (license license:expat)))

(define-public ruby-sqlite3
  (package
    (name "ruby-sqlite3")
    (version "1.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sqlite3" version))
       (sha256
        (base32
         "19r06wglnm6479ffj9dl0fa4p5j2wi6dj7k6k3d0rbx7036cv3ny"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-gemtest-file
           (lambda _
             ;; This file exists in the repository but is not distributed.
             (zero? (system* "touch" ".gemtest")))))))
    (inputs
     `(("sqlite" ,sqlite)))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-mini-portile" ,ruby-mini-portile)))
    (synopsis "Interface with SQLite3 databases")
    (description
     "This module allows Ruby programs to interface with the SQLite3 database
engine.")
    (home-page
     "https://github.com/sparklemotion/sqlite3-ruby")
    (license license:bsd-3)))

(define-public ruby-shoulda-context
  (package
    (name "ruby-shoulda-context")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda-context" version))
       (sha256
        (base32
         "06wv2ika5zrbxn0m3qxwk0zkbspxids3zmlq3xxays5qmvl1qb55"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Do not run tests to avoid circular dependence on rails.  Instead
             ;; just import the library to test.
             (zero? (system* "ruby" "-Ilib" "-r" "shoulda-context")))))))
    (synopsis "Test::Unit context framework extracted from Shoulda")
    (description
     "@code{shoulda-context} is the context framework extracted from Shoulda.
Instead of writing Ruby methods with lots_of_underscores, shoulda-context adds
context, setup, and should blocks combine to produce natural test method
names.")
    (home-page "https://github.com/thoughtbot/shoulda-context")
    (license license:expat)))

(define-public ruby-shoulda-matchers
  (package
    (name "ruby-shoulda-matchers")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda-matchers" version))
       (sha256
        (base32
         "1agabvb8i39mjrp3kb78nvhl41xk1i258hdwdlj0fm8nj9yzn1jb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-import
           (lambda _
             ;; A presumed bug reported upstream at
             ;; https://github.com/thoughtbot/shoulda-matchers/pull/871
             (substitute* (string-append  "lib/shoulda/matchers/active_model/"
                                          "validate_inclusion_of_matcher.rb")
               (("^require 'bigdecimal'")
                "require 'bigdecimal'; require 'date'"))))
         (replace 'check
           (lambda _
             ;; Do not run tests to avoid circular dependence on rails.  Instead
             ;; just import the library to test.
             (zero? (system* "ruby" "-Ilib" "-r" "shoulda-matchers")))))))
    (propagated-inputs
     `(("ruby-activesupport" ,ruby-activesupport)))
    (synopsis "Collection of testing matchers extracted from Shoulda")
    (description
     "Shoulda Matchers provides RSpec- and Minitest-compatible one-liners that
test common Rails functionality.  These tests would otherwise be much longer,
more complex, and error-prone.")
    (home-page "https://github.com/thoughtbot/shoulda-matchers")
    (license license:expat)))

(define-public ruby-shoulda-matchers-2
  (package
    (inherit ruby-shoulda-matchers)
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "shoulda-matchers" version))
              (sha256
               (base32
                "0d3ryqcsk1n9y35bx5wxnqbgw4m8b3c79isazdjnnbg8crdp72d0"))))))

(define-public ruby-shoulda
  (package
    (name "ruby-shoulda")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda" version))
       (sha256
        (base32
         "0csmf15a7mcinfq54lfa4arp0f4b2jmwva55m0p94hdf3pxnjymy"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Don't run tests to avoid circular dependence on rails.  Instead
             ;; just import the library to test.
             (zero? (system* "ruby" "-Ilib" "-r" "shoulda")))))))
    (propagated-inputs
     `(("ruby-shoulda-context" ,ruby-shoulda-context)
       ("ruby-shoulda-matchers" ,ruby-shoulda-matchers-2)))
    (synopsis "Context framework and matchers for testing")
    (description
     "@code{shoulda} is a meta-package combining @code{shoulda-context} and
@code{shoulda-matchers} providing tools for writing tests.")
    (home-page
     "https://github.com/thoughtbot/shoulda")
    (license license:expat)))

(define-public ruby-unf
  (package
    (name "ruby-unf")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unf" version))
       (sha256
        (base32
         "0bh2cf73i2ffh4fcpdn9ir4mhq8zi50ik0zqa1braahzadx536a9"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-dependency-to-bundler
           (lambda _
             ;; test-unit is required but not provided by the bundler
             ;; environment.  This is fixed in the upstream repository but fix
             ;; has not been released.
             (substitute* "Gemfile"
               (("^gemspec") "gem 'test-unit'\ngemspec"))
             #t)))))
    (propagated-inputs
     `(("ruby-unf-ext" ,ruby-unf-ext)))
    (native-inputs
     `(("ruby-shoulda" ,ruby-shoulda)
       ("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Unicode Normalization Form support to Ruby and JRuby")
    (description
     "@code{ruby-unf} is a wrapper library to bring Unicode Normalization Form
support to both Ruby and JRuby.  It uses @code{unf_ext} on CRuby and
@code{java.text.Normalizer} on JRuby.")
    (home-page "https://github.com/knu/ruby-unf")
    (license license:bsd-2)))

(define-public ruby-domain-name
  (package
    (name "ruby-domain-name")
    (version "0.5.25")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "domain_name" version))
       (sha256
        (base32
         "16qvfrmcwlzz073aas55mpw2nhyhjcn96s524w0g1wlml242hjav"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-versions
           (lambda _
             ;; Fix NameError that appears to already be fixed upstream.
             (substitute* "Rakefile"
               (("DomainName::VERSION")
                "Bundler::GemHelper.gemspec.version"))
             ;; Loosen unnecessarily strict test-unit version specification.
             (substitute* "domain_name.gemspec"
               (("<test-unit>, \\[\\\"~> 2.5.5") "<test-unit>, [\">0"))
             #t)))))
    (propagated-inputs
     `(("ruby-unf" ,ruby-unf)))
    (native-inputs
     `(("ruby-shoulda" ,ruby-shoulda)
       ("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Domain name manipulation library")
    (description
     "@code{domain_name} is a Domain name manipulation library.  It parses a
domain name ready for extracting the registered domain and TLD (Top Level
Domain).  It can also be used for cookie domain validation based on the Public
Suffix List.")
    (home-page
     "https://github.com/knu/ruby-domain_name")
    (license license:bsd-2)))

(define-public ruby-http-cookie
  (package
    (name "ruby-http-cookie")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "http-cookie" version))
       (sha256
        (base32
         "0cz2fdkngs3jc5w32a6xcl511hy03a7zdiy988jk1sf3bf5v3hdw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-dependency-to-bundler
           (lambda _
             ;; Fix NameError
             (substitute* "Rakefile"
               (("HTTP::Cookie::VERSION")
                "Bundler::GemHelper.gemspec.version"))
             #t)))))
    (propagated-inputs
     `(("ruby-domain-name" ,ruby-domain-name)))
    (native-inputs
     `(("rubysimplecov" ,ruby-simplecov)
       ("bundler" ,bundler)
       ("ruby-sqlite3" ,ruby-sqlite3)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Handle HTTP Cookies based on RFC 6265")
    (description
     "@code{HTTP::Cookie} is a Ruby library to handle HTTP Cookies based on
RFC 6265.  It has been designed with security, standards compliance and
compatibility in mind, to behave just the same as today's major web browsers.
It has built-in support for the legacy @code{cookies.txt} and
@code{cookies.sqlite} formats of Mozilla Firefox.")
    (home-page
     "https://github.com/sparklemotion/http-cookie")
    (license license:expat)))

(define-public ruby-ansi
  (package
    (name "ruby-ansi")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       ;; Fetch from GitHub as the gem does not contain testing code.
       (uri (string-append "https://github.com/rubyworks/ansi/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zdip30hivyipi8hndhb457bhiz033awd00bgrsk5axjrwp6zhly"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Disable testing to break the cycle ansi, ae, ansi, as well as the
         ;; cycle ansi, qed, ansi.  Instead simply test that the library can
         ;; be require'd.
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "ansi")))))))
    (synopsis "ANSI escape code related libraries")
    (description
     "This package is a collection of ANSI escape code related libraries
enabling ANSI colorization and stylization of console output.  Included in the
library are the @code{Code} module, which defines ANSI codes as constants and
methods, a @code{Mixin} module for including color methods, a @code{Logger}, a
@code{ProgressBar}, and a @code{String} subclass.  The library also includes a
@code{Terminal} module which provides information about the current output
device.")
    (home-page "http://rubyworks.github.io/ansi")
    (license license:bsd-2)))

