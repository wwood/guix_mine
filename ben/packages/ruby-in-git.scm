;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;;
;;; This file contains patches that are in my personal git tree, but have not
;;; yet been incorporated upstream.
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

(define-module (ben packages ruby-in-git)
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
  #:use-module (gnu packages rails))


;; RSpec is the dominant testing library for Ruby projects.  Even RSpec's
;; dependencies use RSpec for their test suites!  To avoid these circular
;; dependencies, we disable tests for all of the RSpec-related packages.
(define ruby-rspec-support
  (package
    (name "ruby-rspec-support")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-support" version))
              (sha256
               (base32
                "10vf3k3d472y573mag2kzfsfrf6rv355s13kadnpryk8d36yq5r0"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "RSpec support library")
    (description "Support utilities for RSpec gems.")
    (home-page "https://github.com/rspec/rspec-support")
    (license license:expat)))

(define-public ruby-rspec-core
  (package
    (name "ruby-rspec-core")
    (version "3.5.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "0brfq51fwkkh5g6vw7smky5fvip46pryi243jmin0nzn7iwh9j5g"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)))
    (synopsis "RSpec core library")
    (description "Rspec-core provides the RSpec test runner and example
groups.")
    (home-page "https://github.com/rspec/rspec-core")
    (license license:expat)))

(define-public ruby-rspec-expectations
  (package
    (name "ruby-rspec-expectations")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "0bbqfrb1x8gmwf8x2xhhwvvlhwbbafq4isbvlibxi6jk602f09gs"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)
       ("ruby-diff-lcs" ,ruby-diff-lcs)))
    (synopsis "RSpec expectations library")
    (description "Rspec-expectations provides a simple API to express expected
outcomes of a code example.")
    (home-page "https://github.com/rspec/rspec-expectations")
    (license license:expat)))

(define-public ruby-rspec-mocks
  (package
    (name "ruby-rspec-mocks")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "0nl3ksivh9wwrjjd47z5dggrwx40v6gpb3a0gzbp1gs06a5dmk24"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)
       ("ruby-diff-lcs" ,ruby-diff-lcs)))
    (synopsis "RSpec stubbing and mocking library")
    (description "Rspec-mocks provides RSpec's \"test double\" framework, with
support for stubbing and mocking.")
    (home-page "https://github.com/rspec/rspec-mocks")
    (license license:expat)))

(define-public ruby-rspec
  (package
    (name "ruby-rspec")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "16g3mmih999f0b6vcz2c3qsc7ks5zy4lj1rzjh8hf6wk531nvc6s"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-mocks" ,ruby-rspec-mocks)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)))
    (synopsis "Behavior-driven development framework for Ruby")
    (description "RSpec is a behavior-driven development (BDD) framework for
Ruby.  This meta-package includes the RSpec test runner, along with the
expectations and mocks frameworks.")
    (home-page "http://rspec.info/")
    (license license:expat)))


(define-public ruby-spring
  (package
    (name "ruby-spring")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/rails/spring/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dd58y0cpsm2izj74yscn0ybfygmgcbbfdw1891g7cq41aai4b35"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "test:unit"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-bump
           (lambda _
             (substitute* "spring.gemspec"
               (("gem.add_development_dependency 'bump'") "")
               (("gem.add_development_dependency 'activesupport'.*")
                "gem.add_development_dependency 'activesupport'\n"))
             (substitute* "Rakefile"
               (("require \\\"bump/tasks\\\"") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-activesupport" ,ruby-activesupport)))
    (synopsis "Ruby on Rails application preloader")
    (description
     "Spring is a Ruby on Rails application preloader.  It speeds up
development by keeping your application running in the background so the
application does need to boot it every time you run a test, rake task or
migration.")
    (home-page "https://github.com/rails/spring")
    (license license:expat)))


(define-public ruby-rack
  (package
    (name "ruby-rack")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       ;; Download from GitHub so that the patch can be applied.
       (uri (string-append
             "https://github.com/rack/rack/archive/"
             version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00k62v8lpyjzghkn0h0awrnqj1jmlcs2wp57py27m43y65v89cp3"))
       ;; Ignore test which fails inside the build environment but works
       ;; outside.
       (patches (search-patches "ruby-rack-ignore-failing-test.patch"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             ;; A few of the tests use the length of a file on disk for
             ;; Content-Length and Content-Range headers.  However, this file
             ;; has a shebang in it which an earlier phase patches, growing
             ;; the file size from 193 to 239 bytes when the store prefix is
             ;; "/gnu/store".
             (let ((size-diff (- (string-length (which "ruby"))
                                 (string-length "/usr/bin/env ruby"))))
               (substitute* '("test/spec_file.rb")
                 (("193")
                  (number->string (+ 193 size-diff)))
                 (("bytes(.)22-33" all delimiter)
                  (string-append "bytes"
                                 delimiter
                                 (number->string (+ 22 size-diff))
                                 "-"
                                 (number->string (+ 33 size-diff))))))
             #t)))))
    (native-inputs
     `(("ruby-minitest" ,ruby-minitest)
       ("ruby-minitest-sprint" ,ruby-minitest-sprint)
       ("which" ,which)))
    (propagated-inputs
     `(("ruby-concurrent" ,ruby-concurrent)))
    (synopsis "Unified web application interface for Ruby")
    (description "Rack provides a minimal, modular and adaptable interface for
developing web applications in Ruby.  By wrapping HTTP requests and responses,
it unifies the API for web servers, web frameworks, and software in between
into a single method call.")
    (home-page "http://rack.github.io/")
    (license license:expat)))

(define-public ruby-rack-test
  (package
    (name "ruby-rack-test")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rack-test" version))
       (sha256
        (base32
         "0h6x5jq24makgv2fq5qqgjlrk74dxfy62jif9blk43llw8ib2q7z"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable tests because of circular dependencies: requires sinatra,
     ;; which requires rack-protection, which requires rack-test.  Instead
     ;; simply require the library.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "rack/test")))))))
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (synopsis "Testing API for Rack applications")
    (description
     "Rack::Test is a small, simple testing API for Rack applications.  It can
be used on its own or as a reusable starting point for Web frameworks and
testing libraries to build on.")
    (home-page "http://github.com/brynary/rack-test")
    (license license:expat)))


(define-public ruby-activesupport
  (package
    (name "ruby-activesupport")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activesupport" version))
       (sha256
        (base32
         "0k7zhnz0aw1ym8phs10r85f91ja45vsd058fm9v0h2k0igw12cpf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; There is no tests, instead attempt to load the library.
             (zero? (system* "ruby" "-Ilib" "-r" "active_support")))))))
    (propagated-inputs
     `(("ruby-concurrent" ,ruby-concurrent)
       ("ruby-i18n" ,ruby-i18n)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-tzinfo" ,ruby-tzinfo)
       ("ruby-tzinfo-data" ,ruby-tzinfo-data)))
    (synopsis "Ruby on Rails utility library")
    (description "ActiveSupport is a toolkit of support libraries and Ruby
core extensions extracted from the Rails framework.  It includes support for
multibyte strings, internationalization, time zones, and testing.")
    (home-page "http://www.rubyonrails.org")
    (license license:expat)))

(define-public ruby-concurrent
  (package
    (name "ruby-concurrent")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       ;; Download from GitHub because the rubygems version does not contain
       ;; Rakefile.
       (uri (string-append
             "https://github.com/ruby-concurrency/concurrent-ruby/archive/v"
             version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1x3g2admp14ykwfxidsicqbhlfsnxh9wyc806np4i15hws4if1d8"))
       ;; Exclude failing test reported at
       ;; https://github.com/ruby-concurrency/concurrent-ruby/issues/534
       (patches (search-patches "ruby-concurrent-ignore-broken-test.patch"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'replace-git-ls-files 'remove-extra-gemspecs
           (lambda _
             ;; Delete extra gemspec files so 'first-gemspec' chooses the
             ;; correct one.
             (delete-file "concurrent-ruby-edge.gemspec")
             (delete-file "concurrent-ruby-ext.gemspec")
             #t))
         (add-before 'build 'replace-git-ls-files2
           (lambda _
             (substitute* "support/file_map.rb"
               (("git ls-files") "find * |sort"))
             #t))
         (add-before 'check 'rake-compile
           ;; Fix the test error described at
           ;; https://github.com/ruby-concurrency/concurrent-ruby/pull/408
           (lambda _ (zero? (system* "rake" "compile"))))
         (add-before 'check 'remove-timecop-dependency
           ;; Remove timecop-dependent tests as having timecop as a depedency
           ;; causes circular depedencies.
           (lambda _
             (delete-file "spec/concurrent/executor/timer_set_spec.rb")
             (delete-file "spec/concurrent/scheduled_task_spec.rb")
             #t)))))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Concurrency tools for Ruby")
    (description
     "This library provides modern concurrency tools including agents,
futures, promises, thread pools, actors, supervisors, and more.  It is
inspired by Erlang, Clojure, Go, JavaScript, actors and classic concurrency
patterns.")
    (home-page "http://www.concurrent-ruby.com")
    (license license:expat)))


(define-public ruby-ruby-engine
  (package
    (name "ruby-ruby-engine")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_engine" version))
       (sha256
        (base32
         "1d0sd4q50zkcqhr395wj1wpn2ql52r0fpwhzjfvi1bljml7k546v"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'clean-up
           (lambda _
             (delete-file "Gemfile.lock")
             (substitute* "ruby_engine.gemspec"
               ;; Remove unnecessary imports that would entail further
               ;; dependencies.
               ((".*<rdoc.*") "")
               ((".*<rubygems-tasks.*") "")
               ;; Remove extraneous .gem file
               (("\\\"pkg/ruby_engine-1.0.0.gem\\\",") ""))
             (substitute* "Rakefile"
               (("require 'rubygems/tasks'") "")
               (("Gem::Tasks.new") ""))
             ;; Remove extraneous .gem file that otherwise gets installed.
             (delete-file "pkg/ruby_engine-1.0.0.gem")
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis "Simplifies checking for Ruby implementation")
    (description
     "@code{ruby_engine} provides an RubyEngine class that can be used to check
which implementation of Ruby is in use.  It can provide the interpreter name and
provides query methods such as @{RubyEngine.mri?}.")
    (home-page
     "https://github.com/janlelis/ruby_engine")
    (license license:expat)))

(define-public ruby-erubis
  (package
    (name "ruby-erubis")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "erubis" version))
       (sha256
        (base32
         "1fj827xqjs91yqsydf0zmfyw9p4l2jz5yikg3mppz6d7fi8kyrb3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; tests do not run properly with Ruby 2.0
    (synopsis "Implementation of embedded Ruby (eRuby)")
    (description
     "Erubis is a fast implementation of embedded Ruby (eRuby) with several
features such as multi-language support, auto escaping, auto trimming spaces
around @code{<% %>}, a changeable embedded pattern, and Ruby on Rails
support.")
    (home-page "http://www.kuwata-lab.com/erubis/")
    (license license:expat)))

(define-public ruby-sporkmonger-rack-mount
  ;; Testing the addressable gem requires a newer commit than that released, so
  ;; use an up to date version.
  (let ((revision "1")
        (commit "076aa2c47d9a4c081f1e9bcb56a826a9e72bd5c3"))
    (package
      (name "ruby-sporkmonger-rack-mount")
      (version (string-append "0.8.3." revision "." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sporkmonger/rack-mount.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1scx273g3xd93424x9lxc4zyvcp2niknbw5mkz6wkivpf7xsyxdq"))))
      (build-system ruby-build-system)
      (arguments
       ;; Tests currently fail so disable them.
       ;; https://github.com/sporkmonger/rack-mount/pull/1
       `(#:tests? #f))
      (propagated-inputs `(("ruby-rack" ,ruby-rack)))
      (synopsis "Stackable dynamic tree based Rack router")
      (description
       "@code{Rack::Mount} supports Rack's @code{X-Cascade} convention to
continue trying routes if the response returns pass.  This allows multiple
routes to be nested or stacked on top of each other.")
      (home-page "https://github.com/sporkmonger/rack-mount")
      (license license:expat))))


(define-public ruby-creole
  (package
    (name "ruby-creole")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "creole" version))
       (sha256
        (base32
         "00rcscz16idp6dx0dk5yi5i0fz593i3r6anbn5bg2q07v3i025wm"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-bacon" ,ruby-bacon)))
    (synopsis "Creole markup language converter")
    (description
     "Creole is a lightweight markup language and this library for converting
creole to @code{HTML}.")
    (home-page "http://github.com/minad/creole")
    (license license:ruby)))


(define-public ruby-rack-protection
  (package
    (name "ruby-rack-protection")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rack-protection" version))
       (sha256
        (base32
         "0cvb21zz7p9wy23wdav63z5qzfn4nialik22yqp6gihkgfqqrh5r"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)
       ("ruby-rack-test" ,ruby-rack-test)))
    (synopsis "Rack middleware that protects against typical web attacks")
    (description "This library is rack middleware that can be used to protect
against typical web attacks.  It can protect all Rack apps, including Rails.
For instance, it protects against cross site request forgery, cross site
scripting, clickjacking, directory traversal, session hijacking and IP
spoofing.")
    (home-page
     "http://github.com/rkh/rack-protection")
    (license license:expat)))


(define-public ruby-asciidoctor
  (package
  (name "ruby-asciidoctor")
  (version "1.5.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "asciidoctor" version))
      (sha256
        (base32
          "0q9yhan2mkk1lh15zcfd9g2fn6faix9yrf5skg23dp1y77jv7vm0"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "test:all"
     #:phases
     (modify-phases %standard-phases
       (add-before 'check 'remove-circular-tests
         (lambda _
           ;; Remove tests that require circular dependencies to load or pass.
           (delete-file "test/invoker_test.rb")
           (delete-file "test/converter_test.rb")
           (delete-file "test/options_test.rb")
           #t)))))
  (native-inputs
   `(("ruby-minitest" ,ruby-minitest)
     ("ruby-nokogiri" ,ruby-nokogiri)
     ("ruby-asciimath" ,ruby-asciimath)
     ("ruby-coderay" ,ruby-coderay)))
  (synopsis "Converter from AsciiDoc content to other formats")
  (description
    "Asciidoctor is a text processor and publishing toolchain for converting
AsciiDoc content to HTML5, DocBook 5 (or 4.5) and other formats.")
  (home-page "http://asciidoctor.org")
  (license license:expat)))


(define-public ruby-asciimath
  (package
    (name "ruby-asciimath")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciimath" version))
       (sha256
        (base32
         "0dzwkjn0q4f1vd5v8wr8azzh4b94yrs7ycq7s0r1g9ijdxdlii6k"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* _ (zero? (system* "rspec" "test/parser_spec.rb")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "AsciiMath parsing and conversion library")
    (description
     "A pure Ruby AsciiMath parsing and conversion library.  AsciiMath is an
easy-to-write markup language for mathematics.")
    (home-page "https://github.com/pepijnve/asciimath")
    (license license:expat)))

(define-public ruby-contest
  (package
    (name "ruby-contest")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contest" version))
       (sha256
        (base32
         "1p9f2292b7b0fbrcjswvj9v01z7ig5ig52328wyqcabgb553qsdf"))))
    (build-system ruby-build-system)
    (synopsis "Write declarative tests using nested contexts")
    (description
     "Contest allows writing declarative @code{Test::Unit} tests using nested
contexts without performance penalties.")
    (home-page
     "http://github.com/citrusbyte/contest")
    (license license:expat)))
