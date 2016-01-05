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

(define-module (ben packages rails)
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
  #:use-module (ben packages rails-submitted)
  )


(define-public ruby-hoe-travis
(package
  (name "ruby-hoe-travis")
  (version "1.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "hoe-travis" version))
      (sha256
        (base32
          "1qg5sf9kpd0akj94glq1sj55ys5cxphgajxa0mllm40sb4dnzny2"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-hoe" ,ruby-hoe)
      ("ruby-travis" ,ruby-travis)))
  (synopsis
    "hoe-travis is a Hoe plugin that allows your gem to gain maximum benefit from
http://travis-ci.org.  The plugin contains a <code>.travis.yml</code>
generator and a pre-defined rake task which runs the tests and ensures your
manifest file is correct.

With hoe-travis it is easy to add additional checks.  Custom checks can be
easily verified locally by simply running a rake task instead of committing
and pushing a change, waiting for travis to run your tests, then trying a new
commit if you didn't fix the problem.")
  (description
    "hoe-travis is a Hoe plugin that allows your gem to gain maximum benefit from
http://travis-ci.org.  The plugin contains a <code>.travis.yml</code>
generator and a pre-defined rake task which runs the tests and ensures your
manifest file is correct.

With hoe-travis it is easy to add additional checks.  Custom checks can be
easily verified locally by simply running a rake task instead of committing
and pushing a change, waiting for travis to run your tests, then trying a new
commit if you didn't fix the problem.")
  (home-page
    "https://github.com/drbrain/hoe-travis")
  (license #f)))







(define-public ruby-addressable
  (package
    (name "ruby-addressable")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "addressable" version))
       (sha256
        (base32
         "0mpn7sbjl477h56gmxsjqb89r5s3w7vx5af994ssgc3iamvgzgvs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
          (lambda _
            (substitute* "Gemfile"
              (("git: 'https://github.com/sporkmonger/rack-mount.git',") ""))
            #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)
       ("ruby-sporkmonger-rack-mount" ,ruby-sporkmonger-rack-mount)
       ("ruby-rspec-its", ruby-rspec-its)))
    (synopsis "Uniform resource identifier (URI) reimplementation")
    (description
     "Addressable is a replacement for the URI implementation that is part of
Ruby's standard library.  It more closely conforms to the relevant RFCs and
adds support for IRIs and URI templates.")
    (home-page
     "https://github.com/sporkmonger/addressable")
    (license license:asl2.0)))

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

(define-public ruby-rspec-its
  (package
    (name "ruby-rspec-its")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-its" version))
       (sha256
        (base32
         "1pwphny5jawcm1hda3vs9pjv1cybaxy17dc1s75qd7drrvx697p3"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-aruba" ,ruby-aruba)))
    (synopsis "RSpec extension that provides the @code{its} method")
    (description
     "RSpec::Its provides the its method as a short-hand to specify the expected
value of an attribute.  For example, one can use @code{its(:size)\\{should
eq(1)\\}}.")
    (home-page "https://github.com/rspec/rspec-its")
    (license license:expat)))

(define-public ruby-cucumber
  (package
    (name "ruby-cucumber")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber" version))
       (sha256
        (base32
         "02crmbpf14sf0kaz7zpkmqxs59jxz50346ifprbwr0w3dcnfq3jx"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-builder" ,ruby-builder)
       ("ruby-cucumber-core" ,ruby-cucumber-core)
       ("ruby-diff-lcs" ,ruby-diff-lcs)
       ("ruby-gherkin3" ,ruby-gherkin3)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-multi-test" ,ruby-multi-test)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-aruba", ruby-aruba*))) ; use untested aruba version to avoid
                                     ; dependency cycle
    (synopsis
     "Behaviour Driven Development with elegance and joy")
    (description
     "Behaviour Driven Development with elegance and joy")
    (home-page "http://cukes.info")
    (license license:expat)))

(define ruby-cucumber*
  (package (inherit ruby-cucumber)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `())))

(define-public ruby-multi-test
  (package
    (name "ruby-multi-test")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_test" version))
       (sha256
        (base32
         "1sx356q81plr67hg16jfwz9hcqvnk03bd9n75pmdw8pfxjfy1yxd"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require different sets of specific gem versions to be available,
     ;; and there is no gemfile that specifies the newest versions of
     ;; dependencies to be tested.
     `(#:tests? #f))
    (synopsis
     "Wafter-thin gem to help control rogue test/unit/autorun requires")
    (description
     "Wafter-thin gem to help control rogue test/unit/autorun requires")
    (home-page "http://cukes.info")
    (license license:expat)))

(define-public ruby-multi-json
  (package
    (name "ruby-multi-json")
    (version "1.11.2")
    (source
     (origin
       (method url-fetch)
       ;; Tests are not distributed at rubygems.org so download from GitHub
       ;; instead.
       (uri (string-append "https://github.com/intridea/multi_json/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04vkfz88vj6ab3097n0xgm0335j753ik03zkq44grq6m36m94vk5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-signing-key-reference
          (lambda _
            (substitute* "multi_json.gemspec"
              ((".*spec.signing_key.*") ""))
            #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)
       ("ruby-json-pure" ,ruby-json-pure)
       ("ruby-oj" ,ruby-oj)
       ("ruby-yajl-ruby" ,ruby-yajl-ruby)))
    (synopsis
     "A common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (description
     "A common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (home-page
     "http://github.com/intridea/multi_json")
    (license expat)))

(define-public ruby-oj
  (package
    (name "ruby-oj")
    (version "2.14.3")
    (source
     (origin
       (method url-fetch)
       ;; Version on rubygems.org does not contain Rakefile, so download from
       ;; GitHub instead.
       (uri (string-append "https://github.com/ohler55/oj/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hyyrf21b9dhsq01gdv6wq7zhq9lzb3z9l52jv914qzmrhnbwx4f"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'compile
           (lambda _
             (zero? (system* "rake" "compile")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis
     "The fastest JSON parser and object serializer. ")
    (description
     "The fastest JSON parser and object serializer. ")
    (home-page "http://www.ohler.com/oj")
    (license expat)))

(define-public ruby-yajl-ruby
  (package
    (name "ruby-yajl-ruby")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yajl-ruby" version))
       (sha256
        (base32
         "0zvvb7i1bl98k3zkdrnx9vasq0rp2cyy5n7p9804dqs4fz9xh9vf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;; Remove broken test reported at
         ;; https://github.com/brianmario/yajl-ruby/issues/157
         (add-before 'check 'remove-failing-tests
           (lambda _
             (substitute* "spec/parsing/large_number_spec.rb"
               ((".*should eq\\(\\['', 0\\]\\).*") ""))
             #t)))))
     (native-inputs
      `(("ruby-rake-compiler" ,ruby-rake-compiler)
        ("ruby-rspec" ,ruby-rspec-2)))
     (synopsis
      "Ruby C bindings to the excellent Yajl JSON stream-based parser library.")
     (description
      "Ruby C bindings to the excellent Yajl JSON stream-based parser library.")
     (home-page
      "http://github.com/brianmario/yajl-ruby")
     (license license:expat)))

(define-public ruby-bzip2-ruby
  ;; Use git reference because gem is out of date.
  (let ((revision "1")
        (commit "e58f154624ff2d770e92a70e0d8fb1a9e6564720"))
    (package
      (name "ruby-bzip2-ruby")
      (version (string-append "0.2.7." revision "." commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/brianmario/bzip2-ruby.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "072jzd1fm6z0fnc166q7n4h8h4vrb9jhpcl7swh7d0qm4fx4cxvz"))))
      (build-system ruby-build-system)
      (arguments
       '(#:test-target "spec"
        ; #:gem-flags
        ; (list "--"
        ;       (string-append "--with-bz2-dir="
        ;                      (assoc-ref %build-inputs "bzip2")
        ;                      "/include/" ))
        #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-gemspec-and-source
             (lambda _
               (substitute* "bzip2-ruby.gemspec"
                 (("s.files = `git ls-files`") "s.files = `find *`")
                 (("s.test_files = `git ls-files spec`")
                  "s.files = `find spec`"))
               ;; Use part of the patch proposed at
               ;; https://github.com/brianmario/bzip2-ruby/pull/26
               (substitute* "ext/bzip2/writer.c"
                 (("RBASIC\\(res\\)->klass = rb_cString;")
                  "RBASIC_SET_CLASS(res, rb_cString);"))
               #t)))))
      (native-inputs
       `(("bundler" ,bundler)
         ("ruby-rspec" ,ruby-rspec-2)
         ("ruby-rake-compiler" ,ruby-rake-compiler)))
      (synopsis "Ruby C bindings to libbzip2.")
      (description "Ruby C bindings to libbzip2.")
      (home-page
       "http://github.com/brianmario/bzip2-ruby")
      (license #f))))

(define-public ruby-aruba
  (package
    (name "ruby-aruba")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aruba" version))
       (sha256
        (base32
         "0cris3g3bj8si5i2i51d1g5pcnwj1zqdh06g58yzc2p6ivv6081n"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-childprocess" ,ruby-childprocess)
       ("ruby-contracts" ,ruby-contracts)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-event-bus" ,ruby-event-bus)
       ("ruby-ffi" ,ruby-ffi)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-thor" ,ruby-thor)))
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis
     "Extension for popular TDD and BDD frameworks like \"Cucumber\", \"RSpec\" and \"Minitest\" to make testing commandline applications meaningful, easy and fun.")
    (description
     "Extension for popular TDD and BDD frameworks like \"Cucumber\", \"RSpec\" and \"Minitest\" to make testing commandline applications meaningful, easy and fun.")
    (home-page "http://github.com/cucumber/aruba")
    (license license:expat)))

;; A version of ruby-aruba without tests run so that circular dependencies can
;; be avoided.
(define ruby-aruba*
  (package (inherit ruby-aruba)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("ruby-childprocess" ,ruby-childprocess)
       ("ruby-contracts" ,ruby-contracts)
       ("ruby-cucumber" ,ruby-cucumber*) ; use untested cucumber to avoid
                                        ; dependency cycle
       ("ruby-event-bus" ,ruby-event-bus)
       ("ruby-ffi" ,ruby-ffi)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-thor" ,ruby-thor)))
    (native-inputs
     `())))

;; Seems to work except for 2 rspec errors already fixed upstream
;; https://github.com/egonSchiele/contracts.ruby/commit/c1f22bfc6b28125b55d42a33ca3e05f15e82d6f2.diff
;; might need to add that as a patch when adding to guix
(define-public ruby-contracts
  (package
    (name "ruby-contracts")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contracts" version))
       (sha256
        (base32
         "0xszv56p58q7da8agc4dsnw8x46gnh6ahbag5gdmvbxjgml03mdl"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; enable these when adding TO GUIX!!!!
       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
          (lambda _
            (substitute* "Rakefile"
              ((".*rubocop.*") "")
              ((".*RuboCop.*") ""))
            #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "This library provides contracts for Ruby. Contracts let you clearly express how your code behaves, and free you from writing tons of boilerplate, defensive code.")
    (description
     "This library provides contracts for Ruby. Contracts let you clearly express how your code behaves, and free you from writing tons of boilerplate, defensive code.")
    (home-page
     "http://github.com/egonSchiele/contracts.ruby")
    (license #f)))

(define-public ruby-event-bus
  (package
    (name "ruby-event-bus")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "event-bus" version))
       (sha256
        (base32
         "0bqcznr15q1346avpddnyd9144hqdww86yfpb4jayaj6lm0fqwyq"))))
    (build-system ruby-build-system)
    (arguments
     ;; disable testing to break the cycle with aruba.  Instead simply test that
     ;; the library can be require'd.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "event/bus")))))))
    (synopsis
     "This gem notifies subscribers about event")
    (description
     "This gem notifies subscribers about event")
    (home-page
     "https://github.com/cucumber/event-bus")
    (license license:expat)))

(define-public ruby-childprocess
  (package
    (name "ruby-childprocess")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "childprocess" version))
       (sha256
        (base32
         "1lv7axi1fhascm9njxh3lx1rbrnsm8wgvib0g7j26v4h1fcphqg0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f
;       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-dependency-and-patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "spec/spec_helper.rb"
               ;; cannot require coveralls otherwise there is a dependency cycle
               ;; childprocess, coveralls, rest-client, webmock, addressable,
               ;; rspec-its, aruba, childprocess.
               ((".*coveralls.*") "")
               ((".*Coveralls.*") "")
               ;; patch path to /bin/sh
               (("/bin/sh") (which "sh"))
               ;; testing
               (("; sleep") "; puts \"before_sleep\"; sleep; puts \"after_sleep\"")
               )
             (substitute* "lib/childprocess/unix/process.rb"
               (("send_signal 'TERM'") "puts 'terming'; send_signal 'TERM'")
               (("send_signal 'KILL'") "puts 'killing'; send_signal 'KILL'")
               (("return true if @exit_code")
                "puts \"exit_code: #{@exit_code}\"; p self; return true if @exit_code"))
             ;; sleep POLL_INTERVAL
             (substitute* "lib/childprocess/abstract_process.rb"
               (("sleep POLL_INTERVAL")
                "puts 'sleeping'; sleep POLL_INTERVAL; puts 'awake'")
               (("unless ok") "puts \"ok was #{ok} and exited? was #{exited?}\"; unless ok"))
             #t))
         ;(replace 'check
         ;  ;; for testing for ben only
         ;  (lambda _
         ;    (zero? (system* "rspec" "spec/childprocess_spec.rb" "-e" "kills
         ;    the full process tree"))))
         )))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "This gem aims at being a simple and reliable solution for controlling external programs running in the background on any Ruby / OS combination.")
    (description
     "This gem aims at being a simple and reliable solution for controlling external programs running in the background on any Ruby / OS combination.")
    (home-page
     "http://github.com/jarib/childprocess")
    (license license:expat)))


(define-public ruby-sinatra
  (package
  (name "ruby-sinatra")
  (version "1.4.6")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sinatra" version))
      (sha256
        (base32
          "1hhmwqc81ram7lfwwziv0z70jh92sj1m7h7s9fr0cn2xq8mmn8l7"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-rack" ,ruby-rack)
      ("ruby-rack-protection" ,ruby-rack-protection)
      ("ruby-tilt" ,ruby-tilt)))
  (native-inputs
   `(("ruby-rack-test" ,ruby-rack-test)))
  (synopsis
    "Sinatra is a DSL for quickly creating web applications in Ruby with minimal effort.")
  (description
    "Sinatra is a DSL for quickly creating web applications in Ruby with minimal effort.")
  (home-page "http://www.sinatrarb.com/")
  (license license:expat)))

(define-public ruby-tilt
  (package
    (name "ruby-tilt")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tilt" version))
       (sha256
        (base32
         "1qc1k2r6whnb006m10751dyz3168cq72vj8mgp5m2hpys8n6xp3k"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-asciidoctor" ,ruby-asciidoctor)
       ("ruby-builder" ,ruby-builder)
       ("ruby-coffee-script" ,ruby-coffee-script)
       ("ruby-contest" ,ruby-contest)
       ("ruby-creole" ,ruby-creole)
       ("ruby-erubis" ,ruby-erubis)
       ("ruby-haml" ,ruby-haml-3) ; do we really need haml <4 ?
       ("ruby-kramdown" ,ruby-kramdown)))
    (synopsis
     "Generic interface to multiple Ruby template engines")
    (description
     "Generic interface to multiple Ruby template engines")
    (home-page "http://github.com/rtomayko/tilt/")
    (license license:expat)))

(define-public ruby-radius
  (package
    (name "ruby-radius")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "radius" version))
       (sha256
        (base32
         "0n0clzgvxpjm2gjlpz98x6gkw5hb84bmd435a1yaqs3m0k896v5s"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-simplecov" ,ruby-simplecov)
       ("ruby-coveralls" ,ruby-coveralls)))
    (synopsis
     "Radius is a powerful tag-based template language for Ruby inspired by the template languages used in MovableType and TextPattern. It uses tags similar to XML, but can be used to generate any form of plain text (HTML, e-mail, etc...).")
    (description
     "Radius is a powerful tag-based template language for Ruby inspired by the template languages used in MovableType and TextPattern. It uses tags similar to XML, but can be used to generate any form of plain text (HTML, e-mail, etc...).")
    (home-page "http://github.com/jlong/radius")
    (license #f)))

(define-public ruby-coveralls
  (package
    (name "ruby-coveralls")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "coveralls" version))
       (sha256
        (base32
         "1y6gzahhaymgcxcgm7y16sgbiafsb7i2flhy1sq4x1jizk8bih5s"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-json" ,ruby-json)
       ("ruby-rest-client" ,ruby-rest-client)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-term-ansicolor" ,ruby-term-ansicolor)
       ("ruby-thor" ,ruby-thor)
       ("ruby-tins" ,ruby-tins)))
    (synopsis
     "A Ruby implementation of the Coveralls API.")
    (description
     "A Ruby implementation of the Coveralls API.")
    (home-page "https://coveralls.io")
    (license license:expat)))

(define-public ruby-rest-client
  (package
    (name "ruby-rest-client")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rest-client" version))
       (sha256
        (base32
         "1m8z0c4yf6w47iqz6j2p7x1ip4qnnzvhdph9d5fgx081cvjly3p7"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-http-cookie" ,ruby-http-cookie)
       ("ruby-mime-types" ,ruby-mime-types)
       ("ruby-netrc" ,ruby-netrc)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-webmock", ruby-webmock)))
    (synopsis
     "A simple HTTP and REST client for Ruby, inspired by the Sinatra microframework style of specifying actions: get, put, post, delete.")
    (description
     "A simple HTTP and REST client for Ruby, inspired by the Sinatra microframework style of specifying actions: get, put, post, delete.")
    (home-page
     "https://github.com/rest-client/rest-client")
    (license license:expat)))

(define-public ruby-webmock
  (package
    (name "ruby-webmock")
    (version "1.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webmock" version))
       (sha256
        (base32
         "0la47vzbikhvnx8qcj8jli87agzzffwh11ggm7rpq43iz2rwp0sl"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-crack" ,ruby-crack)
       ));("ruby-hashdiff" ,ruby-hashdiff)))
    (synopsis
     "WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.")
    (description
     "WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.")
    (home-page "http://github.com/bblimke/webmock")
    (license license:expat)))

(define-public ruby-crack
  (package
    (name "ruby-crack")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "crack" version))
       (sha256
        (base32
         "0abb0fvgw00akyik1zxnq7yv391va148151qxdghnzngv66bl62k"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-safe-yaml" ,ruby-safe-yaml)))
    (synopsis
     "Really simple JSON and XML parsing, ripped from Merb and Rails.")
    (description
     "Really simple JSON and XML parsing, ripped from Merb and Rails.")
    (home-page "http://github.com/jnunemaker/crack")
    (license license:expat)))

(define-public ruby-safe-yaml
  (package
    (name "ruby-safe-yaml")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "safe_yaml" version))
       (sha256
        (base32
         "1hly915584hyi9q9vgd968x2nsi5yag9jyf5kq60lwzi5scr7094"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-hashie" ,ruby-hashie)))
    (synopsis "Parse YAML safely")
    (description "Parse YAML safely")
    (home-page "https://github.com/dtao/safe_yaml")
    (license license:expat)))

(define-public ruby-hashie
  (package
    (name "ruby-hashie")
    (version "3.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hashie" version))
       (sha256
        (base32
         "1iv5hd0zcryprx9lbcm615r3afc0d6rhc27clywmhhgpx68k8899"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-pending-for" ,ruby-rspec-pending-for)))
    (synopsis
     "Hashie is a collection of classes and mixins that make hashes more powerful.")
    (description
     "Hashie is a collection of classes and mixins that make hashes more powerful.")
    (home-page "https://github.com/intridea/hashie")
    (license license:expat)))

(define-public ruby-rspec-pending-for
  (package
    (name "ruby-rspec-pending-for")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-pending_for" version))
       (sha256
        (base32
         "0f9sj7v3j14fvd631smxr04l53pk8dqwn9ybqkjdqmzvcv73b5n6"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-ruby-engine" ,ruby-ruby-engine)
       ("ruby-ruby-version" ,ruby-ruby-version)))
    (synopsis
     "Mark specs pending or skipped for specific Ruby engine (e.g. MRI or JRuby) / version combinations")
    (description
     "Mark specs pending or skipped for specific Ruby engine (e.g. MRI or JRuby) / version combinations")
    (home-page
     "https://github.com/pboling/rspec-pending_for")
    (license #f)))











;; (define-public ruby-appraisal
;;   (package
;;     (name "ruby-appraisal")
;;     (version "2.1.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (rubygems-uri "appraisal" version))
;;        (sha256
;;         (base32
;;          "10ng010lhswdykjhwic7bgv28qpjj42qwxvprpj1f4cavdimh4vp"))))
;;     (build-system ruby-build-system)
;;     (arguments
;;      `(#:tests? #f ; tests require network access
;;        #:test-target "spec"
;;        #:phases
;;        (modify-phases %standard-phases
;;          ;; remove bundler from Rakefile to avoid dependency issues
;;          (add-before 'check 'remove-dependency-checking
;;            (lambda _
;; ;             (substitute* "Rakefile"
;;  ;              (("^require 'bundler.*") ""))
;;              (substitute* "Gemfile"
;;                (("thor.*") "thor'\n"))
;;              #t)))))
;;     (propagated-inputs
;;      `(("bundler" ,bundler)
;;        ("ruby-thor" ,ruby-thor)))
;;     (native-inputs
;;      `(("ruby-activesupport" ,ruby-activesupport)
;;        ("ruby-rspec" ,ruby-rspec)))
;;     (synopsis
;;      "Appraisal integrates with bundler and rake to test your library against different versions of dependencies in repeatable scenarios called \"appraisals.\"")
;;     (description
;;      "Appraisal integrates with bundler and rake to test your library against different versions of dependencies in repeatable scenarios called \"appraisals.\"")
;;     (home-page
;;      "http://github.com/thoughtbot/appraisal")
;;     (license license:expat)))

(define-public ruby-kramdown
  (package
    (name "ruby-kramdown")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "kramdown" version))
       (sha256
        (base32
         "12sral2xli39mnr4b9m2sxdlgam4ni0a1mkxawc5311z107zj3p0"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-prawn" ,ruby-prawn)))
    (synopsis
     "kramdown is yet-another-markdown-parser but fast, pure Ruby,
using a strict syntax definition and supporting several common extensions.
")
    (description
     "kramdown is yet-another-markdown-parser but fast, pure Ruby,
using a strict syntax definition and supporting several common extensions.
")
    (home-page "http://kramdown.gettalong.org")
    (license license:expat)))

(define-public ruby-prawn
  (package
    (name "ruby-prawn")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "prawn" version))
       (sha256
        (base32
         "0z9q3l8l73pvx6rrqz40xz9xd5izziprjnimb572hcah6dh30cnw"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-pdf-core" ,ruby-pdf-core)
       ("ruby-ttfunk" ,ruby-ttfunk)))
    (synopsis
     "  Prawn is a fast, tiny, and nimble PDF generator for Ruby
")
    (description
     "  Prawn is a fast, tiny, and nimble PDF generator for Ruby
")
    (home-page
     "http://prawn.majesticseacreature.com")
    ;; Prawn is released under a slightly modified form of the License of Ruby,
    ;; allowing you to choose between Matz's terms, the GPLv2, or GPLv3.
    (license (list license:gpl2 license:gpl3
                   (license:non-copyleft "file://LICENSE"
                                         "See LICENSE in the distribution")))))

(define-public ruby-pdf-core
  (package
    (name "ruby-pdf-core")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pdf-core" version))
       (sha256
        (base32
         "1ks95byqs08vxgf2a7q3spryi467rwimm2awc84fqa2yxf97ikjy"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("pdf-reader" ,pdf-reader)
       ("pdf-inspector" ,pdf-inspector)))
    (synopsis
     "PDF::Core is used by Prawn to render PDF documents")
    (description
     "PDF::Core is used by Prawn to render PDF documents")
    (home-page
     "http://prawn.majesticseacreature.com")
    (license #f)))

(define-public ruby-pdf-reader
  (package
    (name "ruby-pdf-reader")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as rubygem does not contain tests
       (uri (string-append "https://github.com/yob/pdf-reader/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bvrjr93nhfhm652s6fc0i5kvs44y1bqa6dq5dyms38nfqnsym5b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; remove gems that are not actually required for running tests
         (add-before 'check 'remove-dependency-checking
           (lambda _
             (substitute* "pdf-reader.gemspec"
               ((".*spec.add_development_dependency.*ZenTest.*") "")
;;               ((".*spec.add_development_dependency.*cane.*") "")
               ((".*spec.add_development_dependency.*morecane.*") "")
               ((".*spec.add_development_dependency.*ir_b.*") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-afm" ,ruby-afm)
       ("ruby-ascii85" ,ruby-ascii85)
       ("ruby-hashery" ,ruby-hashery)
       ("ruby-rc4" ,ruby-rc4)
       ("ruby-ttfunk" ,ruby-ttfunk)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis
     "The PDF::Reader library implements a PDF parser conforming as much as possible to the PDF specification from Adobe")
    (description
     "The PDF::Reader library implements a PDF parser conforming as much as possible to the PDF specification from Adobe")
    (home-page "http://github.com/yob/pdf-reader")
    (license #f)))

(define-public ruby-cane
  (package
    (name "ruby-cane")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cane" version))
       (sha256
        (base32
         "176x9g8ax9xky4303sf0xzf6ya62mdrm4bfrdj0hhy45x43v03x1"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-parallel" ,ruby-parallel)
       ("bundler" ,bundler)))
    (synopsis
     "Fails your build if code quality thresholds are not met")
    (description
     "Fails your build if code quality thresholds are not met")
    (home-page "http://github.com/square/cane")
    (license license:asl2.0)))

(define-public ruby-parallel
  (package
    (name "ruby-parallel")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as rubygem does not contain tests
       (uri (string-append "https://github.com/grosser/parallel/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0py2zv58ar26v2inxbwq83p9ay62q74gh28iw6mvcfbyiiqby8im"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "rspec-rerun:spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* "parallel.gemspec"
               (("git ls-files") "find"))
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)
       ("ruby-i18n" ,ruby-i18n)))
    (synopsis "Run Ruby code in parallel processes")
    (description
     "Run any kind of code in parallel processes")
    (home-page "https://github.com/grosser/parallel")
    (license license:expat)))










(define-public ruby-coffee-script
  (package
    (name "ruby-coffee-script")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append
             "https://github.com/rails/ruby-coffee-script/archive/v"
             version ".tar.gz"))
       (sha256
        (base32
         "0gbcg40ks4ifm332ljmgq2l44ssld0z6xhjzk48v6mpaxyz8mc92"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test
           (lambda _
             ;; patch submitted upstream at
             ;; https://github.com/rails/ruby-coffee-script/pull/6
             (substitute* "test/test_coffee_script.rb"
               ((" unexpected unless\\\"")
                " unexpected unless\", '[stdin]:3:11: unexpected unless'"))
             #t)))))
    (propagated-inputs
     `(("ruby-coffee-script-source" ,ruby-coffee-script-source)
       ("ruby-execjs" ,ruby-execjs)
       ("ruby-duktape", ruby-duktape))) ; use as the JS interpreter
    (synopsis "bridge to the javascript CoffeeScript compiler.")
    (description
     "Ruby CoffeeScript is a bridge to the javascript CoffeeScript compiler.
CoffeeScript is an attempt to expose the good parts of JavaScript in a simple
way.")
    (home-page
     "http://github.com/rails/ruby-coffee-script")
    (license license:expat)))

(define-public ruby-coffee-script-source
  (package
  (name "ruby-coffee-script-source")
  (version "1.10.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "coffee-script-source" version))
      (sha256
        (base32
          "1k4fg39rrkl3bpgchfj94fbl9s4ysaz16w8dkqncf2vyf79l3qz0"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; no tests
  (synopsis "Source code for the ruby-coffee-script gem.")
  (description
    "This gem contains the JavaScript source code used for converting between
CoffeeScript to JavaScript.  It is updated each time a new version of
CoffeeScript is released.")
  ;; homepage listed on rubygems does not exist, so use the rubygems URL instead
  (home-page
    "https://rubygems.org/gems/coffee-script-source")
  (license license:expat)))

(define-public ruby-execjs
  (package
    (name "ruby-execjs")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append "https://github.com/rails/execjs/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "073kvakgb39gnip23kh4p2gwjydjyzv9hfsy22k80w8swgkh8rvc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; tests require ruby-therubyracer and thus v8.
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis
     "ExecJS lets you run JavaScript code from Ruby.")
    (description
     "ExecJS lets you run JavaScript code from Ruby.")
    (home-page "https://github.com/rails/execjs")
    (license license:expat)))

(define-public duktape
  (package
    (name "duktape")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       ;; Use the GitHub source for ease of updating.
       (uri (string-append "https://github.com/svaarala/duktape/releases/download/v"
                           version "/duktape-" version ".tar.xz"))
       (sha256
        (base32
         "0rd9wz5716qhzqwwj26i2x5m8dd020rvaf2i08sa4jxrl6nk3cil"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Tests require many dependencies including v8.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         ;; Installing is simply copying source code files so they can be
         ;; embedded elsewhere.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src (string-append out "/src")))
               (install-file "src/duktape.c" src)
               (install-file "src/duktape.h" src)))))))
    (synopsis "Embeddable JavaScript engine")
    (description
     "Duktape is an embeddable Javascript engine, with a focus on portability
and compact footprint.  Duktape is easy to integrate into a C/C++ project: add
@code{duktape.c}, @code{duktape.h}, and @code{duk_config.h} to your build, and
use the Duktape API to call Ecmascript functions from C code and vice versa.")
    (home-page "http://duktape.org")
    (license license:expat)))

(define-public ruby-duktape
  (package
    (name "ruby-duktape")
    (version "1.3.0.4")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append "https://github.com/judofyr/duktape.rb/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "1xgs7ll9xwm5p451mh70cm5646wijc2jdvjdb81a17wwvccf7djw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Duktape comes with the duktape .c and .h files.  Replace these with
         ;; those from the duktape Guix package.
         (add-after 'unpack 'replace-bundled-duktape
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (file)
                         (delete-file (string-append "ext/duktape/" file))
                         (copy-file (string-append (assoc-ref inputs "duktape")
                                                   "/src/" file)))
                       (list "duktape.c" "duktape.h"))))
         (add-before 'check 'remove-dependency
           (lambda _
             ;; Gem is not needed for testing.
             (substitute* "Gemfile"
               (("^  gem 'aws-sdk', '~> 2.2.0'") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-sdoc" ,ruby-sdoc)
       ("duktape" ,duktape)))
    (synopsis "Bindings to the Duktape JavaScript interpreter")
    (description
     "Bindings to the Duktape JavaScript interpreter")
    (home-page
     "https://github.com/judofyr/duktape.rb")
    (license license:expat)))

(define-public ruby-therubyracer
  (package
    (name "ruby-therubyracer")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "therubyracer" version))
       (sha256
        (base32
         "0x93jrl0ac5r0pfqnbv228mikgqxcvdy3rgkl322k1cssr1yvhid"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-libv8" ,ruby-libv8)
       ("ruby-ref" ,ruby-ref)))
    (synopsis
     "Call JavaScript code and manipulate JavaScript objects from Ruby. Call Ruby code and manipulate Ruby objects from JavaScript.")
    (description
     "Call JavaScript code and manipulate JavaScript objects from Ruby. Call Ruby code and manipulate Ruby objects from JavaScript.")
    (home-page
     "http://github.com/cowboyd/therubyracer")
    (license license:expat)))

(define-public ruby-libv8 ; fails to build because it tries to fetch v8 over the
                          ; network. Also unclear about how to package
                          ; javascript libraries in Guix generally.
  (package
    (name "ruby-libv8")
    (version "4.5.95.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "libv8" version))
       (sha256
        (base32
         "0i4mlxwn7iqxamxk69sxiqwmfdxvc0v5vs8dib8bxc7d6c4xk501"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ("python" ,python-2)
       ("which" ,which)))
    (synopsis
     "Distributes the V8 JavaScript engine in binary and source forms in order
to support fast builds of The Ruby Racer")
    (description
     "Distributes the V8 JavaScript engine in binary and source forms in order
to support fast builds of The Ruby Racer")
    (home-page "http://github.com/cowboyd/libv8")
    (license license:expat)))


;; haml 4 cannot currently be packaged because tilt is not yet packaged.
(define-public ruby-haml
  (package
    (name "ruby-haml")
    (version "4.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "haml" version))
       (sha256
        (base32
         "0mrzjgkygvfii66bbylj2j93na8i89998yi01fin3whwqbvx0m1p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; circular dependency with tilt
;    (propagated-inputs `(("ruby-tilt" ,ruby-tilt)))
    (synopsis "Haml (HTML Abstraction Markup Language) library.")
    (description
     "Haml (HTML Abstraction Markup Language) is a layer on top of HTML or XML
that is designed to express the structure of documents using indentation rather
than closing tags.  It was originally envisioned as a plugin for Ruby on Rails,
but it can function as a stand-alone templating engine.")
    (home-page "http://haml.info/")
    (license license:expat)))

;; haml 3 cannot currently be packaged because action_pack, action_controller
;; and action_view are required at test time.
(define-public ruby-haml-3
  (package
    (name "ruby-haml")
    (version "3.1.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "haml" version))
              (sha256
               (base32
                "05qnmrcjp85bgjwgmb0yhi7jyb7vd9jdqgxhzc7mmz0ch58rvxj4"))))
    (build-system ruby-build-system)
                                        ;    (arguments
                                        ;     `(#:tests? #f))
    (synopsis "Haml (HTML Abstraction Markup Language) library.")
    (description
     "Haml (HTML Abstraction Markup Language) is a layer on top of HTML or XML
that is designed to express the structure of documents using indentation rather
than closing tags.  It was originally envisioned as a plugin for Ruby on Rails,
but it can function as a stand-alone templating engine.")
    (home-page "http://haml.info/")
    (license license:expat)))





;; (define-public ruby-rack-test
;;   (package
;;     (name "ruby-rack-test")
;;     (version "0.6.3")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (rubygems-uri "rack-test" version))
;;        (sha256
;;         (base32
;;          "0h6x5jq24makgv2fq5qqgjlrk74dxfy62jif9blk43llw8ib2q7z"))))
;;     (build-system ruby-build-system)
;;     (arguments
;;      ;; Disable tests because of circular dependencies: requires sinatra,
;;      ;; which requires rack-protection, which requires rack-test.
;;      `(#:tests? #f
;;        #:phases
;;        (modify-phases %standard-phases
;;          (add-after 'enter-src-dir 'fix-dependencies
;;            (lambda _
;;              ;; Remove test dependencies as we are not testing.
;;              (substitute* "Gemfile"
;;                (("^gem \\\"sinatra\\\"") "")
;;                (("^gem 'rspec'") "")
;;                (("^gem \\\"codeclimate-test-reporter.*") ""))
;;              (substitute* "Rakefile"
;;                (("^require .rspec.*") "")))))))
;;     (propagated-inputs
;;      `(("ruby-rack" ,ruby-rack)))
;;     (native-inputs
;;      `(("bundler" ,bundler)))
;;     (synopsis "Testing API for Rack applications")
;;     (description
;;      "Rack::Test is a small, simple testing API for Rack applications.  It can
;; be used on its own or as a reusable starting point for Web frameworks and
;; testing libraries to build on.")
;;     (home-page "http://github.com/brynary/rack-test")
;;     (license license:expat)))




(define-public ruby-backports
  (package
  (name "ruby-backports")
  (version "3.6.7")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "backports" version))
      (sha256
        (base32
          "0m8jkjh4kymgfipd6yb0zcxlas4x5r60k2c94zkklb1ryma33grc"))))
  (build-system ruby-build-system)
  (synopsis
    "Essential backports that enable many of the nice features of Ruby 1.8.7 up to 2.1.0 for earlier versions.")
  (description
    "Essential backports that enable many of the nice features of Ruby 1.8.7 up to 2.1.0 for earlier versions.")
  (home-page
    "http://github.com/marcandre/backports")
  (license license:expat)))

(define-public ruby-faraday
  (package
    (name "ruby-faraday")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday" version))
       (sha256
        (base32
         "1kplqkpn2s2yl3lxdf6h7sfldqvkbkpxwwxhyk7mdhjplb5faqh6"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-multipart-post" ,ruby-multipart-post)))
    (synopsis "HTTP/REST API client library.")
    (description "HTTP/REST API client library.")
    (home-page
     "https://github.com/lostisland/faraday")
    (license license:expat)))

(define-public ruby-faraday-middleware
  (package
    (name "ruby-faraday-middleware")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday_middleware" version))
       (sha256
        (base32
         "0nxia26xzy8i56qfyz1bg8dg9yb26swpgci8n5jry8mh4bnx5r5h"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday)))
    (synopsis "Various middleware for Faraday")
    (description "Various middleware for Faraday")
    (home-page
     "https://github.com/lostisland/faraday_middleware")
    (license license:expat)))

(define-public ruby-gh
  (package
    (name "ruby-gh")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "gh" version))
       (sha256
        (base32
         "0j7m6jmxzkxvnqgnhmci33a89qpaxxcrm55kk5vz4bcpply04hx2"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-backports" ,ruby-backports)
       ("ruby-faraday" ,ruby-faraday)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-net-http-persistent"
        ,ruby-net-http-persistent)
       ("ruby-net-http-pipeline"
        ,ruby-net-http-pipeline)))
    (synopsis
     "multi-layer client for the github api v3")
    (description
     "multi-layer client for the github api v3")
    (home-page "http://gh.rkh.im/")
    (license license:expat)))

(define-public ruby-highline
  (package
    (name "ruby-highline")
    (version "1.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "highline" version))
       (sha256
        (base32
         "1nf5lgdn6ni2lpfdn4gk3gi47fmnca2bdirabbjbz1fk9w4p8lkr"))))
    (build-system ruby-build-system)
    (synopsis
     "A high-level IO library that provides validation, type conversion, and more for
command-line interfaces. HighLine also includes a complete menu system that can
crank out anything from simple list selection to complete shells with just
minutes of work.
")
    (description
     "A high-level IO library that provides validation, type conversion, and more for
command-line interfaces. HighLine also includes a complete menu system that can
crank out anything from simple list selection to complete shells with just
minutes of work.
")
    (home-page "https://github.com/JEG2/highline")
    (license #f)))

(define-public ruby-launchy
  (package
    (name "ruby-launchy")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "launchy" version))
       (sha256
        (base32
         "190lfbiy1vwxhbgn4nl4dcbzxvm049jwc158r2x7kq3g5khjrxa2"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)))
    (synopsis
     "Launchy is helper class for launching cross-platform applications in a fire and forget manner. There are application concepts (browser, email client, etc) that are common across all platforms, and they may be launched differently on each platform. Launchy is here to make a common approach to launching external application from within ruby programs.")
    (description
     "Launchy is helper class for launching cross-platform applications in a fire and forget manner. There are application concepts (browser, email client, etc) that are common across all platforms, and they may be launched differently on each platform. Launchy is here to make a common approach to launching external application from within ruby programs.")
    (home-page
     "http://github.com/copiousfreetime/launchy")
    (license #f)))



;; TODO: inherit from mime-types
;; (define-public ruby-mime-types-2
;;   (package
;;     (name "ruby-mime-types")
;;     (version "2.6.2")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (rubygems-uri "mime-types" version))
;;        (sha256
;;         (base32
;;          "136ybsrwn1k7zcbxbrczf0n4z3liy5ygih3q9798s8pi80smi5dm"))))
;;     (build-system ruby-build-system)
;;     (native-inputs
;;      `(("ruby-hoe" ,ruby-hoe)
;;        ("ruby-hoe-travis" ,ruby-hoe-travis)))
;;     (synopsis "library and registry for MIME content type definitions")
;;     (description "The mime-types library provides a library and registry for
;; information about MIME content type definitions.  It can be used to determine
;; defined filename extensions for MIME types, or to use filename extensions to
;; look up the likely MIME type definitions.")
;;     (home-page "https://github.com/mime-types/ruby-mime-types/")
;;     (license (list license:gpl2+ license:expat license:artistic2.0))))








(define-public ruby-travis
  (package
    (name "ruby-travis")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "travis" version))
       (sha256
        (base32
         "0s88790wlhlsaxs9561w3h6vhj00sc36bw6k7rajj6vi0416s73z"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-backports" ,ruby-backports)
       ("ruby-faraday" ,ruby-faraday)
       ("ruby-faraday-middleware"
        ,ruby-faraday-middleware)
       ("ruby-gh" ,ruby-gh)
       ("ruby-highline" ,ruby-highline)
       ("ruby-launchy" ,ruby-launchy)
       ("ruby-pry" ,ruby-pry)
       ("ruby-pusher-client" ,ruby-pusher-client)
       ("ruby-typhoeus" ,ruby-typhoeus)))
    (synopsis
     "CLI and Ruby client library for Travis CI")
    (description
     "CLI and Ruby client library for Travis CI")
    (home-page
     "https://github.com/travis-ci/travis.rb")
    (license license:expat)))


