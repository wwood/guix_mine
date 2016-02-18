;;; GNU Guix --- Functional package management for GNU
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

(define-module (ben packages chromium)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages python)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public chromium-depot-tools
  ;; use git as there are no releases
  (let ((revision "1")
        (commit "c53b8cbf728bac2504a6ad22774c8097a9424415"))
    (package
      (name "chromium-depot-tools")
      (version (string-append revision "." commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://chromium.googlesource.com/chromium/tools/depot_tools.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0m3i7d4n0cs4dqrc75inqqyz2z09m05hwgs9q33iclvkv3xwchjw"))))
      (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; tests require network access
       #:phases
       (modify-phases %standard-phases
         ;(add-after 'enter-dir 'remove-third-party-code
           ;(delete-file "third_party"))
         (delete 'configure)
         (delete 'build))))
    (inputs
     `(("python" ,python-2)))
    (synopsis "")
    (description
     "")
    (home-page "http://www.chromium.org/developers/how-tos/depottools")
    (license license:expat))))

(define-public v8
  (package
    (name "v8")
    (version "4.9.265")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://chromium.googlesource.com/v8/v8.git")
             (commit "eae618137190cfe34670243e10b5f1d7fe586b2a")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "05i8mbn6g5m3p47cps75frqay1pqixnkdvgqifvp155f0q32gisv"))))
    (build-system gnu-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;                                     ;(add-after 'enter-dir 'remove-third-party-code
    ;;                                     ;(delete-file "third_party"))
    ;;      (delete 'configure)
    ;;      (delete 'build))))
    (inputs
     `(("python" ,python-2)))
    (native-inputs
     `(("gyp" ,gyp)))
    (synopsis "")
    (description
     "")
    (home-page "https://chromium.googlesource.com/v8/v8.git")
    (license license:expat)))

(define-public gyp
  ;; use git as there are no releases
  (let ((revision "1")
        (commit "b85ad3e578da830377dbc1843aa4fbc5af17a192"))
    (package
      (name "gyp")
      (version (string-append revision "." commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://chromium.googlesource.com/external/gyp.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "17y780vq97mr0aqxkmgq8h2y92dzg3p6bhnvik05m8gsr3l2vk2f"))))
      (build-system python-build-system)
      (arguments
       `(;#:tests? #f ; a minority of tests fail for unknown reasons
         #:python ,python-2
         #:phases
         (modify-phases %standard-phases
           ;; TODO: remove bundled python stuff
           (replace 'check
             (lambda _
               (setenv "CC" "gcc")
               (setenv "CXX" "g++")
               (zero? (system* "python" "gyptest.py"
                               "-a")))))))
      (native-inputs
       `(("python-setuptools" ,python2-setuptools)))
      (inputs
       `(("gcc" ,gcc-5) ; need updated GCC to pass tests?
         ("ninja" ,ninja)))
      (synopsis "")
      (description
       "")
      (home-page "")
      (license license:expat))))

;;; not needed?
(define-public chromium-buildtools
  ;; use git as there are no releases
  (let ((revision "1")
        (commit "2f04b1cc1e012d0e016de9a201aacc9e1cd0b804"))
    (package
      (name "chromium-buildtools")
      (version (string-append revision "." commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://chromium.googlesource.com/chromium/buildtools.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1l2ci2sfskh259n0jjjldd0naig5hb5qw42287wpzsnmvr24w8a3"))))
      (build-system gnu-build-system)
      (synopsis "")
      (description
       "")
      (home-page "")
      (license license:expat))))

(define-public chromium
  (package
    (name "chromium")
    (version "49.0.2605.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://chromium.googlesource.com/chromium/src.git")
             (commit "162eed838ef068b4980eb112579868b283a208a3")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "05i8mbn6g5m3p47cps75frqay1pqixnkdvgqifvp155f0q32gisv"))))
    (build-system gnu-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;                                     ;(add-after 'enter-dir 'remove-third-party-code
    ;;                                     ;(delete-file "third_party"))
    ;;      (delete 'configure)
    ;;      (delete 'build))))
    (synopsis "")
    (description
     "")
    (home-page "https://chromium.googlesource.com")
    (license license:expat)))

(define-public googletest ; unsure whether this works or not. Intended to be
                          ; bundled rather than packaged?
  ;; use git as there are no recent releases
  (let ((revision "1")
        (commit "13206d6f53aaff844f2d3595a01ac83a29e383db"))
    (package
      (name "googletest")
      (version (string-append "1.7.0." revision "." commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/google/googletest.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1v73r7wm9q9kpjbpbk0qs15703y2caaz6dkvz9bkn7nnv98jbqqc"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("python" ,python-wrapper)))
      (synopsis "")
      (description
       "")
      (home-page "")
      (license license:expat))))
