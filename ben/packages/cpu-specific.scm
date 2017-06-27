;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;;
;;; This code is free software; you can redistribute it and/or modify it
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

(define-module (ben packages cpu-specific)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages bioinformatics))

;; "sandybridge" for Ben's laptop
;;(define cpu "sandybridge")

(define-public (gcc-cpu-specific cpu)
  (let ((base gcc-7))
    (package
     (inherit base)
     (name "gcc-cpu-specific")
     (version (string-append (package-version base) "-cpu-specific-" cpu))
     (arguments
      (substitute-keyword-arguments (package-arguments base)
        ((#:configure-flags configure-flags)
         `(append ,configure-flags
                  (list (string-append
                         "--with-arch=" ,cpu)))))))))

(define-public (cpu-specific-package base-package cpu)
  (package
    (inherit base-package)
    (name (package-name base-package))
    ;; We must set a higher package version so this package is used instead of
    ;; the package in Guix proper.
    (version (string-append (package-version base-package) "-cpu-specific-" cpu))
    (inputs
     `(,@(package-inputs base-package)
       ("gcc" ,((@@ (gnu packages commencement)
                    gcc-toolchain) (gcc-cpu-specific cpu)))))))

;; (define-public diamond-cpu-specific (cpu-specific-package diamond))
;; (define-public fasttree-cpu-specific (cpu-specific-package fasttree))
;; (define-public blast+-cpu-specific (cpu-specific-package blast+))
;; (define-public bwa-cpu-specific (cpu-specific-package bwa))
;; (define-public metabat-cpu-specific (cpu-specific-package metabat))
