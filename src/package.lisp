;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER -*-

(defpackage :uax-15
  (:use :common-lisp)
  (:export #:normalize
           #:get-mapping
           #:get-illegal-char-list
           #:get-canonical-combining-class-map
           #:from-unicode-string
           #:to-unicode-string
           #:unicode-string
           #:codepoint-as-utf-16
           #:surrogates-to-codepoint
           #:unicode-letter-p))
