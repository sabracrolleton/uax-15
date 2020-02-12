;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :uax-15
  (:use :common-lisp :trivial-utf-16)
  (:export #:normalize
           #:get-mapping
           #:get-illegal-char-list
           #:get-canonical-combining-class-map))
