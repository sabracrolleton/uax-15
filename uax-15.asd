;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: ASDF -*-

(defsystem "uax-15"
  :description "Common lisp implementation of Unicode normalization functions :nfc, :nfd, :nfkc and :nfkd (Uax-15)"
  :author "Takeru Ohta, Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on ("split-sequence" "cl-ppcre")
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "utilities" :depends-on ("package"))
                         (:file "trivial-utf-16" :depends-on ("package"))
                         (:file "precomputed-tables" :depends-on ("package" "utilities" "trivial-utf-16"))
                         (:file "normalize-backend" :depends-on ("package" "utilities" "precomputed-tables" "trivial-utf-16"))
                         (:file "uax-15" :depends-on ("package" "utilities" "normalize-backend" "trivial-utf-16")))))
  :in-order-to ((test-op (test-op "uax-15/tests"))))

(defsystem "uax-15/tests"
  :depends-on ("uax-15" "parachute" "cl-ppcre" "split-sequence")
  :components
  ((:module "src"
            :components ((:file "trivial-utf-16")))
   (:module "t"
            :depends-on ("src")
            :components ((:file "test-package")
                         (:file "tests"))))
  :perform (test-op (o c)
		    (symbol-call :parachute '#:test (find-symbol* 'suite :uax-15-tests)
				      :report (find-symbol* 'quiet :parachute))))
