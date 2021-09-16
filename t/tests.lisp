;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15-TESTS -*-

(in-package :uax-15-tests)

(define-test uax-15)

(defun parse-hex-string-to-string (str)
  "Takes a string which may be one or more hex numbers e.g. '0044 0307', builds an array of characters, coerces to string and returns the string. Mostly used for testing."
  (let* ((split-str (split-sequence:split-sequence #\Space str :remove-empty-subseqs t))
         (arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
         (setf (aref arry (- y 1)) (parse-integer x :radix 16)))
    (uax-15:from-unicode-string (coerce arry 'uax-15:unicode-string))))

(defun hs-to-cs (str)
  "Syntactic sugar"
  (parse-hex-string-to-string str))

(defparameter *test-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "t") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'uax-15 nil))))

(defun read-test-data (fname)
  (with-open-file (in (uiop:merge-pathnames* *test-directory* fname))
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))

(defun first-failure (fname fmt)
  "Reports the data line for the first failure for debugging purposes where fname is the test data filename and fmt is e.g. :nfkc"
  (loop for x in (read-test-data fname) counting x into y do
       (when (not (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) fmt)))
         (format t "Failure Line ~a ~a fourth should equal normalized first" y x)
         (return-from first-failure))))

(defparameter *part0* (read-test-data "test-part0.txt"))
(defparameter *part1* (read-test-data "test-part1.txt"))
(defparameter *part2* (read-test-data "test-part2.txt"))
(defparameter *part3* (read-test-data "test-part3.txt"))

(define-test suite)

(define-test part0-nfkc
    :parent suite
  (loop for x in *part0* do
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(define-test part1-nfkc
    :parent suite
    (loop for x in *part1* do
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(define-test part2-nfkc
    :parent suite
  (loop for x in *part2* do
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(define-test part3-nfkc
    :parent suite
    (loop for x in *part3* do
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(define-test part0-nfkd
    :parent suite
  (loop for x in *part0* do
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(define-test part1-nfkd
    :parent suite
  (loop for x in *part1* do
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(define-test part2-nfkd
    :parent suite
  (loop for x in *part2* do
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(define-test part3-nfkd
        :parent suite
  (loop for x in *part3* do
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(define-test part0-nfc
    :parent suite
  (loop for x in *part0* do
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(define-test part1-nfc
    :parent suite
  (loop for x in *part1* do
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(define-test part2-nfc
    :parent suite
  (loop for x in *part2* do
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(define-test part3-nfc
    :parent suite
  (loop for x in *part3* do
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(define-test part0-nfd
    :parent suite
  (loop for x in *part0* do
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(define-test part1-nfd
    :parent suite
      (loop for x in *part1* do
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(define-test part2-nfd
        :parent suite
  (loop for x in *part2* do
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(define-test part3-nfd
        :parent suite
  (loop for x in *part3* do
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(define-test unicode-letters
  :parent suite
  (true (every #'unicode-letter-p "새우"))
  (true (every #'unicode-letter-p "이해"))
  (false (every #'unicode-letter-p "zAp2"))
  (true (every #'unicode-letter-p "タイムゾーン"))
  (true (every #'unicode-letter-p "タイムゾーン"))
  (true (every #'unicode-letter-p "时区"))
  (true (every #'unicode-letter-p "時區")))
