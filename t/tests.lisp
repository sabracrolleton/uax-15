;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15-TESTS; -*-
(in-package :uax-15-tests)

(fiveam:def-suite :uax-15
    :description "Test suite for uax-15")
(fiveam:in-suite :uax-15)

(defun parse-hex-string-to-string (str)
  "Takes a string which may be one or more hex numbers e.g. '0044 0307', builds an array of characters, coerces to string and returns the string. Mostly used for testing."
  (let* ((split-str (split-sequence:split-sequence #\Space str :remove-empty-subseqs t))
         (arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
         (setf (aref arry (- y 1)) (parse-hex-string-to-int x)))
    (from-unicode-string (coerce arry 'unicode-string))))

(defun parse-hex-string-to-int (str)
  "Parse a string which is a single character in hex to a decimal."
  (parse-integer str :radix 16))

(defun parse-hex-string-to-char (str)
  "Parse a hex string which is a single character into a character using code-char."
  (code-char (parse-hex-string-to-int str)))

(defun int-to-hex-string (int)
  (write-to-string int :base 16))

(defun parse-hex-list-to-string (lst)
  "Takes a list of numbers and returns a string of characters"
  (let ((arry (make-array (length lst))))
    (loop for x in lst counting x into y do
         (setf (aref arry (- y 1)) (code-char x)))
    (coerce arry 'string)))



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

(test part0-nfkc
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part1-nfkc
  (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part2-nfkc
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part3-nfkc
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part0-nfkd
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part1-nfkd
  (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part2-nfkd
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part3-nfkd
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part0-nfc
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part1-nfc
  (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part2-nfc
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part3-nfc
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part0-nfd
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(test part1-nfd
      (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(test part2-nfd
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(test part3-nfd
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))
