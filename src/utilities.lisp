;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15 -*-

(in-package :uax-15)

(define-condition bad-char-error (error)
  ((message
    :initarg :message
    :accessor bad-char-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor bad-char-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (normalization-form
    :initarg :normalization-form
    :accessor bad-char-error-normalization-form
    :initform nil
    :documentation "The normalization form for the error was signalled.")))

(defmethod print-object ((object bad-char-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[L~A ~]~S~@[: ~S~]"
            (bad-char-error-normalization-form object)
            (bad-char-error-message object)
            (bad-char-error-value object))))

(defun bad-char-error (message &key value normalization-form)
  (error 'bad-char-error
         :message message
         :value value
         :normalization-form normalization-form))

(defmacro nconcf (list1 list2) `(setf ,list1 (nconc ,list1 ,list2)))

(defun parse-hex-string-to-string (str)
  "Takes a string which may be one or more hex numbers e.g. '0044 0307', builds an array of characters, coerces to string and returns the string. Mostly used for testing."
  (let* ((split-str (split-sequence:split-sequence #\Space str :remove-empty-subseqs t))
         (arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
         (setf (aref arry (- y 1)) (parse-hex-string-to-char x)))
    (coerce arry 'string)))

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
