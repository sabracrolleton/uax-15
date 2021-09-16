;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15 -*-

(in-package :uax-15)

(defun normalize (str normalization-form &key (rfc 3454))
  "Base external function which calls the appropriate normalization for the normalization form. The default normaliation form is :nfkc, but :nfd, :nfkd and :nfc are also available."
  (if (= rfc 3454)
      (ecase normalization-form
        (:nfd  (from-unicode-string (nfd (to-unicode-string str))))
        (:nfkd (from-unicode-string (nfkd (to-unicode-string str))))
        (:nfc  (from-unicode-string (nfc (to-unicode-string str))))
        (:nfkc (from-unicode-string (nfkc (to-unicode-string str)))))
      (cerror "RFCs other than 3454 and 4013 not yet supported. Sorry" (format nil "~a" rfc))))

(defun normalize-char (chr normalization-form)
  "Runs normalize on a single character input and returns a single character string. You must provide the normalization form (:nfd, :nfkd, :nfc, or :nfkc)"
  (ecase normalization-form
    (:nfd  (from-unicode-string (nfd (to-unicode-string (format nil "~a" chr)))))
    (:nfkd (from-unicode-string (nfkd (to-unicode-string (format nil "~a" chr)))))
    (:nfc  (from-unicode-string (nfc (to-unicode-string (format nil "~a" chr)))))
    (:nfkc (from-unicode-string (nfkc (to-unicode-string (format nil "~a" chr)))))))

(defun get-mapping (normalization-form &aux (mapping '()))
  "Note no mapping for :nfkc"
  (dolist (map (ecase normalization-form
                 (:nfd  (list *canonical-decomp-map*))
                 (:nfkd (list *canonical-decomp-map* *compatible-decomp-map*))
                 (:nfc  (list *canonical-comp-map*))))
    (maphash
     (lambda (from to)
       (flet ((to-str (x)
                (if (listp x) (coerce x 'string) (string x))))
             (case normalization-form
               (:nfd  (push (list (to-str from) (decompose (to-str to) :canonical)) mapping))
               (:nfkd (push (list (to-str from) (decompose (to-str to) :compatible)) mapping))
               (:nfc  (when (string= (compose (to-str from)) (to-str to))
                        (push (list (decompose (to-str from) :canonical) (to-str to)) mapping))))))
     map))

  ;; hangul
  (loop for code from #xAC00 below (+ #xAC00 11172)
        for char = (string (code-char code))
    do
    (case normalization-form
      ((:nfd :nfkd) (push (list char (decompose char :canonical)) mapping))
      ((:nfc)       (push (list (decompose char :canonical) char) mapping))))

  (nreverse mapping))

(defparameter *derived-normalization-props-data* (make-hash-table :test #'equal))

(defparameter *derived-normalization-props-data-file*
  (uiop:merge-pathnames* *data-directory* "DerivedNormalizationProps.txt"))

(defun get-canonical-combining-class-map ()
  *canonical-combining-class*)

(let ((nfd-illegal-list '())
      (nfkd-illegal-list '())
      (nfc-illegal-list '())
      (nfkc-illegal-list '()))

  (flet ((parse-line (maybe-key line)
           (let* ((fst (string-trim " " (car (cl-ppcre:split ";" line))))
                  (range (mapcar #'parse-hex-string-to-int (cl-ppcre:split "\\.\\." fst)))
                  (maybe? (and maybe-key (not (null (search maybe-key line))))))
             (loop for code from (first range) to (or (second range) (first range))
                   for char = code
               collect (list char maybe?)))))
    (with-open-file (in *derived-normalization-props-data-file*
                        #-(or :lispworks :genera :clisp) :external-format
                        #-(or :lispworks :genera :clisp) :UTF-8)
      (loop for line = (read-line in nil nil)
            while line
        do
        (cond ((or (search "NFKC_QC; N" line) (search "NFKC_QC; M" line))
               (nconcf nfkc-illegal-list (parse-line "NFKC_QC; M" line)))
              ((or (search "NFC_QC; N" line) (search "NFC_QC; M" line))
               (nconcf nfc-illegal-list (parse-line "NFC_QC; M" line)))
              ((search "NFKD_QC; N" line)
               (nconcf nfkd-illegal-list (parse-line nil line)))
              ((search "NFD_QC; N" line)
               (nconcf nfd-illegal-list (parse-line nil line)))))))

  (defun get-illegal-char-list (normalization-form)
    "Takes a normalization form, e.g. :nfkc and returns a list of lists of form (#\NO-BREAK_SPACE NIL) where the first item is the character name and the second item has the value N or M or nil indicating whether the character may require renormalization."
    (ecase normalization-form
      (:nfd  nfd-illegal-list)
      (:nfkd nfkd-illegal-list)
      (:nfc  nfc-illegal-list)
      (:nfkc nfkc-illegal-list))))

(defun unicode-letter-p (char)
  "Returns T if the character is one of the unicode characters falling into a letter category: uppercase, lowercase, titlecase, modifier and other."
  (when (gethash char *unicode-letters*)
    t))
