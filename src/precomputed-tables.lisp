;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :uax-15)

(defparameter *data-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "data") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'uax-15 nil))))

;; http://www.unicode.org/L2/L1999/UnicodeData.html


(defvar *unicode-data*
  (with-open-file (in (uiop:merge-pathnames* *data-directory* "UnicodeData.txt"))
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))

(let ((canonical-decomp-map (make-hash-table))
      (compatible-decomp-map (make-hash-table))
      (canonical-combining-class (make-hash-table)))
  (loop for (1st _ __ 4th ___ 6th) in *unicode-data*
        for char = (parse-hex-string-to-char 1st)
        for ccc  = (parse-integer 4th)
        for decomp-chars =
        (let ((tmp (cl-ppcre:split " " 6th)))
          (when tmp
            (if (char= #\< (char (first tmp) 0))
                (cons :compatible (mapcar #'parse-hex-string-to-char (cdr tmp))) ; swap decomposition
              (cons :canonical (mapcar #'parse-hex-string-to-char tmp)))))       ; formal decomposition
    do
    (when (plusp ccc)
      (setf (gethash char canonical-combining-class) ccc))

    (when decomp-chars
      (if (eq (car decomp-chars) :canonical)
          (setf (gethash char canonical-decomp-map) (cdr decomp-chars))   ; formal decomposition
        (setf (gethash char compatible-decomp-map) (cdr decomp-chars))))) ; swap decomposition

  (defvar *canonical-decomp-map* canonical-decomp-map)
  (defvar *compatible-decomp-map* compatible-decomp-map)
  (defvar *canonical-combining-class* canonical-combining-class))


(defparameter *composition-exclusions-data* (make-hash-table))
(with-open-file (in (uiop:merge-pathnames* *data-directory* "CompositionExclusions.txt"))
  (loop for line = (read-line in nil nil)
     while line
       when (and (plusp (length line))
                  (char/= (char line 0) #\#))
         do (setf (gethash (parse-hex-string-to-char (subseq line 0 (position #\Space line)))
                           *composition-exclusions-data*)
                  t)))

(let ((canonical-comp-map (make-hash-table :test #'equal)))
  (maphash
   (lambda (src-char decomped-chars)
     (when (and (= 2 (length decomped-chars))
                (not (gethash src-char *composition-exclusions-data*)))
       (setf (gethash (coerce decomped-chars 'list)
                      canonical-comp-map)
             src-char)))
   *canonical-decomp-map*)
  (defparameter *canonical-comp-map* canonical-comp-map))
