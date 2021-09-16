;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15 -*-

(in-package :uax-15)

(defparameter *data-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "data") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'uax-15 nil))))

;; http://www.unicode.org/L2/L1999/UnicodeData.html


(defvar *unicode-data*
  (with-open-file (in (uiop:merge-pathnames* *data-directory* "UnicodeData.txt")
                      #-(or :lispworks :genera :clisp) :external-format
                      #-(or :lispworks :genera :clisp) :UTF-8)
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))

(let ((canonical-decomp-map (make-hash-table))
      (compatible-decomp-map (make-hash-table))
      (canonical-combining-class (make-hash-table)))
  (loop for (1st _ __ 4th ___ 6th) in *unicode-data*
        for char = (parse-hex-string-to-int 1st)
        for ccc  = (parse-integer 4th)
        for decomp-chars =
        (let ((tmp (cl-ppcre:split " " 6th)))
          (when tmp
            (if (char= #\< (char (first tmp) 0))
                (cons :compatible (mapcar #'parse-hex-string-to-int (cdr tmp))) ; swap decomposition
              (cons :canonical (mapcar #'parse-hex-string-to-int tmp)))))       ; formal decomposition
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
(with-open-file (in (uiop:merge-pathnames* *data-directory* "CompositionExclusions.txt")
                    #-(or :lispworks :genera :clisp) :external-format
                    #-(or :lispworks :genera :clisp) :UTF-8)
  (loop for line = (read-line in nil nil)
     while line
       when (and (plusp (length line))
                  (char/= (char line 0) #\#))
         do (setf (gethash (parse-hex-string-to-int (subseq line 0 (position #\Space line)))
                           *composition-exclusions-data*)
                  t)))

(defparameter *canonical-comp-map* (make-hash-table :test #'equal))
(maphash
   (lambda (src-char decomped-chars)
     (when (and (= 2 (length decomped-chars))
                (not (gethash src-char *composition-exclusions-data*)))
       (setf (gethash (coerce decomped-chars 'list)
                      *canonical-comp-map*)
             src-char)))
   *canonical-decomp-map*)

(defparameter *unicode-letters* (make-hash-table :size 170000))
(loop for x in uax-15::*unicode-data*
      do
      (when (member (third x) '("Ll" "Lu" "Lm" "Lt" "Lo") :test 'equal)
        (let ((char (char-from-hexstring (first x))))
          (setf (gethash char
                         *unicode-letters*)
                      (third x)))))
(loop for code from #x3400 below #x4DB5 ; CJK Ideograph Extension A
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))
(loop for code from #x4E00 below #x9FEF ; CJK Ideograph
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

(loop for code from #xAC00 below #xD7A3 ; Hangul Syllable
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#-utf-16 (loop for code from #x17000 below #x187F7 ; Tangut Ideograph
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#-utf-16 (loop for code from #x20000 below #x2A6D6 ; CJK Ideograph Extension B
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#-utf-16 (loop for code from #x2A700 below #x2B734 ; CJK Ideograph Extension C
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#-utf-16 (loop for code from #x2B740 below #x2B81D ; CJK Ideograph Extension D
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#-utf-16 (loop for code from #x2B820 below #x2CEA1 ; CJK Ideograph Extension E
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#-utf-16 (loop for code from #x2CEB0 below #x2EBE0 ; CJK Ideograph Extension F
      do
      (setf (gethash (code-char code) *unicode-letters*) "Lo"))

#|
Letter characters in ranges

4E00;<CJK Ideograph, First>;Lo;0;L;;;;;N;;;;;
9FEF;<CJK Ideograph, Last>;Lo;0;L;;;;;N;;;;;

AC00;<Hangul Syllable, First>;Lo;0;L;;;;;N;;;;;
D7A3;<Hangul Syllable, Last>;Lo;0;L;;;;;N;;;;;

17000;<Tangut Ideograph, First>;Lo;0;L;;;;;N;;;;;
187F7;<Tangut Ideograph, Last>;Lo;0;L;;;;;N;;;;;

20000;<CJK Ideograph Extension B, First>;Lo;0;L;;;;;N;;;;;
2A6D6;<CJK Ideograph Extension B, Last>;Lo;0;L;;;;;N;;;;;

2A700;<CJK Ideograph Extension C, First>;Lo;0;L;;;;;N;;;;;
2B734;<CJK Ideograph Extension C, Last>;Lo;0;L;;;;;N;;;;;

2B740;<CJK Ideograph Extension D, First>;Lo;0;L;;;;;N;;;;;
2B81D;<CJK Ideograph Extension D, Last>;Lo;0;L;;;;;N;;;;;

2B820;<CJK Ideograph Extension E, First>;Lo;0;L;;;;;N;;;;;
2CEA1;<CJK Ideograph Extension E, Last>;Lo;0;L;;;;;N;;;;;

2CEB0;<CJK Ideograph Extension F, First>;Lo;0;L;;;;;N;;;;;
2EBE0;<CJK Ideograph Extension F, Last>;Lo;0;L;;;;;N;;;;;

|#
