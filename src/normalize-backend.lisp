;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15 -*-

(in-package :uax-15)

(defun get-canonical-combining-class (ch)
  (gethash ch *canonical-combining-class* 0))

(defun decompose-char (char &optional (type :canonical))
  (let ((decomped-chars (or (gethash char *canonical-decomp-map*)
                            (and (eq type :compatible)
                                 (gethash char *compatible-decomp-map*)))))
    (if decomped-chars
        (mapcan (lambda (c) (decompose-char c type)) decomped-chars)
      (list char))))

(let* ((s-base #xAC00)
       (l-base #x1100)
       (v-base #x1161)
       (t-base #x11A7)
       (l-count 19)
       (v-count 21)
       (t-count 28)
       (n-count (* v-count t-count))
       (s-count (* l-count n-count)))
  ;; split
  (defun decompose-hangul-char (ch)
    (let ((s-index (- ch s-base)))
      (unless (<= 0 s-index (1- s-count))
        (return-from decompose-hangul-char (list ch)))

      (let ((lc (+ l-base (floor s-index n-count)))
            (vc (+ v-base (floor (mod s-index n-count) t-count)))
            (tc (+ t-base (mod s-index t-count))))
        (if (/= tc t-base)
            (list lc vc tc)
          (list lc vc)))))

  ;; synthesis
  (defun compose-hangul (str &aux (len (length str)))
    (if (zerop len)
        str
      (let* ((last (aref str 0))
             (new-chars (list last)))
        (loop for i from 1 below len
              for ch = (aref str i)
              for l-index = (- last l-base)
              for s-index = (- last s-base)
          do
          (tagbody
           ;; 1. check to see if two current characters are L and V
           (when (<= 0 l-index (1- l-count))
             (let ((v-index (- ch v-base)))
               (when (<= 0 v-index (1- v-count))
                 ;; make syllable of form LV
                 (setf last
                       (+ s-base (* (+ (* l-index v-count) v-index) t-count)))
		 (setf (car new-chars) last) ; reset last
                 (go :end))))                ; discard ch

           ;; 2. check to see if two current characters are LV and T
           (when (and (<= 0 s-index (1- s-count))
                      (zerop (mod s-index t-count)))
             (let ((t-index (- ch t-base)))
               (when (< 0 t-index t-count)
                 ;; make syllable of form LVT
                 (setf last (+ last t-index))
                 (setf (car new-chars) last) ; reset last
                 (go :end))))                ; discard ch

           ;; if neigher case was true, just add the character
           (push (setf last ch) new-chars)
           :end))
        (coerce (nreverse new-chars) 'unicode-string)))))

(defun decompose (s type)
  (loop for c across s
    append
      (mapcan #'decompose-hangul-char (decompose-char c type))
      into new-s
    finally
      (return (coerce new-s 'unicode-string))))

(defun canonical-ordering (decomposed-string &aux (s decomposed-string))
  (let ((starter-indices
         (loop for i from 1 below (length s)
               for ccc = (get-canonical-combining-class (aref s i))
               when (zerop ccc)
           collect i)))
    (loop for (beg end) on (cons 0 starter-indices) do
      (setf #1=(subseq s beg end)
            (stable-sort #1# #'< :key #'get-canonical-combining-class))))
  s)

(defun compose (decomposed-string)
  (let* ((s decomposed-string)
         (to-cs (coerce s 'simple-vector)))
    (loop for i from 1 below (length s)
          for ch-right  = (aref s i)      ; right character
          for ccc-right = (get-canonical-combining-class ch-right)
       do
      (loop for j from (1- i) downto 0
            for ch-left  = (aref to-cs j) ; left character
            for ccc-left = (and ch-left (get-canonical-combining-class ch-left))
            when ch-left
         do
        (when (zerop ccc-left)
          ;; ch-left + ch-right  if there is a composite character ch-left replace
          (let ((comped-char (gethash (list ch-left ch-right) *canonical-comp-map*)))
            (when comped-char
              (setf (aref to-cs j) comped-char
                    (aref to-cs i) nil)))
          (return))

        (unless (< ccc-left ccc-right)
          (return))))
    (compose-hangul (coerce (remove nil to-cs) 'unicode-string))))

;;;;;;;;;;;;;;;;;;;;;;
;;;; NFD/NFKD/NFC/NFKC
(defun nfd (s)
  (canonical-ordering (decompose s :canonical)))

(defun nfkd (s)
  (canonical-ordering (decompose s :compatible)))

(defun nfc (s)
  (compose (nfd s)))

(defun nfkc (s)
 (compose (nfkd s)))
