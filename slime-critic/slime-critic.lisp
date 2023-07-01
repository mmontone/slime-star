(defpackage :slime-critic
  (:use :cl)
  (:export :critique-file))

(in-package :slime-critic)

(declaim (ftype (function (string) string) reformat-critique))
(defun reformat-critique (critique)
  "Remove the separators from CRITIQUE."
  (with-input-from-string (in critique)
    (let ((lines (uiop/stream:slurp-stream-lines in)))
      (with-output-to-string (s)
        (dolist (line (butlast (rest lines)))
          (write-string line s)
          (terpri s))))))

(declaim (ftype (function ((or pathname string) &optional list) list) critique-file))
(defun critique-file
    (file &optional (names (lisp-critic::get-pattern-names)))
  "Critique definitions found in FILE, using patterns in NAMES.
The result is a list of (CONS file-position definition-critique)."
  (let (critiques)
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (let ((critique
                  (with-output-to-string (out)
                    (lisp-critic::critique-definition code out names))))
            (when (not (zerop (length critique)))
              (push (cons (file-position in)
                          (reformat-critique critique))
                    critiques))))))
    (nreverse critiques)))
