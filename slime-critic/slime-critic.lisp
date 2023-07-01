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

(declaim (ftype (function ((or pathname string) &key (:names list)
                                                (:return (member :simple :slime-notes)))
                          list)
                critique-file))

(defun critique-file
    (file &key (names (lisp-critic::get-pattern-names))
            (return :simple))
  "Critique definitions found in FILE, using patterns in NAMES.
The result depends on the value of RETURN:
- :SIMPLE: a list of (CONS file-position definition-critique).
- :SLIME-NOTES: the list of critiques in Emacs slime-note format."
  (let (critiques)
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((file-position (file-position in) (file-position in))
	     (code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (let ((critique
                  (with-output-to-string (out)
                    (lisp-critic::critique-definition code out names))))
            (when (not (zerop (length critique)))
              (setq critique (reformat-critique critique))
              (case return
                (:simple
                 (push (cons file-position critique)
                       critiques))
                (:slime-notes
                 (push (list :severity :STYLE-WARNING ;; :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
                             :message critique
                             :source-context nil
                             ;; See slime-goto-source-location for location format
                             :location (list :location
                                             (list :file (princ-to-string file))
                                             (list :position file-position 0)
                                             nil))
                       critiques))))))))
    (nreverse critiques)))

(provide 'slime-critic)
