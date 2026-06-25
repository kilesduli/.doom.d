;;; autoload/duplicate-line.el -*- lexical-binding: t; -*-

;;;###autoload
(defun aa/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (current-column))
        (line (concat "\n" (buffer-substring-no-properties (pos-bol) (pos-eol)))))
    (end-of-line)
    (insert line)
    (beginning-of-line)
    (forward-char column)))

