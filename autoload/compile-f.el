;;; autoload/compile-f.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar vv/compile-interactive-flag nil)

;;;###autoload
(defun ff/compile-expand-placeholders (args)
  (pcase-let ((`(,command ,comint) args))
    (let ((expanded (replace-regexp-in-string
                     "@f"
                     (shell-quote-argument
                      (or buffer-file-name ""))
                     command
                     t t)))
      (if (not vv/compile-interactive-flag)
          (list command comint)
        (setq vv/compile-interactive-flag nil)
        (list expanded comint)))))
