;;; autoload/org-block-visibility.el -*- lexical-binding: t; -*-

(defvar vv/org-cycle-hide-result-startup nil)

;;;###autoload
(defun ff/block-visibility-according-to-property ()
  (org-block-map
   (lambda ()
     (pcase (cdr (assq :visibility (nth 2 (org-babel-get-src-block-info))))
       ("fold"
        (org-fold-hide-block-toggle 'hide)
        (when-let ((location (org-babel-where-is-src-block-result)))
          (goto-char location)
          (org-babel-hide-result-toggle 'hide)))
       ("all"
        (org-fold-hide-block-toggle 'off)
        (when-let ((location (org-babel-where-is-src-block-result)))
          (goto-char location)
          (org-babel-hide-result-toggle 'off)))
       ("block"
        (org-fold-hide-block-toggle 'off)
        (when-let ((location (org-babel-where-is-src-block-result)))
          (goto-char location)
          (org-babel-hide-result-toggle 'hide)))
       ("results"
        (org-fold-hide-block-toggle 'hide)
        (when-let ((location (org-babel-where-is-src-block-result)))
          (goto-char location)
          (org-babel-hide-result-toggle 'off)))))))
