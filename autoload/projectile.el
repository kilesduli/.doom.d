;;; autoload/projectile.el -*- lexical-binding: t; -*-

;; Seems we just need C-c p i
;;;###autoload
(defalias 'aa/projectile-reset-current-project-cache #'projectile-invalidate-cache)

;;;###autoload
(defun aa/projectile-reset-all-project-cache ()
  (interactive)
  (setq projectile-projects-cache (make-hash-table :test 'equal))
  (setq projectile-project-type-cache (make-hash-table :test 'equal))
  (setq projectile-projects-cache-time (make-hash-table :test 'equal))
  (delete-directory doom-project-cache-dir 't)
  (mkdir doom-project-cache-dir)
  (projectile-save-known-projects)
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))
