;;; autoload/generate-frame-title.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ff/generate--frame-format ()
  (let* ((dir (or (vc-root-dir) default-directory))
         (projectp (and (bound-and-true-p project--list)
                        (listp project--list)
                        (member (list dir) project--list)))
         (projectilep (and (bound-and-true-p projectile-known-projects)
                           (listp projectile-known-projects)
                           (member dir projectile-known-projects))))
    (cond
     ((and (or projectilep projectp) (eq major-mode #'dired-mode))
      `("@[" ,(abbreviate-file-name dir) "]"))
     ((or projectilep projectp)
      `("%b" " - @[" ,(abbreviate-file-name dir) "]"))
     ((eq major-mode #'dired-mode)
      `("[" ,(abbreviate-file-name dir) "]"))
     (t
      `("%b" " - GNU Emacs at " system-name)))))
