;;; autoload/project-corresponding-files.el -*- lexical-binding: t; -*-

(defvar vv/project-corresponding-files nil)
(defvar vv/project-corresponding-files-dir-locals-file (concat "~/" dir-locals-file))

(defun ff/create-or-select-note-by-choice (prompt)
  (save-window-excursion
    (let ((choice (read-char-choice prompt '(?c ?s ?q))))
      ;; It shouldn't switch buffer
      (pcase choice
        (?c (call-interactively #'denote))
        (?s (progn
              (+default/find-in-notes)
              (buffer-file-name)))
        (?q (progn (message "quit")
                   (user-error "No file is created or selected")))))))

(defun ff/find-corresponding-project-file (type force-file-missing-p)
  (let* ((project-dir (cdr-safe (project-current)))
         (key (cons project-dir type))
         (file (alist-get key vv/project-corresponding-files nil nil #'equal)))
    (when (or (not file)
              force-file-missing-p)
      (when-let* ((filename (ff/create-or-select-note-by-choice
                             (format "project corresponding file(Type: %s) not exist. create or select one?\n[c]reate [s]elect [q] quit" (symbol-name type)))))
        (setf (alist-get key vv/project-corresponding-files nil nil #'equal) filename)
        (save-window-excursion
          (add-dir-local-variable nil 'vv/project-corresponding-files vv/project-corresponding-files vv/project-corresponding-files-dir-locals-file)
          (save-buffer))
        (setq file filename)))
    file))

(defun ff/remove-current-project-corresponding-file (type)
  (let* ((project-dir (cdr-safe (project-current)))
         (key (cons project-dir type)))
    (when (yes-or-no-p (format "Remove corresponding %s file for %s" (symbol-name type) project-dir))
      (setq vv/project-corresponding-files (assoc-delete-all key vv/project-corresponding-files #'equal))
      (save-window-excursion
        (add-dir-local-variable nil 'vv/project-corresponding-files vv/project-corresponding-files vv/project-corresponding-files-dir-locals-file)
        (save-buffer)))))

;;;###autoload
(defun aa/open-corresponding-project-todo (&optional prefix)
  (interactive "P")
  (find-file (ff/find-corresponding-project-file 'todo prefix)))

;;;###autoload
(defun aa/open-corresponding-project-notes ()
  (interactive "P")
  (find-file (ff/find-corresponding-project-file 'notes prefix)))

;;;###autoload
(defun aa/remove-corresponding-project-todo ()
  (interactive)
  (ff/remove-current-project-corresponding-file 'todo))

;;;###autoload
(defun aa/remove-corresponding-project-notes ()
  (interactive)
  (ff/remove-current-project-corresponding-file 'notes))

