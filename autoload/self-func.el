;;; autoload/self-func.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ff/org-yank-image-with-denote-id-or-default ()
  (if-let ((id (denote-retrieve-filename-identifier (buffer-file-name))))
      (format (concat "C" id "-" (substring (org-id-uuid) 0 (min 8))))
    (org-yank-image-autogen-filename)))

;;;###autoload
(defun aa/insert-space-between-chinese-and-english ()
  "Insert space between Chinese and English text in each line of the region.
   Ignore lines starting with '#+'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1)
      (beginning-of-line)
      (when (not (looking-at "^\\s-*#\\+")) ; Ignore lines starting with #+
        (while (progn (beginning-of-line)
                      (re-search-forward "\\(?1:\\cC\\|\\cH\\|\\cK\\)\\(?2:[0-9A-Za-z]\\)\\|\\(?1:[0-9A-Za-z]\\)\\(?2:\\cC\\|\\cH\\|\\cK\\)" (line-end-position) t))
          (replace-match "\\1 \\2" nil nil))))))

;;;###autoload
(defun aa/denote-random-note ()
  (interactive)
  (if-let* ((denotes (denote-directory-files nil t nil)))
      (find-file (seq-random-elt denotes))
    (user-error "No note found")))

;; Seriously? Maybe we could use super + 2
;;;###autoload
(defun aa/launch-microsoft-todo ()
  "Launch Microsoft To-Do using gtk-launch."
  (interactive)
  (shell-command "gtk-launch chrome-jlhoajbaojeilbdnlldgecmilgppanbh-Default.desktop"))

;;;###autoload
(defun aa/replace-dashes-with-stars (n)
  "Replace dashes with stars at the beginning of each line.
The number of stars will be increased by N for each tab before the dash."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\t*\\)-" nil t)
      (replace-match (make-string (1+ (length (match-string 1))) ?*) t nil))))

(defun aa/replace-stars-with-dashes (n)
  "Replace stars with dashes at the beginning of each line.
The number of stars will be increased by N for each tab before the dash."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\t*\\)\*" nil t)
      (replace-match (make-string (1+ (length (match-string 1))) ?-) t nil))))

;;;###autoload
(defun aa/denote-rename-file-by-selecting (file)
  (interactive "fRename FILE Denote-style:")
  (let ((args (append (list file)
                      (denote--rename-get-file-info-from-prompts-or-existing file))))
    (apply #'denote-rename-file args)))

;;;###autoload
(defun aa/generate-denote-keyword-checklist ()
  (interactive)
  (if (eq major-mode #'org-mode)
      (letrec ((generate-checkbox (lambda (keyword)
                                    (org-element-interpret-data
                                     (org-element-create 'item
                                                         '(:bullet "-"
                                                           :pre-blank 0
                                                           :checkbox off)
                                                         keyword)))))
        (let* ((headline (org-element-create 'headline
                                             `(:level ,(org-current-level)
                                               :title "keyword checklist [/]")))
               (headline-text (org-element-interpret-data headline))
               (checkboxs (mapcar generate-checkbox (denote-keywords))))
          (save-excursion
            (insert headline-text)
            (mapc #'insert checkboxs))))
    (message "not working without org-mode")))

;;;###autoload
;; (defun generate-denote-keyword-todolist ()
;;   (interactive)
;;   (if (eq major-mode #'org-mode)
;;       (cl-flet* ((generate-element (level title &optional (todo-keyword "[ ]"))
;;                    (org-element-create 'headline
;;                                        (append `(:level ,level
;;                                                  :title ,title)
;;                                                (when todo-keyword
;;                                                  `(:todo-keyword ,todo-keyword
;;                                                    :todo-type 'todo)))))
;;                  (generate-checkbox (keyword)
;;                    (org-element-interpret-data
;;                     (generate-element (1+ (org-outline-level))
;;                                       keyword))))
;;         (save-excursion
;;           (insert (org-element-interpret-data (generate-element (org-outline-level)
;;                                                                 "keyword checklist [/]"
;;                                                                 nil)))
;;           (mapc #'insert (mapcar #'generate-checkbox (denote-keywords))))))
;;   (message "not working without org-mode"))
