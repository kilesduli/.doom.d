;;; autoload/denote-backlinks.el -*- lexical-binding: t; -*-

(defcustom vv/denote-org-backlinks-line-format-type 'all
  "empty"
  :type '(choice (const :tag "nil" nil)
          (const :tag "all line" all)
          (const :tag "only-heading" only-heading)))

(defun ff/denote-org--backlinks-xref (identifier)
  (when-let* ((files (denote-directory-files))
              (backlinks (--> (denote--get-all-backlinks files)
                              (gethash identifier it)
                              (denote--get-files-by-file-type it)
                              (gethash 'org it)
                              (delete-dups it)))
              (format-parts (split-string
                             (denote--link-retrieval-format 'org)
                             "%VALUE%"))
              (query-simple (concat
                             (regexp-quote (nth 0 format-parts))
                             (regexp-quote identifier)
                             (regexp-quote (nth 1 format-parts)))))
    (xref-matches-in-files query-simple backlinks)))

(defun ff/denote-org--remove-duplicate-lines (xrefs)
  (cl-remove-duplicates xrefs
                        ;; xref will sorted by line and column, so we keep the one with smaller column.
                        :from-end t
                        :test
                        (lambda (a b)
                          (let ((a-loc (xref-match-item-location a))
                                (b-loc (xref-match-item-location b)))
                            (and (equal (xref-file-location-file a-loc)
                                        (xref-file-location-file b-loc))
                                 (equal (xref-file-location-line a-loc)
                                        (xref-file-location-line b-loc)))))))

(defun ff/denote-org--backlinks-rewrite-xrefs-summary (xrefs)
  (with-temp-buffer
    (org-mode)
    (dolist (xref xrefs xrefs)
      (pcase-let* ((location (xref-match-item-location xref))
                   ((cl-struct xref-file-location file line column) location))
        (erase-buffer)
        (goto-char (point-min))
        (insert-file-contents file)
        (beginning-of-line line)
        (forward-char column)
        (when (org-at-heading-p)
          (let ((text (--> (buffer-substring-no-properties (org-entry-beginning-position)
                                                           (save-excursion (org-end-of-subtree t) (point)))
                           (split-string it "\n")))
                (start column))
            (setf (xref-match-item-summary xref)
                  (--> (mapcar #'ff/denote-org--fix-level text)
                       (mapcar #'(lambda (str) (ff/denote-org--fix-denote-link str start)) it)
                       (mapconcat #'identity it "\n")))))))))

(defun ff/denote-org--fix-level (str)
  (prog1 str
    (when (string-match org-outline-regexp-bol str)
      (let ((level (1- (- (match-end 0) (match-beginning 0)))))
        (add-text-properties 0 (length str) `(face ,(intern (format "org-level-%d" level))) str)
        (add-text-properties 0 (length str) `(+denote-query-outline-level ,(1+ level)) str)))))

(defun ff/denote-org--fix-denote-link (str start)
  (prog1 str
    (let ((item-text-props (list 'mouse-face 'highlight
                                 'keymap xref--button-map
                                 'help-echo
                                 (concat "mouse-2: display in another window, "
                                         "RET or mouse-1: follow reference"))))
      (when (and (length> str start) (string-match "\\[\\[denote:[^]]+\\]\\[[^]]+\\]\\]" str (1- start)))
        (add-text-properties (match-beginning 0) (match-end 0) item-text-props str)))))

;;;###autoload
(defun aa/denote-org-backlinks (&optional file)
  (interactive)
  (let* ((inhibit-read-only t))
    (if-let* ((current-file (or file (buffer-file-name))))
        (let* ((identifier (denote-retrieve-filename-identifier current-file))
               (xref-alist (--> (ff/denote-org--backlinks-xref identifier)
                                (ff/denote-org--remove-duplicate-lines it)
                                (ff/denote-org--backlinks-rewrite-xrefs-summary it)
                                (xref--analyze it)))
               (buf-name (denote--backlinks-get-buffer-name current-file identifier)))
          (unless xref-alist
            (error "No results to display"))
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (denote-query-mode)
            (setq-local outline-search-function
                        (lambda (&optional bound move backward looking-at)
                          (outline-search-text-property
                           '+denote-query-outline-level nil bound move backward looking-at)))
            (setq-local outline-level
                        (lambda ()
                          (save-excursion
                            (get-text-property (point) '+denote-query-outline-level))))
            (ff/denote-org--xref-insert-xrefs xref-alist))
          (display-buffer buf-name))
      (user-error "Buffer `%s' is not associated with a file" (current-buffer)))))

(defun ff/denote-org--xref-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...), where
GROUP is a string for decoration purposes and XREF is an
`xref-item' object."
  (require 'compile)                    ; For the compilation faces.
  (setq xref-num-matches-found 0)
  (cl-loop for (group . xrefs) in xref-alist
           for max-line = (cl-loop for xref in xrefs
                                   maximize (xref-location-line
                                             (xref-item-location xref)))
           for line-format = (and max-line
                                  (format
                                   #("%%%dd:" 0 4 (face xref-line-number) 5 6 (face shadow))
                                   (1+ (floor (log max-line 10)))))
           with prev-group = nil
           with prev-line = nil
           do
           (unless (string-prefix-p "/" group)
             (setq group (concat "--|" group)))
           (xref--insert-propertized '(face xref-file-header xref-group t +denote-query-outline-level 1)
                                     group "\n")
           (dolist (xref xrefs)
             (cl-incf xref-num-matches-found)
             (pcase-let* (((cl-struct xref-item summary location) xref)
                          ((cl-struct xref-file-location file line column) location))
               (unless (eq vv/denote-org-backlinks-line-format-type 'nil)
                 (setq summary (--> (string-split summary "\n")
                                    (mapcar (let ((lc line)
                                                  (lc-origin line))
                                              (lambda (line)
                                                (let ((lf (cond
                                                           ((eq vv/denote-org-backlinks-line-format-type 'all)
                                                            (substring line-format))
                                                           ((eq vv/denote-org-backlinks-line-format-type 'only-heading)
                                                            (if (> lc lc-origin)
                                                                ;; line-format has ':', so we need add two here
                                                                (make-string (+ 2 (floor (log max-line 10))) ?\s)
                                                              (substring line-format))))))
                                                  (when-let* ((level (get-text-property 0 '+denote-query-outline-level line)))
                                                    (add-text-properties 0 (length lf) `(+denote-query-outline-level ,level) lf))
                                                  (concat (format lf (cl-incf lc))
                                                          line))))
                                            it)
                                    (mapconcat #'identity it "\n"))))
               (when (and (equal prev-group group)
                          (or (null line)
                              (not (equal prev-line line))))
                 (insert "\n"))
                 (xref--insert-propertized (list 'xref-item xref)
                                           summary)
                 (setq prev-line line
                       prev-group group)))
           (insert "\n"))
  (add-to-invisibility-spec '(ellipsis . t))
  (save-excursion
    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (xref--apply-truncation)))
  (run-hooks 'xref-after-update-hook))
