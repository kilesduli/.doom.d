;;; autoload/self-func.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +generate--frame-format ()
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

;;;###autoload
(defun +org-yank-image-with-denote-id-or-default ()
  (if-let ((id (denote-retrieve-filename-identifier (buffer-file-name))))
      (format (concat "C" id "-" (substring (org-id-uuid) 0 (min 8))))
    (org-yank-image-autogen-filename)))

;;;###autoload
(defun insert-space-between-chinese-and-english ()
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

;; Seriously? Maybe we could use super + 2
;;;###autoload
(defun aa/launch-microsoft-todo ()
  "Launch Microsoft To-Do using gtk-launch."
  (interactive)
  (shell-command "gtk-launch chrome-jlhoajbaojeilbdnlldgecmilgppanbh-Default.desktop"))

;;;###autoload
(defun replace-dashes-with-stars (n)
  "Replace dashes with stars at the beginning of each line.
The number of stars will be increased by N for each tab before the dash."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\t*\\)-" nil t)
      (replace-match (make-string (1+ (length (match-string 1))) ?*) t nil))))

(defun replace-stars-with-dashes (n)
  "Replace stars with dashes at the beginning of each line.
The number of stars will be increased by N for each tab before the dash."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\t*\\)\*" nil t)
      (replace-match (make-string (1+ (length (match-string 1))) ?-) t nil))))

;;;###autoload
(defun +installed-from-nix-p (executable-path)
  (string-match-p ".nix-profile" executable-path))

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

;;; unpackage
(defcustom unpackaged/lorem-ipsum-overlay-exclude nil
  "List of regexps to exclude from `unpackaged/lorem-ipsum-overlay'."
  :type '(repeat regexp))

;;;###autoload
(cl-defun aa/unpackaged/lorem-ipsum-overlay (&key replace-p use-map-p)
  "Overlay all text in current buffer with \"lorem ipsum\" text.
When called again, remove overlays.  Useful for taking
screenshots without revealing buffer contents.

If REPLACE-P is non-nil (interactively, with prefix and prompt),
replace buffer contents rather than overlaying them.  When a
buffer is very large and would have so many overlays that
performance would be prohibitively slow, you may replace the
buffer contents instead.  (Of course, be careful about saving the
buffer after replacing its contents.)

If USE-MAP-P is non-nil (interactively, with prefix and prompt),
all instances of a real word are replaced with the same word;
otherwise, each instance of a real word is replaced with a random
word (further obscuring the text).

Each piece of non-whitespace text in the buffer is compared with
regexps in `unpackaged/lorem-ipsum-overlay-exclude', and ones
that match are not overlaid.  Note that the regexps are compared
against the entire non-whitespace token, up-to and including the
preceding whitespace, but only the alphabetic part of the token
is overlaid.  For example, in an Org buffer, a line that starts
with:

  #+TITLE: unpackaged.el

could be matched against the exclude regexp (in `rx' syntax):

  (rx (or bol bos blank) \"#+\" (1+ alnum) \":\" (or eol eos blank))

And the line would be overlaid like:

  #+TITLE: parturient.et"
  (interactive (when current-prefix-arg
                 (list :replace-p (yes-or-no-p "Replace contents (or just overlay)? ")
                       :use-map-p (yes-or-no-p "Map words (or be completely random)? "))))
  (require 'lorem-ipsum)
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :lorem-ipsum-overlay))
        ;; Remove overlays.
        (dolist (ov ovs)
          (when (overlay-get ov :lorem-ipsum-overlay)
            (delete-overlay ov)))
      ;; Add overlays.
      (let ((lorem-ipsum-words (--> lorem-ipsum-text
                                    (-flatten it) (apply #'concat it)
                                    (split-string it (rx (or space punct)) 'omit-nulls)))
            (case-fold-search nil)
            (map (make-hash-table :test #'equal)))
        (cl-labels ((overlay-group (group)
                      (let* ((beg (match-beginning group))
                             (end (match-end group))
                             (replacement-word (if use-map-p
                                                   (lorem-word* (match-string-no-properties group))
                                                 (lorem-word (match-string-no-properties group))))
                             (ov (make-overlay beg end)))
                        (when replacement-word
                          (overlay-put ov :lorem-ipsum-overlay t)
                          (overlay-put ov 'display replacement-word))))
                    (replace-group (group)
                      (let* ((beg (match-beginning group))
                             (end (match-end group))
                             (replacement-word (if use-map-p
                                                   (lorem-word* (match-string-no-properties group))
                                                 (lorem-word (match-string-no-properties group)))))
                        (when replacement-word
                          (setf (buffer-substring-no-properties beg end) replacement-word))))
                    (lorem-word (word)
                      (if-let* ((matches (lorem-matches (length word))))
                          (apply-case word (downcase (seq-random-elt matches)))
                        ;; Word too long: compose one.
                        (apply-case word (downcase (compose-word (length word))))))
                    (lorem-word* (word)
                      (or (gethash word map)
                          (puthash word
                                   (if-let ((matches (lorem-matches (length word))))
                                       (apply-case word (downcase (seq-random-elt matches)))
                                     ;; Word too long: compose one.
                                     (apply-case word (downcase (compose-word (length word)))))
                                   map)))
                    (lorem-matches (length &optional (comparator #'=))
                      (cl-loop for liw in lorem-ipsum-words
                               when (funcall comparator (length liw) length)
                               collect liw))
                    (apply-case (source target)
                      (cl-loop for sc across-ref source
                               for tc across-ref target
                               when (not (string-match-p (rx lower) (char-to-string sc)))
                               do (setf tc (string-to-char (upcase (char-to-string tc)))))
                      target)
                    (compose-word (length)
                      (cl-loop while (> length 0)
                               for word = (seq-random-elt (lorem-matches length #'<=))
                               concat word
                               do (cl-decf length (length word)))))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (rx (group (1+ (or bol bos blank (not alpha)))
                                                 (0+ (not (any alpha blank)))
                                                 (group (1+ alpha))
                                                 (0+ (not (any alpha blank)))))
                                      nil t)
              (unless (cl-member (match-string-no-properties 0) unpackaged/lorem-ipsum-overlay-exclude
                                 :test (lambda (string regexp)
                                         (string-match-p regexp string)))
                (if replace-p
                    (replace-group 2)
                  (overlay-group 2)))
              (goto-char (match-end 2)))))))))
;;; denote org transclusion
;;;###autoload
(defun denote-org-transclusion-add (link plist)
  (when (string= "denote" (org-element-property :type link))
    (let* ((denote-full (org-element-property :path link)) ;; get denote id from denote:<denote-full> link
           (denote-parts (string-split denote-full "::"))
           (file-path (denote-get-path-by-id (car denote-parts))) ;; path resolved by the id
           (new-link (with-temp-buffer ;; create a [[file:/path/to/denote/note]] org link
                       (insert "[[file:") ;; and store it in 'new-link' variable
                       (insert file-path)
                       (when (cadr denote-parts)
                         (insert "::" (cadr denote-parts)))
                       (insert "]]")
                       (goto-char (point-min))
                       (org-element-link-parser))))
      (pcase (file-name-extension file-path)
        ("org" (org-transclusion-add-org-file new-link plist))
        ("md"  (org-transclusion-add-other-file new-link plist))))))


;;; denote org backlinks
(defcustom +denote-org-backlinks-line-format-type 'all
  "empty"
  :type '(choice (const :tag "nil" nil)
          (const :tag "all line" all)
          (const :tag "only-heading" only-heading)))

(defun +denote-org--backlinks-xref (identifier)
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

(defun +denote-org--remove-duplicate-lines (xrefs)
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

(defun +denote-org--backlinks-rewrite-xrefs-summary (xrefs)
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
                  (--> (mapcar #'+denote-org--fix-level text)
                       (mapcar #'(lambda (str) (+denote-org--fix-denote-link str start)) it)
                       (mapconcat #'identity it "\n")))))))))

(defun +denote-org--fix-level (str)
  (prog1 str
    (when (string-match org-outline-regexp-bol str)
      (let ((level (1- (- (match-end 0) (match-beginning 0)))))
        (add-text-properties 0 (length str) `(face ,(intern (format "org-level-%d" level))) str)
        (add-text-properties 0 (length str) `(+denote-query-outline-level ,(1+ level)) str)))))

(defun +denote-org--fix-denote-link (str start)
  (prog1 str
    (let ((item-text-props (list 'mouse-face 'highlight
                                 'keymap xref--button-map
                                 'help-echo
                                 (concat "mouse-2: display in another window, "
                                         "RET or mouse-1: follow reference"))))
      (when (and (length> str start) (string-match "\\[\\[denote:[^]]+\\]\\[[^]]+\\]\\]" str (1- start)))
        (add-text-properties (match-beginning 0) (match-end 0) item-text-props str)))))

;;;###autoload
(defun +denote-org-backlinks (&optional file)
  (interactive)
  (let* ((inhibit-read-only t))
    (if-let* ((current-file (or file (buffer-file-name))))
        (let* ((identifier (denote-retrieve-filename-identifier current-file))
               (xref-alist (--> (+denote-org--backlinks-xref identifier)
                                (+denote-org--remove-duplicate-lines it)
                                (+denote-org--backlinks-rewrite-xrefs-summary it)
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
            (+denote-org--xref-insert-xrefs xref-alist))
          (display-buffer buf-name))
      (user-error "Buffer `%s' is not associated with a file" (current-buffer)))))

(defun +denote-org--xref-insert-xrefs (xref-alist)
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
               (unless (eq +denote-org-backlinks-line-format-type 'nil)
                 (setq summary (--> (string-split summary "\n")
                                    (mapcar (let ((lc line)
                                                  (lc-origin line))
                                              (lambda (line)
                                                (let ((lf (cond
                                                           ((eq +denote-org-backlinks-line-format-type 'all)
                                                            (substring line-format))
                                                           ((eq +denote-org-backlinks-line-format-type 'only-heading)
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
