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
(defun +denote-random-note ()
  (interactive)
  (if-let* ((denotes (denote-directory-files nil t nil)))
      (find-file (seq-random-elt denotes))
    (user-error "No note found")))

;;;###autoload
(defun projectile-reset-current-project-cache ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (puthash project-root
             '()
             projectile-projects-cache)))

;;;###autoload
(defun projectile-reset-all-project-cache ()
  (interactive)
  (setq projectile-projects-cache (make-hash-table :test 'equal)))

;; Seriously? Maybe we could use super + 2
;;;###autoload
(defun launch-microsoft-todo ()
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

;;; unpackage
(defcustom unpackaged/lorem-ipsum-overlay-exclude nil
  "List of regexps to exclude from `unpackaged/lorem-ipsum-overlay'."
  :type '(repeat regexp))

;;;###autoload
(cl-defun unpackaged/lorem-ipsum-overlay (&key replace-p use-map-p)
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
