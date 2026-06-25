;;; volatile.el -*- lexical-binding: t; -*-

;;;; beframe?
;; (after! consult
;;   (unless (featurep 'beframe)
;;     (require 'beframe))
;;   (defface beframe-buffer
;;     '((t :inherit font-lock-string-face))
;;     "Face for `consult' framed buffers.")
;;   (defvar beframe--consult-source
;;     `( :name     "Frame-specific buffers (current frame)"
;;        :narrow   ?F
;;        :category buffer
;;        :face     beframe-buffer
;;        :history  beframe-history
;;        :items    ,#'beframe--buffer-names
;;        :action   ,#'switch-to-buffer
;;        :state    ,#'consult--buffer-state))
;;   (add-to-list 'consult-buffer-sources 'beframe--consult-source))

;; (after! consult
;;   (defvar +consult-kill-buffer-source '(beframe--consult-source
;;                                         consult--source-hidden-buffer
;;                                         consult--source-modified-buffer
;;                                         consult--source-buffer
;;                                         consult--source-project-buffer-hidden))
;;   (defun +consult-kill-buffer ()
;;     (interactive)
;;     (let ((selected (consult--multi +consult-kill-buffer-source
;;                                     :prompt "Kill buffer: "
;;                                     :history 'consult--buffer-history
;;                                     :preview-key nil
;;                                     :sort nil)))
;;       (when (plist-get (cdr selected) :match)
;;         (kill-buffer (car selected))))))
;; (autoload '+consult-kill-buffer "consult" :type t)
;; (map! "C-x k" #'+consult-kill-buffer)

;;;; org-tree-slide
;; (after! org-tree-slide
;;   (advice-remove 'org-tree-slide--display-tree-with-narrow
;;                  #'+org-present--hide-first-heading-maybe-a)
;;   (defadvice! +org-present--hide-first-heading-maybe-a (fn &rest args)
;;     "Omit the first heading if `+org-present-hide-first-heading' is non-nil."
;;     :around #'org-tree-slide--display-tree-with-narrow
;;     (letf!
;;       (defun org-narrow-to-subtree (&optional element)
;;         "Narrow buffer to the current subtree."
;;         (interactive)
;;         (save-excursion
;;           (save-match-data
;;             (org-with-limited-levels
;;              (narrow-to-region
;;               (progn
;;                 (when (org-before-first-heading-p)
;;                   (org-next-visible-heading 1))
;;                 (org-back-to-heading t)
;;                 (when +org-present-hide-first-heading
;;                   (forward-line 1))
;;                 (point))
;;               (progn
;;                 (org-end-of-subtree t t)
;;                 (when (and (org-at-heading-p) (not (eobp)))
;;                   (backward-char 1))
;;                 (point)))))))
;;       (apply fn args))))

;;;; typst-mode
;; (use-package typst-ts-mode
;;   :custom
;;   (typst-ts-watch-options "--open")
;;   (typst-ts-mode-enable-raw-blocks-highlight t)
;;   :config
;;   (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

;;;; gptel
;; (after! gptel
;;   (setq gptel-expert-commands t)
;;   (setq gptel-default-mode 'org-mode)
;;   (gptel-make-openai "minimax"          ;Any name you want
;;     :host "api.minimax.chat"
;;     :endpoint "/v1/text/chatcompletion_v2"
;;     :stream t
;;     :key (gptel-api-key-from-auth-source "api.minimax.chat" "apikey")
;;     :models '(MiniMax-Text-01 abab6.5s-chat))
;;   (gptel-make-gemini "linkapi"
;;     :host "api.linkapi.ai"
;;     :key (gptel-api-key-from-auth-source "api.linkapi.ai" "apikey")
;;     :models +gptel--linkapi-gemini-models))

;;;; emmet
(after! emmet-mode
  (unbind-key "<tab>" emmet-mode-keymap)
  (map! :map emmet-mode-keymap
        "C-<tab>" #'+web/indent-or-yas-or-emmet-expand))
;;;; ekg
(use-package! ekg
  :config
  (setq ekg-notes-display-images nil))

;;;; telega
(use-package! telega
  :config
  (setq telega-server-libs-prefix "~/.nix-profile"))

;;;; org-translucion
;; ;;;###autoload
;; (defun ff/denote-org-transclusion-add (link plist)
;;   (when (string= "denote" (org-element-property :type link))
;;     (let* ((denote-full (org-element-property :path link)) ;; get denote id from denote:<denote-full> link
;;            (denote-parts (string-split denote-full "::"))
;;            (file-path (denote-get-path-by-id (car denote-parts))) ;; path resolved by the id
;;            (new-link (with-temp-buffer ;; create a [[file:/path/to/denote/note]] org link
;;                        (insert "[[file:") ;; and store it in 'new-link' variable
;;                        (insert file-path)
;;                        (when (cadr denote-parts)
;;                          (insert "::" (cadr denote-parts)))
;;                        (insert "]]")
;;                        (goto-char (point-min))
;;                        (org-element-link-parser))))
;;       (pcase (file-name-extension file-path)
;;         ("org" (org-transclusion-add-org-file new-link plist))
;;         ("md"  (org-transclusion-add-other-file new-link plist))))))
;;         
;; (after! org-transclusion
;;   (cl-pushnew 'ff/denote-org-transclusion-add
;;               org-transclusion-add-functions)
;;   (cl-pushnew 'keyword org-transclusion-exclude-elements))

;;;; mu4e load auth from auth-source
;; (add-to-list 'auth-sources 'pizauth)

;; (defvar pizauth-account-alist '(((:host "smtp.gmail.com" :user "xxxx@gmail.com") . "Gmail")))

;; (cl-defun auth-source-pizauth-search (&rest spec
;;                                             &key backend type host user port
;;                                             require max
;;                                             &allow-other-keys)
;;   (when-let ((account (cdr
;;                        (assoc
;;                         `(:host ,host :user ,user)
;;                         pizauth-account-alist))))
;;     (if-let ((secret (with-temp-buffer
;;                        (when (= 0 (call-process "pizauth" nil t nil "show" account))
;;                          (buffer-substring-no-properties (point-min) (- (point-max) 1))))))
;;         `(,(list :host host
;;                  :user user
;;                  :port port
;;                  :backend backend 
;;                  :type type
;;                  :secret secret
;;                  :smtp-auth 'xoauth2))
;;       `(,(list :host host
;;                :user user
;;                :port port
;;                :backend backend
;;                :type type
;;                :smtp-auth 'xoauth2)))))

;; (defun auth-source-backends-parser-pizauth (entry)
;;   (when (eq entry 'pizauth)
;;     (auth-source-backend
;;      :source ""
;;      :type 'pizauth
;;      :search-function #'auth-source-pizauth-search)))

;; (add-hook 'auth-source-backend-parser-functions #'auth-source-backends-parser-pizauth)

;;; org-yank hack
;; (defun org-yank-generic (command arg)
;;   "Perform some yank-like command.

;; This function implements the behavior described in the `org-yank'
;; documentation.  However, it has been generalized to work for any
;; interactive command with similar behavior."

;;   ;; pretend to be command COMMAND
;;   (setq this-command command)

;;   (if arg
;;       (call-interactively command)

;;     (let ((subtreep    ; is kill a subtree, and the yank position appropriate?
;; 	   (and (org-kill-is-subtree-p)
;; 		(or (bolp)
;; 		    (and (looking-at "[ \t]*$")
;; 			 (string-match
;; 			  "\\`\\*+\\'"
;;                           (buffer-substring (line-beginning-position) (point)))))))
;; 	  swallowp)
;;       (cond
;;        ((and subtreep org-yank-folded-subtrees)
;; 	(let ((beg (point))
;; 	      end)
;; 	  (if (and subtreep org-yank-adjusted-subtrees)
;; 	      (org-paste-subtree nil nil 'for-yank)
;; 	    (call-interactively command))

;; 	  (setq end (point))
;; 	  (goto-char beg)
;; 	  (when (and (bolp) subtreep
;; 		     (not (setq swallowp
;; 			        (org-yank-folding-would-swallow-text beg end))))
;; 	    (org-with-limited-levels
;; 	     (or (looking-at org-outline-regexp)
;; 		 (re-search-forward org-outline-regexp-bol end t))
;; 	     (while (and (< (point) end) (looking-at org-outline-regexp))
;; 	       (org-fold-subtree t)
;; 	       (org-cycle-show-empty-lines 'folded)
;; 	       (condition-case nil
;; 		   (outline-forward-same-level 1)
;; 		 (error (goto-char end))))))
;; 	  (when swallowp
;; 	    (message
;; 	     "Inserted text not folded because that would swallow text"))

;; 	  (goto-char end)
;; 	  (skip-chars-forward " \t\n\r")
;; 	  (forward-line 0)
;; 	  (push-mark beg 'nomsg)))
;;        ((and subtreep org-yank-adjusted-subtrees)
;;         (let ((beg (line-beginning-position)))
;; 	  (org-paste-subtree nil nil 'for-yank)
;; 	  (push-mark beg 'nomsg)))

;;        ((org-element-type-p (org-element-at-point) 'item)
;;         (let ((wait-paste (split-string (current-kill 0 'do-not-move) "[\n\r]+" t)))
;;           (while wait-paste
;;             (insert (pop wait-paste))
;;             (when wait-paste
;;               (org-newline-and-indent)))))
;;        (t
;; 	(call-interactively command))))))
