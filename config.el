;;; $DOOMDIR/config.el --- duli's doom emacs config -*- lexical-binding: t; -*-
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;

;; Function Varible named rules
;; - `aa/' is for interactive function
;; - `ff/' is for non interactive function
;; - `vv/' is for defvar defvar-local
;; - 'mm/' is for macro

;;; Hack:
;;;; benchmark init
(use-package! benchmark-init
  :unless (daemonp)
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'doom-first-input-hook 'benchmark-init/deactivate))

;;;; use menu key as <hyper>
(keymap-global-unset "<menu>" 'remove)
(keymap-set function-key-map "<menu>" #'event-apply-hyper-modifier)

;;;; default emacs newline
;; We wanna default emacs behaviour. Use M-j add new comment line.
;; This is more compatible
(advice-remove 'newline-and-indent #'+default--newline-indent-and-continue-comments-a)
(keymap-global-unset "<remap> <newline>")

;;;; doom-hacks fix
(add-hook! doom-after-init
  (setq gc-cons-threshold (* 16 1024 1024))
  (setq gc-cons-percentage 0.2))

;;;; unicode fonts
;; Emacs will read LANG env to decide what fonts add to frame.
;; and this function will compute hash according frame font. so...
(after! unicode-fonts
  (advice-add 'unicode-fonts--configuration-checksum
              :around #'(lambda (old-fun &rest args)
                          (cl-letf* (((symbol-function 'font-utils-list-names)
                                      (lambda () '())))
                            (apply old-fun args)))))

;;;; fix java .project file is being detected project in doom
(after! projectile
  (setq projectile-project-root-files-bottom-up '(".ccls-root" ".projectile"
                                                  ".git" ".hg"))
  (advice-remove #'projectile-dirconfig-file #'doom--projectile-dirconfig-file-a))

;;;; xft, if we not using cairo, emoji fix
;; Fallback to xft, if we not using cairo
(when (boundp 'xft-ignore-color-fonts)
  (setq xft-ignore-color-fonts nil
        face-ignored-fonts nil)
  (set-fontset-font t 'emoji
                    '("Noto Color Emoji" . "iso10646-1") nil 'prepend))

;;;; org block support
(eval-after-load 'ob-core
  (lambda ()
    (advice-patch 'org-babel-hide-result-toggle
                  '(or
                    (memq t
                          (mapcar
                           (lambda (overlay)
                             (eq (overlay-get overlay 'invisible)
                                 'org-babel-hide-result))
                           (overlays-at start)))
                    (eq force 'off))
                  '(memq t
                    (mapcar
                     (lambda (overlay)
                       (eq (overlay-get overlay 'invisible)
                           'org-babel-hide-result))
                     (overlays-at start))))))

(advice-add 'org-cycle-set-startup-visibility
            :after
            (lambda ()
              (unless (eq org-startup-folded 'showeverything)
                (when vv/org-cycle-hide-result-startup
                  (org-babel-result-hide-all))
                (ff/block-visibility-according-to-property))))

;;; Function in autoload
;;;###autoload
(setq frame-title-format '(:eval (ff/generate--frame-format)))
(keymap-global-set "C-M-j" #'aa/duplicate-line)

;;;; compile @f advice and compile
;; let default empty
(setopt compile-command "")
(setq savehist-ignored-variables '(compile-history))

(advice-add 'compile :before #'(lambda (&rest r) (setq vv/compile-interactive-flag (called-interactively-p 'interactive))))
(advice-add 'recompile :before #'(lambda (&rest r) (setq vv/compile-interactive-flag (called-interactively-p 'interactive))))
(advice-add 'compilation-start :filter-args #'ff/compile-expand-placeholders)

;;; Misc:
;; I thought it is useful.
(setopt show-trailing-whitespace t)

(setq doom-font (font-spec :family "MonoLisa duli Modified" :weight 'regular :size 32)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
      doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 32)
      doom-symbol-font (font-spec :family "LXGW Wenkai")
      doom-serif-font (font-spec :family "CMU Typewriter Text" :weight 'light :size 32))

(setq user-full-name "duli kiles"
      user-mail-address "duli4868@gmail.com")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil
      cursor-in-non-selected-windows t
      truncate-lines nil
      scroll-preserve-screen-position 'always
      scroll-margin 0
      scroll-conservatively 101
      word-wrap-by-category t
      delete-by-moving-to-trash t
      display-line-numbers-type 'relative)

(after! projectile
  (setq projectile-files-cache-expire 300))

;; make height higher
(set-popup-rule!
  '"^\\*compilation"
  :vslot -2 :size 0.36 :autosave t :quit t :ttl 0)

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)
(remove-hook 'doom-first-buffer-hook #'global-flycheck-mode)
(smartparens-global-mode -1)
(global-flycheck-mode -1)
(pixel-scroll-precision-mode 1)

(put 'narrow-to-region 'disabled nil)

;;;; fullscreen setup
(if (daemonp)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;;;; theme
(setq doom-theme 'modus-operandi-tritanopia
      doom-modeline-height 44)

(add-hook! doom-load-theme
  (custom-theme-set-faces 'user
                          `(font-lock-builtin-face ((t (:foreground "#d73a49"))))
                          `(font-lock-comment-face ((t (:foreground "#6a737d"))))
                          `(font-lock-comment-delimiter-face ((t (:foreground "#6a737d"))))
                          `(font-lock-constant-face ((t (:foreground "#005cc5"))))
                          `(font-lock-doc-face ((t (:foreground "#032f62"))))
                          `(font-lock-function-name-face ((t (:foreground "#6f42c1"))))
                          `(font-lock-keyword-face ((t (:foreground "#d73a49"))))
                          `(font-lock-negation-char-face ((t (:foreground "#d73a49"))))
                          `(font-lock-preprocessor-face ((t (:foreground "#d73a49"))))
                          `(font-lock-regexp-grouping-construct ((t (:foreground "#d73a49"))))
                          `(font-lock-regexp-grouping-backslash ((t (:foreground "#6a737d"))))
                          `(font-lock-string-face ((t (:foreground "#032f62"))))
                          `(font-lock-type-face ((t (:foreground "#005cc5"))))
                          `(font-lock-variable-name-face ((t (:foreground "#24292e"))))
                          `(font-lock-warning-face ((t (:foreground "#24292e"))))
                          `(org-date ((t (:foreground "#24292e" :underline t))))
                          `(org-document-title ((t (:foreground "#24292e"))))
                          `(org-block ((t (:background unspecified))))
                          `(org-block-begin-line ((t (:background unspecified))))
                          `(org-block-end-line ((t (:background unspecified))))))
;; org level face
(add-hook! doom-load-theme
  (apply #'custom-theme-set-faces
         'user
         (cons
          '(org-list-dt ((t (:inherit nil :foreground "#004f5f"))))
          (cl-loop for i from 1 to 8
                   collect (list (intern (format "org-level-%d" i))
                                 `((t (:inherit ,(intern (format "modus-themes-heading-%d" i))
                                       :extend t
                                       :weight normal))))))))

;;;; undo-limit
(after! undo-fu
  (setq undo-limit (* 8 1024 1024))
  (setq undo-strong-limit (* 16 1024 1024))
  (setq undo-outer-limit (* 36 1024 1024)))

;;;; which-key delay
(after! which-key
  (setq which-key-idle-delay 0.01)
  (setq which-key-idle-secondary-delay 0.01))

;;;; keymap set (C-c b, workspaces, ....)
(keymap-global-set "C-<mouse-wheel-up-event>"  'text-scale-increase)
(keymap-global-set "C-<mouse-wheel-down-event>" 'text-scale-decrease)
(map!
 (:when (modulep! :ui workspaces)
   :g "<f1>"   #'+workspace/switch-to-0
   :g "<f2>"   #'+workspace/switch-to-1
   :g "<f3>"   #'+workspace/switch-to-2
   :g "<f4>"   #'+workspace/switch-to-3
   :g "<f5>"   #'+workspace/switch-to-4))

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
       :desc "Previous buffer"             "["   #'previous-buffer
       :desc "Next buffer"                 "]"   #'next-buffer
       (:when (modulep! :ui workspaces)
         :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
         :desc "Switch buffer"           "B" #'switch-to-buffer)
       (:unless (modulep! :ui workspaces)
         :desc "Switch buffer"           "b" #'switch-to-buffer)
       :desc "Clone buffer"                "c"   #'clone-indirect-buffer
       :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "Set bookmark"                "m"   #'bookmark-set
       :desc "Delete bookmark"             "M"   #'bookmark-delete
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "New empty buffer"            "N"   #'+default/new-buffer
       :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Swap buffer"                 "s"   #'ace-swap-window
       :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'scratch-buffer
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers))

(map! :map doom-leader-file-map
      "o" #'find-file-other-window)

;;;; emacs-rime
;; emacs do not provide us a way to make keybinding live all over the time, but
;; use-package does. and don't need define a new minor mode. found in
;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
;; We could use use-package:bind-key*
;;
;; Now it is default C-\, there no needed.

;; rime setting
(use-package rime
  :bind
  ;; make delete words works
  (:map rime-mode-map
        ("S-<delete>" . 'rime-send-keybinding))
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.local/share/emacs-rime")
  (rime-share-data-dir "~/.local/share/rime-data")
  (rime-show-candidate 'posframe))

(after! rime
  (when (daemonp)
    (unless rime--lib-loaded
      (unless (file-exists-p rime--module-path)
        (rime-compile-module))
      (rime--load-dynamic-module))))

;;;; +utils and something
(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

(use-package! wakatime-mode
  :config
  (global-wakatime-mode))

(use-package! indent-bars
  :hook ((python-mode yaml-mode) . indent-bars-mode))

(after! doom-modeline
  (setopt doom-modeline-env-enable-python nil))

;;;; cns
(use-package! cns
  :config
  (setq cns-process-type 'network)
  (setq cns-client-host "127.0.0.1")
  (setq cns-recent-segmentation-limit 20) ; default is 10
  (setq cns-debug nil) ; disable debug output, default is t
  (setq cns-client-port 7878)
  :hook
  (find-file . cns-auto-enable))

;;;; outli
(use-package outli
  :hook ((prog-mode text-mode) . outli-mode)
  :bind
  (:map outli-mode-map
        ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading)))))


;;;; mail
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(setq send-mail-function #'smtpmail-send-it)
(setq message-send-mail-function #'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-stream-type 'starttls)
(setq smtpmail-smtp-user "duli4868@gmail.com")

(setq smtpmail-auth-supported '(xoauth2 cram-md5 plain login))
(setq smtpmail-servers-requiring-authorization ".*")

(defun aa/copy-gmail-oauth2-access-key ()
  (interactive)
  (when (= 1 (shell-command "pgrep -f 'pizauth server'"))
    (shell-command "pizauth server && cat ~/.config/gmail.auth | pizauth restore"))
  (kill-new (shell-command-to-string "pizauth show dulikiles"))
  (message "Copyed from pizauth show"))

;;; Org-mode
;;;; org
(after! org
  (setq org-directory "~/documents/notes"

        org-id-method 'ts ;; uuid is not human friendly
        org-id-ts-format "%Y%m%dT%H%M%S%2N"
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-cycle-separator-lines 2

        ;; yank-media
        org-yank-image-save-method (concat org-directory "/assets")
        org-yank-image-file-name-function #'ff/org-yank-image-with-denote-id-or-default

        org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
                             "|" "DONE(d)" "KILL(k)" "CANCEL(c)")
                            (sequence "[ ](b)" "[-](-)" "[?](?)" "|" "[X](x)")
                            (sequence "YEAR FLAG(y)" "CONTINUOUS FLAG(f)" "|" "MISSION COMPLETE"))

        ;; org-latex
        org-preview-latex-default-process 'xdv2svg
        org-latex-packages-alist '(("" "amssymb" t ("xelatex"))
                                   ("" "amsmath" t ("xelatex")))
        org-preview-latex-process-alist '((xdv2svg
                                           :programs ("xelatex" "dvisvgm")
                                           :description "xdv > svg"
                                           :message "you need to install the programs: latex and dvisvgm."
                                           :image-input-type "xdv"
                                           :image-output-type "svg"
                                           :image-size-adjust (1.5 . 1.5)
                                           :latex-compiler ("xelatex -interaction nonstopmode -no-pdf -output-directory %o %f")
                                           :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")))
        org-agenda-inhibit-startup t
        ;; much useful
        org-log-done 'time)
  ;; org-latex
  (add-hook! org-mode #'org-fragtog-mode)

  (after! ox-latex
    (setq org-latex-pdf-process '("tectonic -X compile --outdir=%o -Z shell-escape -Z continue-on-errors %f")))

  ;; org company-dabbrev
  (set-company-backend! 'org-mode
    'company-dabbrev 'company-yasnippet)
  (setq-hook! org-mode
    company-dabbrev-char-regexp "[\\.0-9a-zA-Z-_'/]")

  (setq-hook! org-mode
    ;; display-line-numbers-mode 0
    line-spacing 0.1)

  ;; open file in org-mode
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file-other-window)

  ;; open file:path way defintion
  (setf (cdr (assoc "\\.pdf\\'" org-file-apps)) "sioyek %s")
  (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "fixGL sioyek %s --page %1"))

  ;; disable org-agenda dynamic add and remove
  (put 'org-agenda-file-to-front 'disabled t)
  (put 'org-remove-file 'disabled t)

  ;; set org-encrypt key
  (setq org-crypt-key "01FC4F4457FB76A78E17C1CEDDF3E86600E4955A"))

;;;; denote with org-capture
(use-package! denote
  :config
  (setq denote-directory (concat (getenv "HOME") "/documents/notes/")
        denote-dired-directories (list denote-directory)
        denote-sort-keywords nil
        denote-known-keywords '("emacs" "note" "game" "refile" "rewrite")
        denote-prompts '(keywords title)
        denote-file-name-slug-functions  `((title . denote-sluggify-title)
                                           (keyword . ,(lambda (str) (downcase str))) ; make multi word keyword works
                                           (signature . denote-sluggify-signature)))
  )
(denote-rename-buffer-mode t)

(map! :map org-mode-map
      "C-c n r" #'denote-rename-file)

(map! :leader
      :prefix "n"
      :desc "denote-dired-default" "d" #'(lambda ()
                                           (interactive)
                                           (denote-sort-dired nil 'identifier t nil))
      :desc "denote-dired"         "D" #'denote-sort-dired)

(use-package! denote-journal
  :config
  (setq denote-journal-title-format "%B%e %Y %A"))

(defun ff/denote-journal-mdy-title-format-with-env (&optional r)
  (let ((system-time-locale "en_US.UTF-8"))
    (format-time-string "%B %d, %Y" (if r (date-to-time r) nil))))
(advice-add #'denote-journal-daily--title-format :override #'ff/denote-journal-mdy-title-format-with-env)

(keymap-global-set "H-c" #'aa/capture-note)
(keymap-global-set "H-=" #'(lambda (arg)
                             (interactive "P")
                             (let (denote-rename-confirmations)
                               (funcall #'denote-rename-file-signature))))


(cl-defun ff/org-capture-with-metadata (&key title keywords directory date template signature)
  (pcase-let* ((`(,title ,keywords _ ,directory ,date ,identifier ,template ,signature)
                (denote--creation-prepare-note-data title keywords 'org directory date nil template signature))
               (front-matter (denote--format-front-matter title date keywords identifier signature 'org))
               (template-string (cond ((stringp template) template)
                                      ((functionp template) (funcall template))
                                      (t (user-error "Invalid template")))))
    (setq denote-last-path
          (denote-format-file-name directory identifier keywords title ".org" signature))
    (when (file-regular-p denote-last-path)
      (user-error "A file named `%s' already exists" denote-last-path))
    (denote--keywords-add-to-history keywords)
    (concat front-matter template-string denote-org-capture-specifiers)))

(defun aa/capture-note (&optional goto)
  (interactive "P")
  (dlet ((org-capture-templates '(("N" "New capture note (with Denote)" plain (file denote-last-path)
                                   #'(lambda ()
                                       (let ((denote-org-capture-specifiers "%?"))
                                         (ff/org-capture-with-metadata
                                          :directory (concat (expand-file-name org-directory) "/capture")
                                          :keywords '("unorganized"))))
                                   :no-save t
                                   :immediate-finish nil
                                   :kill-buffer t
                                   :jump-to-captured t))))
    (org-capture goto "N")))

(defun aa/turn-unorganized-to-journal (&optional op)
  (interactive "P")
  (when (denote-file-has-denoted-filename-p (buffer-file-name))
    (let (denote-rename-confirmations)
      (when-let ((file (denote-rename-file (buffer-file-name)
                                           (ff/denote-journal-mdy-title-format-with-env (denote-extract-id-from-string (buffer-file-name)))
                                           '("journal")
                                           'keep-current
                                           'keep-current
                                           'keep-current))
                 (new-path (string-replace "capture" "journal" file)))
        (rename-file file new-path)
        (set-visited-file-name new-path t t)))))

;; TODO
(defun aa/visit-whole-day-journal (&optional op)
  (interactive "P"))

;; Disable C-, to prevent accidental triggers due to its proximity to C-k.
(add-hook 'org-capture-mode-hook
          (lambda ()
            (keymap-local-unset "C-,")
            (keymap-local-unset "C-.")))

(defvar vv/denote-link-display-help-echo nil)
;; fix help-echo
(defun ff/denote-link-ol-help-echo (_window _object position)
  "Echo the full file path of the identifier at POSITION."
  (when vv/denote-link-display-help-echo
    (let* ((data (denote--link-at-point-get-data position))
           (target (caar data)))
      (denote-get-path-by-id target))))
(advice-add #'denote-link-ol-help-echo :override #'ff/denote-link-ol-help-echo)

(use-package denote-menu
  :bind
  (:map denote-menu-mode-map
        ("c"   . #'denote-menu-clear-filters)
        ("/ r" . #'denote-menu-filter)
        ("/ k" . #'denote-menu-filter-by-keyword)
        ("/ o" . #'denote-menu-filter-out-keyword)
        ("e"   . #'denote-menu-export-to-dired)))

;; (map! :map doom-leader-notes-map
;;       "r" #'+denote-random-note)

(defvar +denote-todo-file (concat denote-directory "19700102T000000--todo__denote_todo.org"))

(after! org-capture
  (setq org-capture-templates
        '(("n" "New note (with Denote)" plain (file denote-last-path)
           #'denote-org-capture
           :no-save t
           :immediate-finish nil
           :kill-buffer t
           :jump-to-captured t)
          ("t" "Personal todo" entry (file+headline +denote-todo-file "Inbox")
           "* [ ] %?" :prepend t)
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline (lambda () (ff/find-corresponding-project-file 'todo nil)) "Inbox [%]")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline (lambda () (ff/find-corresponding-project-file 'notes nil)) "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t))))

(setq org-agenda-files '("~/documents/notes/" "~/documents/notes/journal/"))
(setq org-roam-directory (concat (getenv "HOME") "/roam"))

;;; Programming
;;;; company and orderless
(after! company
  (setq company-idle-delay 0.05)
  (setq company-tooltip-idle-delay 0)
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection))

(after! orderless
  (setq orderless-component-separator "[ &·]+"))

;;;; lang: csharp
(after! csharp-mode
  (add-hook! csharp-mode
    (c-set-offset 'func-decl-cont 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'topmost-intro-cont 0)))


(add-hook! '(scheme-mode-hook emacs-lisp-mode-hook lisp-mode-hook) #'paredit-mode)

;;;; clang-format+
(use-package! clang-format+
  :hook (c-mode-common . clang-format+-mode)
  :config
  (setq clang-format+-context 'modification
        clang-format+-always-enable t))

;;;; lang: rust
;; no more rustic-mode
(use-package! rust-mode
  :defer t
  :config
  (setq rust-indent-method-chain t)
  :init
  (add-hook 'rust-mode-local-vars-hook #'lsp! 'append)
  (when  (modulep! :tools lsp -eglot)
    (defadvice! +rust--dont-cache-results-from-ra-a (&rest _)
      :after #'lsp-eldoc-function
      (when (derived-mode-p 'rust-mode 'rust-ts-mode)
        (setq lsp--hover-saved-bounds nil))))
  (after! lsp-rust
    (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
      (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
             (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
             (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                              ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                              (t nil)))
             (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
             (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                              ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                              (t (-first-item groups))))
             (sig (->> sig-group
                       (--drop-while (s-equals? "```rust" it))
                       (--take-while (not (s-equals? "```" it)))
                       (--map (s-replace-regexp "//.*" "" it))
                       (--map (s-trim it))
                       (s-join " "))))
        (lsp--render-element (concat "```rust\n" sig cmt "\n```"))))))

;;;; lsp-mode (modulep lsp)
(setq lsp-use-plists t)
(when (modulep! :tools lsp -eglot)
  (after! lsp-pyright
    (setq lsp-pyright-use-library-code-for-types t
          lsp-pyright-stub-path (concat (getenv "HOME") "/clone/python-type-stubs")
          lsp-pyright-langserver-command "basedpyright"))

  (after! lsp-rust
    (setq lsp-rust-all-features t
          lsp-rust-all-targets t
          lsp-rust-features "all"
          lsp-rust-analyzer-checkonsave-features nil
          lsp-rust-analyzer-completion-term-search-enable t
          ;; lsp-rust-completion-full-function-signatures t
          lsp-rust-analyzer-lru-capacity 1024
          lsp-rust-analyzer-diagnostics-enable nil
          lsp-rust-analyzer-rustfmt-rangeformatting-enable t))

  (after! lsp-mode
    (setq lsp-enable-file-watchers nil
          lsp-keep-workspace-alive nil
          lsp-enable-symbol-highlighting nil
          lsp-auto-guess-root t
          lsp-modeline-code-actions-enable nil
          lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-segments '(symbols)
          lsp-headerline-breadcrumb-enable-diagnostics nil
          lsp-enable-indentation t
          lsp-modeline-diagnostics-enable nil
          lsp-eldoc-enable-hover t
          lsp-enable-snippet nil
          lsp-log-io nil
          lsp-ui-sideline-enable nil

          lsp-clients-clangd-args '("--background-index"
                                    "--clang-tidy"
                                    "--completion-style=detailed"
                                    "--header-insertion=never"
                                    "--header-insertion-decorators=0")
          lsp-diagnostics-disabled-modes '(rustic-mode rust-mode rust-ts-mode go-mode go-ts-mode)
          lsp-signature-auto-activate nil)

    (setq +lsp-company-backends '(company-yasnippet :separate company-capf))))

;;;; eglot (modulep lsp +eglot)
(when (modulep! :tools lsp +eglot)
  (after! eglot
    (setopt eglot-extend-to-xref t)
    (setf (alist-get '(csharp-mode csharp-ts-mode) eglot-server-programs nil nil #'equal)
          '("roslyn-language-server" "--stdio"))
    (setf (alist-get '(rust-ts-mode rust-mode) eglot-server-programs nil nil #'equal)
          `("rust-analyzer" :initializationOptions
            ,(alist-get :rust-analyzer eglot-workspace-configuration))))
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer . (:cargo (:allFeatures t :allTargets t :features "full")
                                     :checkOnSave :json-false
                                     :completion (:termSearch (:enable t)
                                                  :fullFunctionSignatures (:enable t))
                                     :hover (:memoryLayout (:size "both")
                                             :show (:traitAssocItems 5)
                                             :documentation (:keywords (:enable :json-false)))
                                     :inlayHints( ;:bindingModeHints (:enable t)
                                                 :lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
                                                 :closureReturnTypeHints (:enable "always")
                                                 :discriminantHints (:enable t)
                                                 :genericParameterHints (:lifetime (:enable t)))
                                     :semanticHighlighting (:operator (:specialization (:enable t))
                                                            :punctuation (:enable t :specialization (:enable t)))
                                     :workspace (:symbol (:search (:kind "all_symbols"
                                                                   :scope "workspace_and_dependencies")))
                                     :lru (:capacity 1024)
                                     :diagnostics (:enable :json-false)
                                     :rustfmt (:rangeFormatting (:enable t))))
                  (:java :autobuild (:enabled :json-false)))))

;;;; formatter and template and automode alist
(set-formatter! 'rustfmt '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2024") :modes '(rust-mode))
(set-formatter! 'nixpkgs-fmt '("nixpkgs-fmt") :modes '(nix-mode))
(set-file-template! "\\.h$" :trigger "__h" :mode 'c-mode)


;;;; :lang lisp (racket, common lisp)
(add-to-list 'auto-mode-alist (cons "\\.zuo\\'" 'racket-mode))
(add-to-list 'auto-mode-alist (cons ".sbclrc" 'lisp-mode))
(after! sly
  (setq sly-description-autofocus 't))

;;; Load from volatile
(load! "volatile")

;;; continue here
(use-package! dogears
  :hook (after-init . dogears-mode)
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-idle 1
        dogears-limit 200
        dogears-position-delta 20
        dogears-functions '(find-file recenter-top-bottom
                            other-window switch-to-buffer
                            aw-select toggle-window-split
                            windmove-do-window-select
                            pager-page-down pager-page-up
                            tab-bar-select-tab
                            pop-to-mark-command
                            pop-global-mark
                            goto-last-change
                            xref-go-back
                            xref-find-definitions
                            xref-find-references)))

;; Fix guix
(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin"))

(setq inferior-lisp-program "/usr/bin/sbcl")


(after! comint
  (setq comint-buffer-maximum-size 81920))

;; (add-to-list 'load-path "~/develop/denote-term")
;; (require 'denote-term)
;; (add-to-list 'load-path "~/develop/time-tracker")
;; (require 'time-tracker)


;;; Test ground
(defvar +org-fold-task-ellipsis ".")
(defun +org-fold-find-ov-with-exact-length (begin end ovs)
  (seq-filter (lambda (ov) (and (equal (overlay-start ov) begin)
                                (equal (overlay-end ov) end)))
              ovs))

(defun +org-section-contains-only-types-p (types text)
  (with-temp-buffer
    (setq-local tab-width 8)
    (insert text)
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree 'section
        (lambda (sec)
          (let* ((node-types (mapcar #'org-element-type (org-element-contents sec ))))
            (all (lambda (node-type)
                   (any (lambda (keyword)
                          (eq keyword node-type))
                        types))
                 node-types)))))))

(defun +org-fold-core-region (from to fold &optional spec)
  (when (and (eq spec 'outline)
             (org-element-property :todo-keyword (org-element-at-point))
             (any (lambda (keyword) (org-element-property keyword (org-element-at-point)))
                  '(:closed :scheduled :deadline)))
    (with-silent-modifications
      (org-with-wide-buffer
       (if fold
           (when-let ((ov (car-safe
                           (+org-fold-find-ov-with-exact-length from
                                                                to
                                                                (overlays-in from to)))))
             (let* ((begin (org-element-begin (org-element-at-point)))
                    (end   (org-element-end (org-element-at-point)))
                    (text  (buffer-substring begin end)))
               (when (+org-section-contains-only-types-p '(planning property-drawer) text)
                 (overlay-put ov 'display (if (equal (org-element-property :todo-type (org-element-at-point)) 'done)
                                              (propertize +org-fold-task-ellipsis
                                                          'face
                                                          'org-headline-done)
                                            +org-fold-task-ellipsis)))))
         (remove-overlays from to 'display +org-fold-task-ellipsis))))))
(advice-add 'org-fold-core-region :after #'+org-fold-core-region)

(defun ff/denote-open-link-function (path)
  (cl-flet ((has-dired-mode-p (elt)
              (equal 'dired-mode (buffer-local-value 'major-mode elt))))
    (message "%s" (seq-some has-dired-mode-p (window-list)))
    (if (seq-some has-dired-mode-p (window-list))
        (funcall #'find-file path)
      (funcall #'find-file-other-window path))))
(setq denote-open-link-function #'ff/denote-open-link-function)

(defun ff/denote-link-ol-follow (f link)
  (cl-flet ((has-dired-mode-p (elt)
              (equal 'dired-mode (buffer-local-value 'major-mode (window-buffer elt)))))
    (if (seq-some #'has-dired-mode-p (window-list))
        (dlet ((org-link-frame-setup '((file . find-file))))
          (funcall f link))
      (dlet ((org-link-frame-setup-function '((file . find-file-other-window))))
        (funcall f link)))))
(advice-add #'denote-link-ol-follow :around #'ff/denote-link-ol-follow)

;; (defun screenshot-svg ()
;;   "Save a screenshot of the current frame as an SVG image.
;; Saves to a temp file and puts the filename in the kill ring."
;;   (interactive)
;;   (let* ((filename (make-temp-file "Emacs" nil ".svg"))
;;          (data (x-export-frames nil 'svg)))
;;     (with-temp-file filename
;;       (insert data))
;;     (kill-new filename)
;;     (message filename)))


;; (defun my/log-call-process (orig-fun &rest args)
;;   "Log the call to `call-process` with current time."
;;   (message "[%s] [call-process] args: %S" (current-time-string) args)
;;   (apply orig-fun args))

;; (advice-add 'call-process :around #'my/log-call-process)

(define-derived-mode denote-dash-mode tabulated-list-mode "Denote Menu"
  "Major mode for browsing a list of Denote files."
  :interactive nil
  (if denote-menu-show-file-signature
      (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                    ("Signature" ,denote-menu-signature-column-width nil)
                                    ("Title" ,denote-menu-title-column-width nil)
                                    ("Keywords" ,denote-menu-keywords-column-width nil)])

    (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                  ("Title" ,denote-menu-title-column-width nil)
                                  ("Keywords" ,denote-menu-keywords-column-width nil)]))


  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;; continue here
(defun ff/all-denote-file-names ()
  (mapcar
   (lambda (elt) (string-remove-prefix (denote-directory) elt))
   (denote-directory-files)))

(defun ff/filter-journal-by-dates (&optional date-str)
  (let ((date-str (or date-str (format-time-string "%Y%m%d" (current-time)))))
    (seq-filter
     (lambda (elt) (string-prefix-p
                    (concat (car-safe (denote-directories)) "journal/" date-str)
                    elt))
     (denote-directory-files))))

(defvar vv/merge-journal-section-templates
  '((:line "--[" time "]--" (:when title "[" title "]"))
    (:blank)
    (:line content)))

(defun ff/apply-journal-template (form env)
  (cond
   ((stringp form) form)
   ((and (symbolp form)
         (not (keywordp form)))
    (let ((value (alist-get form env 'not-in)))
      (if (eq 'not-in value)
          (error "key is not in the env")
        value)))
   ((eq :blank (car-safe form))
    (if (null (cdr form))
        "\n"
      (error "(:blank) has no rest argument")))
   ((eq :line (car-safe form))
    (if (cdr form)
        (concat (mapconcat (lambda (form) (ff/apply-journal-template form env))
                           (cdr form))
                "\n")
      (error "(:line ...) should have CDR content. Please use (:blank) represent blank line")))
   ((eq :when (car-safe form))
    (unless (cdr form)
      (error "(:when COND ...) must have cond"))
    (let* ((rest (cdr form))
           (cond (car-safe rest))
           (cond (ff/apply-journal-template cond env)))
      (when cond
        (setq rest (cdr rest))
        (when rest
          (mapconcat (lambda (form) (ff/apply-journal-template form env))
                     rest)))))
   ((listp form)
    (mapconcat (lambda (form) (ff/apply-journal-template form env))
               form))
   (t (error (format "%s not valid template element" form)))))


(defun ff/format-journal-section (time title content)
  (ff/apply-journal-template vv/merge-journal-section-templates
                             `((time . ,time)
                               (title . ,title)
                               (content . ,content))))

(defun ff/denote-file-content-without-front-matter (ab-path)
  (with-temp-buffer
    (insert-file-contents ab-path)
    (goto-char (point-min))
    (while (looking-at "^#\\+.*$")
      (forward-line 1))
    (buffer-substring (point) (point-max))))

(defun ff/merge-journal-content (journals)
  (let (result)
    (dolist (journal journals result)
      (let* ((identifier (denote-retrieve-filename-identifier journal))
             (time-of-identifier (date-to-time identifier))
             (time-of-day (format-time-string "%H:%M" time-of-identifier))
             (title (let ((title (denote-retrieve-filename-title journal)))
                      (when (eq title (downcase (format-time-string "%B-%d-%Y" time-of-identifier)))
                        title)))
             (content (string-remove-prefix "\n" (ff/denote-file-content-without-front-matter journal))))
        (push (cons identifier (ff/format-journal-section time-of-day title content)) result)))))

;; TODO use nlp parse date
(defun ff/get-all-journal-date-for-prompt ()
  (delete-dups
   (mapcar (lambda (elt)
             (substring (string-remove-prefix (concat (car-safe (denote-directories)) "journal/")
                                              elt)
                        0 8))
           (ff/filter-journal-by-dates ""))))

(defun ff/journal-view-prompt ()
  (completing-read "Choose a date:"
                   (ff/get-all-journal-date-for-prompt)))

(defun aa/journal-view (&optional date-str)
  (interactive
   (list (ff/journal-view-prompt)))
  (let* ((journal-sections (ff/merge-journal-content (ff/filter-journal-by-dates date-str)))
         (journal-sections (mapcar #'cdr (sort journal-sections (lambda (x y)
                                                                  (string-lessp (car x) (car y))))))
         (front-matter (denote--format-front-matter (denote-journal-mdy-title-format-with-env date-str)
                                                    (date-to-time (concat date-str "T235959"))
                                                    nil
                                                    (concat date-str "T235959")
                                                    "" 'org))
         (journal (progn (push front-matter journal-sections) (mapconcat #'identity journal-sections)))
         (main-buffer (get-buffer-create (concat "*" (denote-journal-mdy-title-format-with-env date-str) "*")))
         (side-display-action `((display-buffer-in-side-window)
                                (dedicated . t)
                                (side . left)
                                (slot . 0))))
    (let ((main-window (display-buffer main-buffer '((display-buffer-full-frame))))
          (side-window (display-buffer (find-file-noselect "~/aa.log") side-display-action)))
      (with-current-buffer main-buffer
        (erase-buffer)
        (org-mode)
        (insert journal)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (window-live-p side-window)
                      (delete-window side-window)))
                  nil
                  t)))))
