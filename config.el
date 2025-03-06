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

;;; Code:
;;;; indent macro definition
(defmacro progn-package (_ &rest body)
  (declare (indent 1))
  `(progn ,@body))

;;;; benchmark init
(unless (daemonp)
  (use-package! benchmark-init
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'doom-first-input-hook 'benchmark-init/deactivate)))

;;;; doom-hacks fix
;;;;; remove gcmh-mode in doomemacs
(remove-hook! doom-first-buffer #'gcmh-mode)
(add-hook! doom-after-init
  (setq gc-cons-threshold (* 16 1024 1024))
  (setq gc-cons-percentage 0.2))
(fset '+lsp-optimization-mode (lambda (&rest _)))

;;;;; projectile (remove .project to fix jdtls)
(after! projectile
  (setq projectile-project-root-files-bottom-up '(".ccls-root" ".projectile"
                                                  ".git" ".hg"))
  (advice-remove #'projectile-dirconfig-file #'doom--projectile-dirconfig-file-a))

;;;; Font defintion
(setq doom-font (font-spec :family "MonoLisa duli Modified" :weight 'light :size 30)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
      doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 30)
      doom-symbol-font (font-spec :family "LXGW Wenkai Mono")
      doom-serif-font (font-spec :family "CMU Typewriter Text" :weight 'light :size 30))

;;;; Basic settings and more
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
      delete-by-moving-to-trash t)

(put 'narrow-to-region 'disabled nil)

(pixel-scroll-precision-mode 1)

;;;; doom theme and modeline hight
(setq doom-theme 'modus-operandi-tritanopia
      doom-modeline-height 44)

;;;; emoji settings(libxft)
;;Fallback to xft, if we not using cairo
(when (boundp 'xft-ignore-color-fonts)
  (setq xft-ignore-color-fonts nil
        face-ignored-fonts nil)
  (set-fontset-font t 'emoji
                    '("Noto Color Emoji" . "iso10646-1") nil 'prepend))

;;;; org-level-{1..9} faces
(add-hook! doom-load-theme
  (apply #'custom-theme-set-faces
         'user
         (cl-loop for i from 1 to 8
                  collect (list (intern (format "org-level-%d" i))
                                `((t (:inherit ,(intern (format "modus-themes-heading-%d" i))
                                      :extend t
                                      :weight normal)))))))
;;;; disable display-line-numbers-mode
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;;; fullscreen setup
(if (daemonp)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;;;; which-key delay
(after! which-key
  (setq which-key-idle-delay 0.01)
  (setq which-key-idle-secondary-delay 0.01))

;;;; keymap set (C-c b, workspaces, ....)
(global-set-key  [C-mouse-wheel-up-event]  'text-scale-increase)
(global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)
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

(setq frame-title-format '(:eval (+generate--frame-format)))

;;;; Meow
;; setup-doom-keybindings
;; (map! :map meow-normal-state-keymap
;;       doom-leader-key doom-leader-map)
;; (map! :map meow-motion-state-keymap
;;       doom-leader-key doom-leader-map)
;; (map! :map meow-beacon-state-keymap
;;       doom-leader-key nil)

;; (use-package! meow
;;   :config
;;   ;; (meow-global-mode 1)
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
;;   (meow-motion-overwrite-define-key
;;    '("j" . meow-next)
;;    '("k" . meow-prev)
;;    '("<escape>" . keyboard-quit))
;;   (meow-leader-define-key
;;    ;; SPC j/k will run the original command in MOTION state.
;;    '("j" . "H-j")
;;    '("k" . "H-k") ;;因为j，k覆盖了按键，使原生按键可用得加SPC
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet)
;;    ;; '("SPC" . keyboard-escape-quit)
;;    )
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("C" . meow-change-save)
;;    '("d" . meow-C-d)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("F" . meow-find-expand)
;;    '("g" . meow-cancel)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("I" . meow-open-above)
;;    '("i" . meow-insert)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("N" . meow-pop-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("P" . meow-yank-pop)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("T" . meow-till-expand)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("V" . meow-kmacro-matches)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-kmacro-lines)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("Z" . meow-pop-all-selection)
;;    '("&" . meow-query-replace)
;;    '("%" . meow-query-replace-regexp)
;;    '("'" . repeat)
;;    '("\\" . quoted-insert)
;;    '("<escape>" . ignore))
;;   ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
;;   (meow-setup-indicator))

;;;; emacs-rime
;; emacs do not provide us a way to make keybinding live all over the time, but
;; use-package does. and don't need define a new minor mode. found in
;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
;; We could use use-package:bind-key*
;;
;; Now it is default C-\

;; rime setting
(use-package! rime
  :bind
  ;; make delete words works
  (:map rime-mode-map
        ("S-<delete>" . 'rime-send-keybinding))
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.local/share/emacs-rime")
  (rime-share-data-dir "~/.local/share/rime-data")
  (rime-show-candidate 'posframe)
  ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p))
  ;; (rime-disable-predicates
  ;;  '(meow-normal-mode-p
  ;;    meow-motion-mode-p
  ;;    meow-keypad-mode-p
  ;;    meow-beacon-mode-p))
  )

(after! rime
  (when (daemonp)
    (unless rime--lib-loaded
      (unless (file-exists-p rime--module-path)
        (rime-compile-module))
      (rime--load-dynamic-module))))

;;;; Org-mode
(after! org
  (setq org-directory "~/documents/notes"

        org-id-method 'ts ;; do not use uuid
        org-cycle-separator-lines 1

        ;; yank-media
        org-yank-image-save-method (concat org-directory "/assets")
        org-yank-image-file-name-function #'+org-yank-image-with-denote-id-or-default

        org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
                             "|" "DONE(d)" "KILL(k)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "YEAR FLAG" "CONTINUOUS FLAG" "|" "MISSION COMPLETE"))

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
                                           :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))
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
  (setf (cdr (assoc "\\.pdf\\'" org-file-apps)) "nixGL sioyek %s")
  (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "nixGL sioyek %s --page %1"))

  ;; disable org-agenda dynamic add and remove
  (put 'org-agenda-file-to-front 'disabled t)
  (put 'org-remove-file 'disabled t)

  ;; set org-encrypt key
  (setq org-crypt-key "01FC4F4457FB76A78E17C1CEDDF3E86600E4955A"))

(after! org-tree-slide
  (advice-remove 'org-tree-slide--display-tree-with-narrow
                 #'+org-present--hide-first-heading-maybe-a)
  (defadvice! +org-present--hide-first-heading-maybe-a (fn &rest args)
    "Omit the first heading if `+org-present-hide-first-heading' is non-nil."
    :around #'org-tree-slide--display-tree-with-narrow
    (letf!
      (defun org-narrow-to-subtree (&optional element)
        "Narrow buffer to the current subtree."
        (interactive)
        (save-excursion
          (save-match-data
            (org-with-limited-levels
             (narrow-to-region
              (progn
                (when (org-before-first-heading-p)
                  (org-next-visible-heading 1))
                (org-back-to-heading t)
                (when +org-present-hide-first-heading
                  (forward-line 1))
                (point))
              (progn
                (org-end-of-subtree t t)
                (when (and (org-at-heading-p) (not (eobp)))
                  (backward-char 1))
                (point)))))))
      (apply fn args))))
;;;;; denote with org-capture
(defun +denote-downcase-str (STR)
  (downcase STR))

(use-package! denote
  :config
  (setq denote-directory (concat (getenv "HOME") "/documents/notes/")
        denote-dired-directories (list denote-directory)
        denote-sort-keywords nil
        denote-known-keywords '("emacs" "note")
        denote-prompts '(keywords title)
        denote-file-name-slug-functions  '((title . denote-sluggify-title)
                                           (keyword . +denote-downcase-str) ; make multi word keyword works
                                           (signature . denote-sluggify-signature))))

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

(defvar +denote-journal-file (concat denote-directory (format-time-string "%Y0101T000000--") "journal__denote_journal.org"))
(defvar +denote-refile-file (concat denote-directory "19700101T000000--refile__denote_refile.org"))
(defvar +denote-todo-file (concat denote-directory "19700102T000000--todo__denote_todo.org"))

(after! org-capture
  (setq org-capture-templates
        '(("n" "New note (with Denote)" plain (file denote-last-path)
           #'denote-org-capture
           :no-save t
           :immediate-finish nil
           :kill-buffer t
           :jump-to-captured t)
          ("j" "Journal" entry (file+olp+datetree +denote-journal-file)
           "* %<%I:%M %p> %?")
          ("t" "Personal todo" entry (file+headline +denote-todo-file "Inbox")
           "* [ ] %?" :prepend t)
          ("c" "Collections (need refile)" entry (file+headline +denote-refile-file "Inbox")
           "* %?" :prepend t))))

(setq org-agenda-files '("~/documents/notes/"))
(setq org-roam-directory (concat (getenv "HOME") "/roam"))

;;;; lsp and company and orderless and programing
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

        lsp-clients-clangd-args '("-j=8"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))

  (setq +lsp-company-backends '(company-yasnippet :separate company-capf))

  ;; TODO
  ;; (setq-hook! '(c-mode-hook c++-mode-hook) +format-with-lsp nil)
  )

(after! company
  (setq company-idle-delay 0.01)
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection))

(after! orderless
  (setq orderless-component-separator "[ &·]+"))

(set-file-template! "\\.h$" :trigger "__h" :mode 'c-mode)

;;;;; lsp-java
(after! lsp-java
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz"))

;;;;; lsp-rust
(after! lsp-rust
  (setq lsp-rust-analyzer-lru-capacity 1024))

;;;;; lsp-booster
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;;;; lsp-pyright
(use-package! lsp-pyright
  :after lsp-mode
  :config
  (setq lsp-pyright-use-library-code-for-types t
        lsp-pyright-stub-path (concat (getenv "HOME") "/Clone/python-type-stubs")
        lsp-pyright-langserver-command "basedpyright"))

;;;; TODO csharp
(after! csharp-mode
  (add-hook! csharp-mode
    (c-set-offset 'func-decl-cont 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'topmost-intro-cont 0)))

;;;; +utils and something
(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

(use-package! wakatime-mode
  :config
  (global-wakatime-mode))

(after! 'treemacs
  (setq! treemacs-width 25))

(use-package! indent-bars
  :hook ((python-mode yaml-mode) . indent-bars-mode))

(after! doom-modeline
  (setq! doom-modeline-env-enable-python nil))

(add-hook! '(scheme-mode-hook emacs-lisp-mode-hook lisp-mode-hook)
           #'paredit-mode)

;;;; clang-fotmat+
(use-package! clang-format+
  :hook (c-mode-common . clang-format+-mode)
  :config
  (setq clang-format+-context 'modification
        clang-format+-always-enable t))

;;;; cns
(use-package! cns
  :config
  (let ((repodir (concat doom-local-dir "straight/" (format "build-%s" emacs-version) "/cns/")))
    (setq cns-prog (concat repodir "cnws")
          cns-dict-directory (concat repodir "cppjieba/dict")))
  :hook
  (find-file . cns-auto-enable))

;;;; outli
(use-package outli
  :hook ((prog-mode text-mode) . outli-mode)
  :bind
  (:map outli-mode-map
        ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading)))))

;;;; nix
(after! nix-mode
  (set-formatter! 'nixpkgs-fmt '("nixpkgs-fmt") :modes '(nix-mode)))

;;;; copilot
(use-package! copilot
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;;; typst-mode
(use-package typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

;;;; gptel
(after! gptel
  (setq gptel-model 'MiniMax-Text-01
        gptel-backend
        (gptel-make-openai "minimax"    ;Any name you want
          :host "api.minimax.chat"
          :endpoint "/v1/text/chatcompletion_v2"
          :stream t
          :key (gptel-api-key-from-auth-source "api.minimax.chat" "apikey")
          :models '(MiniMax-Text-01 abab6.5s-chat))))

;;; continue here

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

(after! emmet-mode
  (unbind-key "<tab>" emmet-mode-keymap)
  (map! :map emmet-mode-keymap
        "C-<tab>" #'+web/indent-or-yas-or-emmet-expand))

(use-package! annotate
  ;; :hook (prog-mode . annotate-mode)
  :config
  (setq annotate-file (concat (getenv "HOME") "/documents/notes/annotate")))

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

(add-to-list 'display-buffer-alist
             `(,(rx string-start "*Async Shell Command*" string-end)
               (display-buffer-no-window)))

(load! "serenity-org-okular.el")


;; fix guix
(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin"))

(add-to-list 'auto-mode-alist (cons "\\.zuo\\'" 'racket-mode))
(add-to-list 'auto-mode-alist (cons ".sbclrc" 'lisp-mode))

(setq inferior-lisp-program "/usr/bin/sbcl")

(after! sly
  (setq sly-description-autofocus 't))

(after! comint
  (setq comint-buffer-maximum-size 81920))

(use-package! ekg
  :config
  (setq ekg-notes-display-images nil))

(use-package! telega
  :config
  (setq telega-server-libs-prefix "~/.nix-profile"))
