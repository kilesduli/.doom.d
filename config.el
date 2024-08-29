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
;;;; benchmark-init
(unless (daemonp)
  (use-package! benchmark-init
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'doom-first-input-hook 'benchmark-init/deactivate)))
;;;; Basic setup
(setq user-full-name "duli kiles"
      user-mail-address "duli4868@gmail.com")

(setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 30)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
      doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 30)
      doom-symbol-font (font-spec :family "LXGW Wenkai Mono" )
      doom-serif-font (font-spec :family "CMU Typewriter Text" :weight 'light :size 30))

(setq doom-theme 'modus-operandi-tritanopia)

(setq org-directory "~/documents/notes/")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil
      scroll-preserve-screen-position 'always
      scroll-margin 2
      word-wrap-by-category t
      delete-by-moving-to-trash t
      cursor-in-non-selected-windows t
      truncate-lines nil
      undo-limit 80000000
      display-line-numbers-type 'relative)

(if (daemonp)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq which-key-idle-delay 0.01)
(setq which-key-idle-secondary-delay 0.01)

(pixel-scroll-precision-mode 1)

(global-set-key  [C-mouse-wheel-up-event]  'text-scale-increase)
(global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)

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
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

      :map doom-leader-file-map
      "o" #'find-file-other-window)

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
     ((doom-real-buffer-p (current-buffer))
      `("%b" " - [" ,(abbreviate-file-name dir) "]"))
     (t
      `("%b" " - GNU Emacs at " system-name)))))

(setq frame-title-format '(:eval (+generate--frame-format)))

;;;; Meow
;; setup-doom-keybindings
;; (map! :map meow-normal-state-keymap
;;       doom-leader-key doom-leader-map)
;; (map! :map meow-motion-state-keymap
;;       doom-leader-key doom-leader-map)
;; (map! :map meow-beacon-state-keymap
;;       doom-leader-key nil)

(use-package! meow
  :config
  ;; (meow-global-mode 1)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . keyboard-quit))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k") ;;因为j，k覆盖了按键，使原生按键可用得加SPC
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   ;; '("SPC" . keyboard-escape-quit)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-C-d)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("I" . meow-open-above)
   '("i" . meow-insert)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)
   '("<escape>" . ignore))
  ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
  (meow-setup-indicator))

;;;; rime
;; emacs do not provide us a way to make keybinding live all over the time, but
;; use-package does. and don't need define a new minor mode. found in
;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
;; We could use use-package:bind-key*

;; rime setting
(use-package! rime
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.local/share/emacs-rime")
  (rime-share-data-dir "~/.local/share/rime-data")
  (rime-show-candidate 'posframe)
  ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p))
  (rime-disable-predicates
   '(meow-normal-mode-p
     meow-motion-mode-p
     meow-keypad-mode-p
     meow-beacon-mode-p)))

(after! rime
  (when (daemonp)
    (unless rime--lib-loaded
      (unless (file-exists-p rime--module-path)
        (rime-compile-module))
      (rime--load-dynamic-module))))

;;;; Org-mode
;; disable company chinese extend.
(after! org
  (unless (featurep 'company)
    (require 'company))
  (setq company-dabbrev-char-regexp "[\\.0-9a-zA-Z-_'/]")
  (push 'company-dabbrev company-backends)
  (set-company-backend! 'org-mode
    'company-dabbrev 'company-yasnippet))

(after! ox
  (require 'ox-gfm))

(after! org
  (setq org-id-method 'ts)
  (setq org-cycle-separator-lines 1)
  (setq org-link-descriptive nil)
  (setq org-cycle-separator-lines 1)
  (setq org-latex-packages-alist '(("" "amssymb" t ("xelatex"))
                                   ("" "amsmath" t ("xelatex")))))

(add-hook! org-mode
  (setq-local line-spacing 0.1)
  (setq-local display-line-numbers-mode 0))

;;  org-latex-preview
(add-hook 'org-mode-hook 'org-fragtog-mode)

;;set latex preview default process
(setq org-preview-latex-process-alist
      '((xdv2svg
         :programs ("xelatex" "dvisvgm")
         :description "xdv > svg"
         :message "you need to install the programs: latex and dvisvgm."
         :image-input-type "xdv"
         :image-output-type "svg"
         :image-size-adjust (1.5 . 1.5)
         :latex-compiler ("xelatex -interaction nonstopmode -no-pdf -output-directory %o %f")
         :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))

(setq org-preview-latex-default-process 'xdv2svg)
(setq org-latex-pdf-process
      '("tectonic -X compile --outdir=%o -Z shell-escape -Z continue-on-errors %f"))

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

(defun +org-yank-image-filename-with-denote-id ()
  (unless (featurep 'denote)
    (require 'denote))
  (if-let ((id (denote-retrieve-filename-identifier (buffer-file-name))))
      (format (concat "C" id "-" (substring (org-id-uuid) 0 (min 8))))
    (org-yank-image-autogen-filename)))

(after! org
  (setq org-yank-image-save-method (concat org-directory "assets"))
  (setq org-yank-image-file-name-function #'+org-yank-image-filename-with-denote-id))

;;;; lsp and company
(after! lsp-mode
  (setq!
   lsp-enable-file-watchers nil
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
   lsp-log-io nil))

(after! lsp-ui
  (setq! lsp-ui-sideline-enable nil))

;;disable lsp-format-buffer in cc-mode, it doesn't running well according to .clang-format
;;usage: https://docs.doomemacs.org/latest/modules/editor/format/#:~:text=To%20disable%20this%20behavior%20in%20one%20mode%3A%20(setq%2Dhook!%20%27python%2Dmode%2Dhook%20%2Bformat%2Dwith%2Dlsp%20nil)
(setq-hook! 'c++-mode-hook +format-with-lsp nil)
(setq-hook! 'c-mode-hook +format-with-lsp nil)

;;set company tab complete motion
(after! company
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection))

(after! company
  (setq company-idle-delay 0.01))

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0")))

(setq +lsp-company-backends '(company-yasnippet :separate company-capf))
(after! orderless
  (setq! orderless-component-separator "[ &·]+"))

;;;;; python pyright
(use-package! lsp-pyright
  :after lsp-mode
  :config
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/Clone/python-type-stubs")))

;;;; TODO csharp
(add-hook 'csharp-mode-hook #'(lambda ()
                                (c-set-offset 'func-decl-cont 0)
                                (c-set-offset 'statement-cont 0)
                                (c-set-offset 'topmost-intro-cont 0)))

;;;; +utils
(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

(use-package! wakatime-mode
  :config
  (global-wakatime-mode))

(after! treemacs
  (setq! treemacs-width 25))

;;;; clang-fotmat+
(use-package! clang-format+
  :hook (c-mode-common . clang-format+-mode)
  :config
  (setq clang-format+-context 'modification)
  (setq clang-format+-always-enable t))

;;;; something
(use-package! indent-bars
  :hook ((python-mode yaml-mode) . indent-bars-mode))

(after! doom-modeline
  (setq! doom-modeline-env-enable-python nil))

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(run-with-idle-timer 30 t #'recentf-save-list)

;;;; cns
(use-package! cns
  :config
  (let ((repodir (concat doom-local-dir "straight/" straight-build-dir "/cns/")))
    (setq cns-prog (concat repodir "cnws")
          cns-dict-directory (concat repodir "cppjieba/dict")))
  :hook
  (find-file . cns-auto-enable))


;;;; outli
(use-package outli
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :config
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

;;;; nix
(set-file-template! "\\.h$" :trigger "__h" :mode 'c-mode)
(after! nix-mode
  (set-formatter! 'nixpkgs-fmt '("nixpkgs-fmt") :modes '(nix-mode)))

;;;; denote with org-capture
(use-package! denote
  :config
  (setq denote-directory (concat (getenv "HOME") "/documents/notes/"))
  (setq denote-sort-keywords nil)
  (setq denote-known-keywords '(emacs note))
  (setq denote-prompts '(keywords title)))

(defvar +org-capture-collections-file "collections.org"
  "Default target for collections notes

Collecting everyting you do not wanna miss

Is relative to `org-directory' unless it is absolute. Is used in
`org-capture-templates'.")

(after! org-capture
  (setq org-capture-templates
        '(("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
           "* %<%I:%M %p> %?")
          ("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?" :prepend t)))
  (cl-pushnew '("n" "New note (with Denote)" plain
                (file denote-last-path)
                #'denote-org-capture
                :no-save t
                :immediate-finish nil
                :kill-buffer t
                :jump-to-captured t)
              org-capture-templates
              :test #'equal))

(defun replace-dashes-with-stars (n)
  "Replace dashes with stars at the beginning of each line.
The number of stars will be increased by N for each tab before the dash."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\t*\\)-" nil t)
      (replace-match (make-string (1+ (length (match-string 1))) ?*) t nil))))

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



;;;;; continue

(defcustom unpackaged/lorem-ipsum-overlay-exclude nil
  "List of regexps to exclude from `unpackaged/lorem-ipsum-overlay'."
  :type '(repeat regexp))

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


(after! lsp-java
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz"))

(after! projectile
  (setq projectile-project-root-files-bottom-up '(".ccls-root" ".projectile"
                                                  ".git" ".hg")))

(after! projectile
  (defun projectile-reset-current-project-cache ()
    (interactive)
    (let ((project-root (projectile-project-root)))
      (puthash project-root
               '()
               projectile-projects-cache)))
  (defun projectile-reset-all-project-cache ()
    (interactive)
    (setq projectile-projects-cache (make-hash-table :test 'equal))))

(after! consult
  (unless (featurep 'beframe)
    (require 'beframe))
  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")
  (defvar beframe--consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     beframe-buffer
       :history  beframe-history
       :items    ,#'beframe--buffer-names
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))
  (add-to-list 'consult-buffer-sources 'beframe--consult-source))



(after! consult
  (defvar +consult-kill-buffer-source '(beframe--consult-source
                                        consult--source-hidden-buffer
                                        consult--source-modified-buffer
                                        consult--source-buffer
                                        consult--source-project-buffer-hidden))
  (defun +consult-kill-buffer ()
    (interactive)
    (let ((selected (consult--multi +consult-kill-buffer-source
                                    :prompt "Kill buffer: "
                                    :history 'consult--buffer-history
                                    :preview-key nil
                                    :sort nil)))
      (when (plist-get (cdr selected) :match)
        (kill-buffer (car selected))))))

(autoload '+consult-kill-buffer "consult" :type t)
(map! "C-x k" #'+consult-kill-buffer)

(use-package! copilot
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


(after! emmet-mode
  (unbind-key "<tab>" emmet-mode-keymap)
  (map! :map emmet-mode-keymap
        "C-<tab>" #'+web/indent-or-yas-or-emmet-expand))



(after! dash
  (defun -to-head (n list)
    "Return a new list that move the element at Nth to the head of old LIST."
    (declare (pure t) (side-effect-free t))
    (if (> n (1- (length list)))
        (error "Index %d out of the range of list %S" n list))
    (let* ((head (-take n list))
           (rest (-drop n list))
           (target (pop rest)))
      (cons target (nconc head rest))))

  (defun -shuffle (list &optional rng)
    "Return a new shuffled LIST, shuffling using RNG.

The returned list is shuffled by using Fisher-Yates' Algorithm. See
https://en.wikipedia.org/wiki/Fisher-Yates_shuffle for more details."
    (declare (pure t) (side-effect-free t))
    (let* ((len (length list))
           (random-nums (-map (or rng #'random) (number-sequence len 1 -1)))
           result)
      (--each random-nums
        (setq list (-to-head it list))
        (push (pop list) result))
      (nreverse result))))

(use-package! annotate
  :hook (prog-mode . annotate-mode)
  :config
  (setq annotate-file (concat (getenv "HOME") "/documents/notes/annotate")))

(use-package! dogears
  :config
  (dogears-mode 1)
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))
