;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "duli kiles"
      user-mail-address "duli4868@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-pro)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `'relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users mustpress 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;config of basic variable setting =============================================

;;which key delay
(setq which-key-idle-delay 0.01)
(setq which-key-idle-secondary-delay 0.01)

;;pixel scroll
(pixel-scroll-precision-mode 1)

;;set input toggle C-SPC
(global-set-key (kbd "C-SPC") 'toggle-input-method)

;; 滚轮放大
(global-set-key  [C-mouse-wheel-up-event]  'text-scale-increase)
(global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)

;;font
;;(set-face-attribute 'default nil :height 120)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;(setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 33)
;;        doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
;;        doom-unicode-font (font-spec :family "LXGW Wenkai Mono" )
;;        doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 37)
;;        doom-serif-font(font-spec :family "CMU Typewriter Text" :weight 'light :size 37 ))


(setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 28)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
      doom-unicode-font (font-spec :family "LXGW Wenkai Mono" )
      doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 28)
      doom-serif-font(font-spec :family "CMU Typewriter Text" :weight 'light :size 28))

;;redo
(global-set-key (kbd "C-r" ) 'undo-fu-only-redo)

;;centered-window always online
;;(centered-window-mode t)

;;background
;;(add-to-list 'default-frame-alist '(alpha-background . 98))



;;==============================================================================

;;config of meow and buffer ====================================================
(defun set-useful-keybings()
  (define-key doom-leader-workspaces/windows-map (kbd "t") 'treemacs-select-window)
  (global-set-key (kbd "M-j") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "M-k") 'kmacro-end-or-call-macro)
  (map! :leader
        :desc "help"                         "h"   help-map
        (:after projectile :desc "project" "p" projectile-command-map)
        (:prefix-map ("b" . "buffer")
         :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
         :desc "Previous buffer"             "["   #'previous-buffer
         :desc "Next buffer"                 "]"   #'next-buffer
         (:when (featurep! :ui workspaces)
          :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           "B" #'switch-to-buffer)
         (:unless (featurep! :ui workspaces)
          :desc "Switch buffer"           "b" #'switch-to-buffer)
         :desc "Clone buffer"                "c"   #'clone-indirect-buffer
         :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
         :desc "Kill buffer"                 "d"   #'kill-current-buffer
         :desc "ibuffer"                     "i"   #'ibuffer
         :desc "Kill buffer"                 "k"   #'kill-current-buffer
         :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
         :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
         :desc "Set bookmark"                "m"   #'bookmark-set
         :desc "Delete bookmark"             "M"   #'bookmark-delete
         :desc "Next buffer"                 "n"   #'next-buffer
         :desc "New empty buffer"            "N"   #'evil-buffer-new
         :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
         :desc "Previous buffer"             "p"   #'previous-buffer
         :desc "Revert buffer"               "r"   #'revert-buffer
         :desc "Save buffer"                 "s"   #'basic-save-buffer
         :desc "Save all buffers"            "S"   #'evil-write-all
         :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
         :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
         :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
         :desc "Bury buffer"                 "z"   #'bury-buffer
         :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)
        )
  (map! :map doom-leader-file-map
        "o" #'find-file-other-window
        )

  )


(defun meow-setup ()
  (set-useful-keybings)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;;(add-to-list 'meow-keymap-alist (cons 'leader doom-leader-map))
  (meow-normal-define-key (cons "SPC" doom-leader-map))
  (meow-motion-overwrite-define-key (cons "SPC" doom-leader-map))
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k");;因为j，k覆盖了按键，使原生按键可用得加SPC
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
   '("SPC" . keyboard-escape-quit)
   )
  (map!
   (:when (featurep! :ui workspaces)
    ;;    :g "C-t"   #'+workspace/new
    ;;    :g "C-S-t" #'+workspace/display
    :g "M-1"   #'+workspace/switch-to-0
    :g "M-2"   #'+workspace/switch-to-1
    :g "M-3"   #'+workspace/switch-to-2
    :g "M-4"   #'+workspace/switch-to-3
    :g "M-5"   #'+workspace/switch-to-4
    :g "M-6"   #'+workspace/switch-to-5
    :g "M-7"   #'+workspace/switch-to-6
    :g "M-8"   #'+workspace/switch-to-7
    :g "M-9"   #'+workspace/switch-to-8
    :g "M-0"   #'+workspace/switch-to-finla
    ))
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
  )

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  ;;(meow-setup-line-number)
  ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
  (meow-setup-indicator))

;;==============================================================================

;;use emacs rime ===============================================================
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.config/ibus/rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(meow-normal-mode-p
     meow-motion-mode-p
     meow-keypad-mode-p
     meow-beacon-mode-p
     ))
  (rime-inline-predicates '(rime-predicate-space-after-cc-p))
  )

;;==============================================================================


;;org-mode =====================================================================

;;(with-eval-after-load 'org
;;  (defvar-local rasmus/org-at-src-begin -1
;;    "Variable that holds whether last position was a ")
;;
;;  (defvar rasmus/ob-header-symbol ?☰
;;    "Symbol used for babel headers")
;;
;;  (defun rasmus/org-prettify-src--update ()
;;    (let ((case-fold-search t)
;;          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
;;          found)
;;      (save-excursion
;;        (goto-char (point-min))
;;        (while (re-search-forward re nil t)
;;          (goto-char (match-end 0))
;;          (let ((args (org-trim
;;                       (buffer-substring-no-properties (point)
;;                                                       (line-end-position)))))
;;            (when (org-string-nw-p args)
;;              (let ((new-cell (cons args rasmus/ob-header-symbol)))
;;                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
;;                (cl-pushnew new-cell found :test #'equal)))))
;;        (setq prettify-symbols-alist
;;              (cl-set-difference prettify-symbols-alist
;;                                 (cl-set-difference
;;                                  (cl-remove-if-not
;;                                   (lambda (elm)
;;                                     (eq (cdr elm) rasmus/ob-header-symbol))
;;                                   prettify-symbols-alist)
;;                                  found :test #'equal)))
;;        ;; Clean up old font-lock-keywords.
;;        (font-lock-remove-keywords nil prettify-symbols--keywords)
;;        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
;;        (font-lock-add-keywords nil prettify-symbols--keywords)
;;        (while (re-search-forward re nil t)
;;          (font-lock-flush (line-beginning-position) (line-end-position))))))
;;
;;  (defun rasmus/org-prettify-src ()
;;    "Hide src options via `prettify-symbols-mode'.
;;        `prettify-symbols-mode' is used because it has uncollpasing. It's
;;        may not be efficient."
;;    (let* ((case-fold-search t)
;;           (at-src-block (save-excursion
;;                           (beginning-of-line)
;;                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
;;      ;; Test if we moved out of a block.
;;      (when (or (and rasmus/org-at-src-begin
;;                     (not at-src-block))
;;                ;; File was just opened.
;;                (eq rasmus/org-at-src-begin -1))
;;        (rasmus/org-prettify-src--update))
;;      ;; Remove composition if at line; doesn't work properly.
;;      ;; (when at-src-block
;;      ;;   (with-silent-modifications
;;      ;;     (remove-text-properties (match-end 0)
;;      ;;                             (1+ (line-end-position))
;;      ;;                             '(composition))))
;;      (setq rasmus/org-at-src-begin at-src-block)))
;;
;;  ;; This function helps to produce a single glyph out of a
;;  ;; string. The glyph can then be used in prettify-symbols-alist.
;;  ;; This function was provided by Ihor in the org-mode mailing list.
;;  (defun yant/str-to-glyph (str)
;;    "Transform string into glyph, displayed correctly."
;;    (let ((composition nil))
;;      (dolist (char (string-to-list str)
;;                    (nreverse (cdr composition)))
;;        (push char composition)
;;        (push '(Br . Bl) composition))))
;;
;;  (defun rasmus/org-prettify-symbols ()
;;    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
;;          (cl-reduce 'append
;;                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                             `(("#+begin_src" . ?⎡) ;; ⎡ ➤ 🖝 ➟ ➤ ✎
;;                               ;; multi-character strings can be used with something like this:
;;                               ;; ("#+begin_src" . ,(yant/str-to-glyph "```"))
;;                               ("#+end_src"   . ?⎣) ;; ⎣ ✐
;;                               ("#+header:" . ,rasmus/ob-header-symbol)
;;                               ("#+begin_quote" . ?«)
;;                               ("#+end_quote" . ?»)))))
;;    (turn-on-prettify-symbols-mode)
;;    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
;;  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))

;;set org latex preview
(add-hook 'org-mode-hook 'org-fragtog-mode)
;;set latex preview default process
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvipng :programs
                ("latex" "dvipng")
                :description "dvi > png"
                :message "you need to install the programs: latex and dvipng."
                :image-input-type "dvi"
                :image-output-type "png"
                :image-size-adjust
                (0.7 . 0.7)
                :latex-compiler
                ("latex -interaction nonstopmode -output-directory %o %f")
                :image-converter
                ("dvipng -D %D -T tight -o %O %f")
                :transparent-image-converter
                ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
        (dvisvgm :programs
                 ("latex" "dvisvgm")
                 :description "dvi > svg"
                 :message "you need to install the programs: latex and dvisvgm."
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust
                 (1.0 . 1.0)
                 :latex-compiler
                 ("latex -interaction nonstopmode -output-format=dvi -output-directory %o %f")
                 :image-converter
                 ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs
                     ("latex" "convert")
                     :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                     (1.0 . 1.0)
                     :latex-compiler
                     ("pdflatex -interaction nonstopmode -output-directory %o %f")
                     :image-converter
                     ("convert -density %D -trim -antialias %f -quality 100 %O")))
      )

;; org-latex-compilers = ("pdflatex" "xelatex" "lualatex"), which are the possible values for %latex
(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
(setq org-latex-compiler "xelatex")

;;load ob-sharp
(add-to-list 'load-path "~/.doom.d/customel/")

;;disable line number on org mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

;;org-mode 只补全 ascii 字符
(progn
  (push 'company-dabbrev-char-regexp company-backends)
  (setq company-dabbrev-char-regexp "[\\.0-9a-zA-Z-_'/]")
  (set-company-backend! 'org-mode
    'company-dabbrev-char-regexp 'company-yasnippet))


(use-package! org-roam
  :init
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :target (file+head "${slug}.org"
                                                         "${title}\n")
                                      :unnarrowed t)))
  :custom
  (org-roam-complete-everywhere t)
  :config
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))
  )

(map!
 :map org-mode-map
 "C-M-i"  #'completion-at-point
 :map doom-leader-notes-map
 (:prefix ("r" . "roam")
  :desc "go back" "b" #'org-mark-ring-goto)
 )

;;==============================================================================

;;lsp-mode and company =========================================================
(use-package! lsp-mode
  :custom
  (lsp-enable-links nil)                 ;; no clickable links
  (lsp-enable-folding nil)               ;; use `hideshow' instead
  (lsp-enable-snippet nil)               ;; no snippets, it requires `yasnippet'
  (lsp-enable-file-watchers nil)         ;; performance matters
  (lsp-enable-text-document-color nil)   ;; as above
  (lsp-enable-symbol-highlighting nil)   ;; as above
  (lsp-enable-on-type-formatting nil)    ;; as above
  (lsp-enable-indentation t)           ;; don't change my code without my permission
  (lsp-headerline-breadcrumb-enable nil) ;; keep headline clean
  (lsp-modeline-code-actions-enable nil) ;; keep modeline clean
  (lsp-modeline-diagnostics-enable nil)  ;; as above
  (lsp-log-io nil)                       ;; debug only
  (lsp-auto-guess-root t)                ;; Yes, I'm using projectile
  (lsp-keep-workspace-alive nil)         ;; auto kill lsp server
  (lsp-lens-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-eldoc-enable-hover nil)          ;; disable eldoc hover
  (lsp-signature-auto-activate nil)
  )

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))

;;disable lsp-format-buffer in cc-mode, it doesn't run according to .clang-format
;;usage: https://docs.doomemacs.org/latest/modules/editor/format/#:~:text=To%20disable%20this%20behavior%20in%20one%20mode%3A%20(setq%2Dhook!%20%27python%2Dmode%2Dhook%20%2Bformat%2Dwith%2Dlsp%20nil)
(setq-hook! 'c-mode-common-hook +format-with-lsp nil)
;;(use-package clang-format+
;;  :config
;;  (add-hook 'c-mode-common-hook #'clang-format+-mode)
;;  (add-hook 'csharp-mode-hook #'clang-format+-mode)
;;  (setq clang-format+-context 'modification)
;;  (setq clang-format+-always-enable t)
;;  )

;;set company tab complete motion
(after! company
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection)
  )
(map! :map lsp-mode-map  "<tab>"  #'company-indent-or-complete-common)

;;==============================================================================
(use-package! treemacs
  :config
  (setq treemacs-width 25)
  )

(after! magit
  (magit-delta-mode +1))

(use-package! systemd
  :defer t)

(use-package! ox-gfm
  :after ox)

;;something broken of font settings /斜体/
(use-package seperate-inline
  :hook ((org-mode-hook . separate-inline-mode)
         (org-mode-hook . (lambda ()
                            (add-hook 'separate-inline-mode-hook
                                      'separate-inline-use-default-rules-for-org-local
                                      nil 'make-it-local))))
  )

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;(use-package wakatime-mode
;;  :config
;;  (setq wakatime-cli-path "~/.wakatime/wakatime-cli")
;;  (global-wakatime-mode)
;;  )

;;(use-package! sis
;;  ;;:hook
;;  ;;enable the /follow context/ and /inline region/ mode for specific buffers
;;  ;;(((text-mode prog-mode) . sis-context-mode)
;;  ;; ((text-mode prog-mode) . sis-inline-mod
;;  :after meow
;;  ;;:defer-incrementally meow
;;  :config
;;  (sis-ism-lazyman-config "1" "2" 'fcitx5)
;;  (add-hook 'meow-insert-exit-hook #'sis-set-english)
;;  (add-to-list 'sis-context-hooks 'meow-insert-exit-hook)
;;  ;; (defun describe-key-sis ()
;;  ;;   (interactive)
;;  ;;   (sis-set-english)
;;  ;;   (sis-global-respect-mode 0)
;;  ;;   (describe-key (help--read-key-sequence))
;;  ;;   (sis-global-respect-mode t))
;;  ;; :bind (("C-h k" . describe-key-sis))
;;  )
