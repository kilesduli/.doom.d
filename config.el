;;; $DOOMDIR/config.el --- duli's doom emacs config -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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

;;; Code:
;;@1 Basic setup(benchmark user-identify font-setting theme-setting init-outline)
;;@@ benchmark-init
(use-package! benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'doom-first-input-hook 'benchmark-init/deactivate))

;;@@ user identify
;;(Some functionality uses this to identify you,
;;     e.g. GPG configuration, email clients, file templates and snippets)
(setq user-full-name "duli kiles"
      user-mail-address "duli4868@gmail.com")

;;@@ font setting
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
;;
;; Set fonts according to different window systems
(if (string= "x11" (getenv "XDG_SESSION_TYPE"))
    (setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 30)
          doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
          doom-symbol-font (font-spec :family "LXGW Wenkai Mono" )
          doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 30)
          doom-serif-font (font-spec :family "CMU Typewriter Text" :weight 'light :size 30))
  (setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 15)
        doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
        doom-symbol-font (font-spec :family "LXGW Wenkai Mono" )
        doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 15)
        doom-serif-font (font-spec :family "CMU Typewriter Text" :weight 'light :size 15 ))
  )

;;@@ theme setting
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;; doom-theme
(setq doom-theme 'modus-operandi-tritanopia)

;;@@ init-outline
(defun set-init-outline-minor-mode ()
  (interactive)
  (when (equal (expand-file-name "~/dotfiles/doomemacs/.doom.d/config.el")
               (buffer-file-name (current-buffer)))
    (setq-local outline-regexp ";;; Code\\|;;@+")
    (setq-local outline-heading-alist '((";;; Code" . 1) (";;@" . 2) (";;@@" . 3)))
    (setq-local outline-minor-mode-use-buttons 'in-margins)
    (setq-local outline-minor-mode-highlight 'override)
    (setq-local outline-minor-mode-cycle t)
    (setq-local outline-level 'outline-level)
    (outline-minor-mode)
    (init-outline-mode)))

(defvar-keymap init-outline-mode-map
  :doc "部分来自 outline-mode 的键绑定"
  "C-c C-n" #'outline-next-visible-heading
  "C-c C-p" #'outline-previous-visible-heading
  "C-c C-u" #'outline-up-heading
  "C-c C-a" #'outline-show-all)

(define-minor-mode init-outline-mode
  "用于浏览配置文件各节点的 minor-mode，添加了部分 outline-mode 按键绑定"
  :keymap init-outline-mode-map)

;;@2 Simple configuration related to emacs
;;@@ specify org directory
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/documents/org"
      org-roam-directory "~/documents/org-roam")

;;@@ display-line-number setting
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `'relative'.
(setq display-line-numbers-type 'relative)


;;@@ a bunch of setting
;;TODO
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil
      scroll-preserve-screen-position 'always ; Don't have `point' jump around
      scroll-margin 2                    ; It's nice to maintain a little margin
      word-wrap-by-category t
      delete-by-moving-to-trash t)       ; Different languages live together happily

;;@@ background transparent
;; (add-to-list 'default-frame-alist '(alpha-background . 95))


;;@@ let frame maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;@@ bind redo key to C-r
(bind-key* "C-r" 'undo-fu-only-redo)

;;@@ which-key-idle-delay
(setq which-key-idle-delay 0.01)
(setq which-key-idle-secondary-delay 0.01)

;;@@ enable pixel-scroll-precision-mode
(pixel-scroll-precision-mode 1)

;;@@ mouse wheel zoom
(global-set-key  [C-mouse-wheel-up-event]  'text-scale-increase)
(global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)

;;@@ Map M-n to switch workspace
(map!
 (:when (modulep! :ui workspaces)
   ;; :n "C-t"   #'+workspace/new
   ;; :n "C-S-t" #'+workspace/display
   :g "M-1"   #'+workspace/switch-to-0
   :g "M-2"   #'+workspace/switch-to-1
   :g "M-3"   #'+workspace/switch-to-2
   :g "M-4"   #'+workspace/switch-to-3
   :g "M-5"   #'+workspace/switch-to-4
   :g "M-6"   #'+workspace/switch-to-5
   :g "M-7"   #'+workspace/switch-to-6
   :g "M-8"   #'+workspace/switch-to-7
   :g "M-9"   #'+workspace/switch-to-8
   :g "M-0"   #'+workspace/switch-to-final
   ))

;;@@ map some keys..
;;TODO
(map! :leader
      :desc "help"                         "h"   help-map
      :after projectile :desc "project" "p" projectile-command-map
      :after projectile :desc "project-search(fd)" "p s" #'+default/search-project
      :after consult-org-roam :desc "consult-roam-search" "s r" #'consult-org-roam-search
      :after treemacs :desc "treemacs-select-window" "w t" #'treemacs-select-window
      )

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
      "o" #'find-file-other-window
      :after consult :desc "Consult-dir" "b" #'consult-dir
      )

;;@3 package setup
;;@@ Meow
;;setup-doom-keybindings
(map! :map meow-normal-state-keymap
      doom-leader-key doom-leader-map)
(map! :map meow-motion-state-keymap
      doom-leader-key doom-leader-map)
(map! :map meow-beacon-state-keymap
      doom-leader-key nil)
;; (add-to-list 'meow-keymap-alist (cons 'leader doom-leader-map))
;; (meow-normal-define-key (cons "SPC" doom-leader-map))
;; (meow-motion-overwrite-define-key (cons "SPC" doom-leader-map)) diff???


;; wanna figure out binding (set-useful-binding)
(global-set-key (kbd "M-j") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "M-k") 'kmacro-end-or-call-macro)

(use-package! meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . keyboard-quit))
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

;; ;; Use jk to escape from insert state to normal state
;; (defvar meow-two-char-escape-sequence "jk")
;; (defvar meow-two-char-escape-delay 0.5)
;; (defun meow--two-char-exit-insert-state (s)
;;   "Exit meow insert state when pressing consecutive two keys.

;; S is string of the two-key sequence."
;;   (when (meow-insert-mode-p)
;;     (let ((modified (buffer-modified-p))
;;           (undo-list buffer-undo-list))
;;       (insert (elt s 0))
;;       (let* ((second-char (elt s 1))
;;              (event
;;               (if defining-kbd-macro
;;                   (read-event nil nil)
;;                 (read-event nil nil meow-two-char-escape-delay))))
;;         (when event
;;           (if (and (characterp event) (= event second-char))
;;               (progn
;;                 (backward-delete-char 1)
;;                 (set-buffer-modified-p modified)
;;                 (setq buffer-undo-list undo-list)
;;                 (meow-insert-exit))
;;             (push event unread-command-events)))))))
;; (defun meow-two-char-exit-insert-state ()
;;   "Exit meow insert state when pressing consecutive two keys."
;;   (interactive)
;;   (meow--two-char-exit-insert-state meow-two-char-escape-sequence))
;; (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
;;             #'meow-two-char-exit-insert-state)

;;@@ rime
;; https://emacs.stackexchange.com/questions/65080/stop-major-modes-from-overwriting-my-keybinding
;; https://emacs.stackexchange.com/questions/27926/avoiding-overwriting-global-key-bindings
;; emacs do not provide us a way to make keybinding live all over the time, but use-package does. and don't need define a new minor mode.
;; found in https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
;; just (bind-key* ...)
;;; rime setting
;; emacs-rime doesn't support lua will cause some bug, like rime-scheme-select couldn't persistence store
(use-package! rime
  :bind* ("C-," . toggle-input-method)
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.local/share/emacs-rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(meow-normal-mode-p
     meow-motion-mode-p
     meow-keypad-mode-p
     meow-beacon-mode-p
     ))
  ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p))
  )

;; ;; unbind necessary key
;; ;; BUG：listp flyspell-mode
;; (use-package! flyspell
;;   :hook ((flyspell-mode . #'(lambda ()
;;                               (dolist (key '("C-;" "C-," "C-."))
;;                                 (unbind-key key flyspell-mode-map))))))

;;@@ Org-mode
;; disable company chinese extend.
(with-eval-after-load 'org
  (push 'company-dabbrev-char-regexp company-backends)
  (setq company-dabbrev-char-regexp "[\\.0-9a-zA-Z-_'/]")
  (set-company-backend! 'org-mode
    'company-dabbrev-char-regexp 'company-yasnippet))
;;; ob-csharp
;; (load! "ob-csharp")             ; It's org-babel functions for csharp evaluation.


;; export to gfm
(use-package! ox-gfm
  :after ox)

;; (use-package! separate-inline
;;   :hook ((org-mode . separate-inline-mode)
;;          (org-mode . (lambda ()
;;                        (add-hook 'separate-inline-mode-hook
;;                                  'separate-inline-use-default-rules-for-org-local
;;                                  nil 'make-it-local))))
;;   )

(use-package! org
  ;; :defer t
  :config
  ;; change uuid to timestamp
  (setq org-id-method 'ts)
  (add-hook 'org-mode-hook (lambda ()
                             ;; (setq fill-column 120)
                             (display-line-numbers-mode 0)))
  )

;;; org-roam
(use-package! org-roam
  ;; :defer t
  :config
  (setq org-roam-complete-everywhere t)
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))
  ;;org roam ugly hack for yas error
  (set-file-template! "/roam/.+\\.org$" 'org-mode :ignore t)
  )

(map!
 :map org-mode-map
 "C-M-i"  #'completion-at-point
 :map doom-leader-notes-map
 (:prefix ("r" . "roam")
  :desc "go back" "b" #'org-mark-ring-goto)
 )

;;; org roam dynamic agenda file
;; stolen from https://emacs-china.org/t/org-roam/15659
(with-eval-after-load 'org-roam
  (defvar dynamic-agenda-files nil
    "dynamic generate agenda files list when changing org state")

  (defun update-dynamic-agenda-hook ()
    (let ((done (or (not org-state) ;; nil when no TODO list
                    (member org-state org-done-keywords)))
          (file (buffer-file-name))
          (agenda (funcall (ad-get-orig-definition 'org-agenda-files)) ))
      (unless (member file agenda)
        (if done
            (save-excursion
              (goto-char (point-min))
              ;; Delete file from dynamic files when all TODO entry changed to DONE
              (unless (search-forward-regexp org-not-done-heading-regexp nil t)
                (customize-save-variable
                 'dynamic-agenda-files
                 (cl-delete-if (lambda (k) (string= k file))
                               dynamic-agenda-files))))
          ;; Add this file to dynamic agenda files
          (unless (member file dynamic-agenda-files)
            (customize-save-variable 'dynamic-agenda-files
                                     (add-to-list 'dynamic-agenda-files file)))))))

  (defun dynamic-agenda-files-advice (orig-val)
    (cl-union orig-val dynamic-agenda-files :test #'equal))

  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
  (add-to-list 'org-after-todo-state-change-hook 'update-dynamic-agenda-hook t))

(use-package! websocket
  :defer t)

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

;;;  org-latex-preview
(add-hook 'org-mode-hook 'org-fragtog-mode)
;;set latex preview default process
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '(
        (dvipng :programs
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


;;; org-mode-pretty-src
(with-eval-after-load 'org
  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  ;; (defun rasmus/org-prettify-src--update ()
  ;;   (let ((case-fold-search t)
  ;;         (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
  ;;         found)
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (while (re-search-forward re nil t)
  ;;         (goto-char (match-end 0))
  ;;         (let ((args (org-trim
  ;;                      (buffer-substring-no-properties (point)
  ;;                                                      (line-end-position)))))
  ;;           (when (org-string-nw-p args)
  ;;             (let ((new-cell (cons args rasmus/ob-header-symbol)))
  ;;               (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
  ;;               (cl-pushnew new-cell found :test #'equal)))))
  ;;       (setq prettify-symbols-alist
  ;;             (cl-set-difference prettify-symbols-alist
  ;;                                (cl-set-difference
  ;;                                 (cl-remove-if-not
  ;;                                  (lambda (elm)
  ;;                                    (eq (cdr elm) rasmus/ob-header-symbol))
  ;;                                  prettify-symbols-alist)
  ;;                                 found :test #'equal)))
  ;;       ;; Clean up old font-lock-keywords.
  ;;       (font-lock-remove-keywords nil prettify-symbols--keywords)
  ;;       (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
  ;;       (font-lock-add-keywords nil prettify-symbols--keywords)
  ;;       (while (re-search-forward re nil t)
  ;;         (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.
       `prettify-symbols-mode' is used because it has uncollpasing. It's
       may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq rasmus/org-at-src-begin -1))
        ;; (rasmus/org-prettify-src--update)
        )
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  ;; This function helps to produce a single glyph out of a
  ;; string. The glyph can then be used in prettify-symbols-alist.
  ;; This function was provided by Ihor in the org-mode mailing list.
  (defun yant/str-to-glyph (str)
    "Transform string into glyph, displayed correctly."
    (let ((composition nil))
      (dolist (char (string-to-list str)
                    (nreverse (cdr composition)))
        (push char composition)
        (push '(Br . Bl) composition))))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?➤) ;; ⎡ ➤ 🖝 ➟ ➤ ✎
                               ;; multi-character strings can be used with something like this:
                               ;; ("#+begin_src" . ,(yant/str-to-glyph "```"))
                               ("#+end_src"   . ?➤) ;; ⎣ ✐
                               ;; ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_quote" . ?«)
                               ("#+end_quote" . ?»)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))

;;@@ lsp-mode and company-mode
(use-package! lsp-mode
  :config
  (setq lsp-enable-file-watchers nil)         ;; performance matters
  (setq lsp-keep-workspace-alive nil)         ;; auto kill lsp server
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-auto-guess-root t)                ;; Yes, I'm using projectile
  (setq lsp-modeline-code-actions-enable nil) ;; keep modeline clean
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-enable-indentation t)           ;; don't change my code without my permission
  (setq lsp-modeline-diagnostics-enable nil)  ;; as above
  (setq lsp-eldoc-enable-hover t)          ;; disable eldoc hover
  (setq lsp-log-io nil)                       ;; debug only,
  (add-hook 'doom-first-input-hook #'lsp-deferred)
  )

;;disable lsp-format-buffer in cc-mode, it doesn't running well according to .clang-format
;;usage: https://docs.doomemacs.org/latest/modules/editor/format/#:~:text=To%20disable%20this%20behavior%20in%20one%20mode%3A%20(setq%2Dhook!%20%27python%2Dmode%2Dhook%20%2Bformat%2Dwith%2Dlsp%20nil)
(setq-hook! 'c++-mode-hook +format-with-lsp nil)
(setq-hook! 'c-mode-hook +format-with-lsp nil)

;;set company tab complete motion
(after! company
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection)
  )
(map! :map lsp-mode-map  "<tab>"  #'company-indent-or-complete-common)

;;@@ csharp
(add-hook 'csharp-mode-hook #'(lambda ()
                                (c-set-offset 'func-decl-cont 0)
                                (c-set-offset 'statement-cont 0)
                                (c-set-offset 'topmost-intro-cont 0)))

;;@@ +utils
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'(lambda ()
                              (add-to-list 'Info-directory-list
                                           (expand-file-name "~/Documents/info"))
                              (add-to-list 'Info-directory-list
                                           (expand-file-name "/var/guix/profiles/per-user/root/current-guix/share/info/"))))

(use-package! wakatime-mode
  :config
  (global-wakatime-mode)
  )

(use-package! systemd
  :defer t)

(use-package! treemacs
  ;; :defer t
  :config
  (setq treemacs-width 25)
  )

;;; org-capture-templates
(use-package! doct
  :commands doct)

;; https://github.com/VitalyAnkh/config/blob/adcd0ab0c679b108cfb9402b9e024556890379d5/doom/config.el#L2035
(setq org-capture-templates
      (doct `(("Personal todo" :keys "t"
               :icon ("checklist" :set "octicon" :color "green")
               :file +org-capture-todo-file
               :prepend t
               :headline "Inbox"
               :type entry
               :template ("* TODO %?"
                          "%i %a"))
              ;; ("Personal note" :keys "n"
              ;;  :icon ("sticky-note-o" :set "faicon" :color "green")
              ;;  :file +org-capture-todo-file
              ;;  :prepend t
              ;;  :headline "Inbox"
              ;;  :type entry
              ;;  :template ("* %?"
              ;;             "%i %a"))
              ;; ("Personal journal" :keys "j"
              ;;  :icon ("checklist" :set "octicon" :color "yellow")
              ;;  :file +org-capture-journal-file
              ;;  :type entry
              ;;  :prepend t
              ;;  :target (file+olp+datatree +org-capture-journal-file)
              ;;  :type entry
              ;;  :template ("* %U"
              ;;             "%i %a"
              ;;             "%?"
              ;;             ))
              ("Email" :keys "e"
               :icon ("envelope" :set "faicon" :color "blue")
               :file +org-capture-todo-file
               :prepend t
               :headline "Inbox"
               :type entry
               :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                          "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                          "about %^{topic}"
                          "%U %i %a"))
              ("Interesting" :keys "i"
               :icon ("eye" :set "faicon" :color "lcyan")
               :file +org-capture-todo-file
               :prepend t
               :headline "Interesting"
               :type entry
               :template ("* [ ] %{desc}%? :%{i-type}:"
                          "%i %a")
               :children (("Webpage" :keys "w"
                           :icon ("globe" :set "faicon" :color "green")
                           :desc "%(org-cliplink-capture) "
                           :i-type "read:web")
                          ("Article" :keys "a"
                           :icon ("file-text" :set "octicon" :color "yellow")
                           :desc ""
                           :i-type "read:reaserch")
                          ("Information" :keys "i"
                           :icon ("info-circle" :set "faicon" :color "blue")
                           :desc ""
                           :i-type "read:info")
                          ("Idea" :keys "I"
                           :icon ("bubble_chart" :set "material" :color "silver")
                           :desc ""
                           :i-type "idea")))
              )))
;; ;;; scheme geiser

;;; consult-org-roam
(use-package! consult-org-roam
  :after org-roam
  :config
  (consult-org-roam-mode t))

;;; graphviz-dot-mode
(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

;;; clang-fotmat+
(use-package! clang-format+
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode)
  (setq clang-format+-context 'modification)
  (setq clang-format+-always-enable t))

;; ;;; python pyright
(use-package! lsp-pyright
  :after lsp-mode
  :config
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/Clone/python-type-stubs"))
  )

;;could be defer org-protocl will be wake up
(use-package! org-protocol
  ;; :defer t
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-find-file" :protocol "find-file" :function org-protocol-find-file :kill-client nil))
  (defun org-protocol-find-file (fname)
    "Process org-protocol://find-file?path= style URL."
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file f)
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))

(use-package! graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package! indent-bars
  :hook ((python-mode yaml-mode) . indent-bars-mode))

(after! racket-mode
  (add-hook 'racket-mode-hook #'racket-smart-open-bracket-mode))

(use-package! doom-modeline
  :config
  (setq doom-modeline-env-enable-python nil))

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; undo-fu
(use-package! undo-fu
  :hook (doom-first-buffer . undo-fu-mode)
  :config
  (setq undo-limit 80000000))

(use-package! copilot
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
(add-to-list 'copilot-major-mode-alist '("python" . "python")
             (add-to-list 'copilot-major-mode-alist '("c" . "c"))
             )

;; (with-eval-after-load 'org
;;   (setq org-M-RET-may-split-line '((default . t))))

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
      (apply fn args)))
  )

(after! company
  (setq company-idle-delay 0.01))

;; (defun start-lsp-pylance ()
;;   "Start lsp pyright with pdm."
;;   (require 'lsp-pyright)
;;   (when (zerop (shell-command "pdm info"))
;;     (setq-local
;;      lsp-pyright-python-executable-cmd
;;      (string-trim (shell-command-to-string
;;                    "pdm info --python")))
;;     ;; (setq-local flycheck-python-flake8-executable lsp-pyright-python-executable-cmd)
;;     (setq-local python-shell-interpreter lsp-pyright-python-executable-cmd)
;;     (let ((ppath (concat (string-trim (shell-command-to-string
;;                                        "pdm info --package"))
;;                          "/lib")))
;;       (setq-local
;;        lsp-pyright-extra-paths
;;        (vector ppath)
;;        python-shell-extra-pythonpaths (list ppath))))
;;   (lsp))
;; ;; (lsp-inlay-hints-mode 1)

;; (defun nasy/lsp--render-string (str language)
;;   "Render STR using `major-mode' corresponding to LANGUAGE.
;;  When language is nil render as markup if `markdown-mode' is loaded."
;;   (setq str (s-replace "\r" "" (or str "")))
;;   (setq str (s-replace-regexp "<!--.*-->" "" (or str "")))
;;   (if-let ((mode (-some (-lambda ((mode . lang))
;;                           (when (and (equal lang language) (functionp mode))
;;                             mode))
;;                         lsp-language-id-configuration)))
;;       (lsp--fontlock-with-mode str mode)
;;     str))

;; (use-package lsp-pylance
;;   :init (require 'lsp-pylance)
;;   :hook (python-mode . start-lsp-pylance)
;;   )

;; (after! lsp
;;   (advice-add lsp--render-string :override
;;               nasy/lsp--render-string))

(defun kill-ring-save-no-newline ()
  "Save the region to the kill ring without newline characters."
  (interactive)
  (kill-new (replace-regexp-in-string "\n" "" (filter-buffer-substring (region-beginning) (region-end)))))

(bind-key "C-c M-w" 'kill-ring-save-no-newline)

;; (use-package jieba
;;   :commands jieba-mode
;;   )

(run-with-idle-timer 30 t #'recentf-save-list)

;;@@ cns
(use-package! cns
  :config
  (let ((repodir (concat doom-local-dir "straight/repos/emacs-chinese-word-segmentation/")))
    (setq cns-prog (concat repodir "cnws")
          cns-dict-directory (concat repodir "cppjieba/dict")))
  :hook
  (find-file . cns-auto-enable))

;; (use-package! nano-vertico
;;   :init
;;   (nano-vertico-mode 1))

(set-file-template! "\\.h$" :trigger "__h" :mode 'c-mode)



;; Local Variables:
;; eval: (when (fboundp 'set-init-outline-minor-mode) (set-init-outline-minor-mode))
;; End:
