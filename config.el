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
;;; doom-theme
(setq doom-theme 'sanityinc-tomorrow-night)

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


;;; Menu
;;    basic
;;    org-mode
;;    lsp
;;    company
;;    a lot of package
;;    +utils
;;    unusing

;;; basic

;;; background transparent
;;(add-to-list 'default-frame-alist '(alpha-background . 98))

;;; doom-font
;; old one for wayland
;;(set-face-attribute 'default nil :height 120)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;(setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 33)
;;        doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
;;        doom-unicode-font (font-spec :family "LXGW Wenkai Mono" )
;;        doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 37)
;;        doom-serif-font(font-spec :family "CMU Typewriter Text" :weight 'light :size 37 ))

;;new one for X-org
(setq doom-font (font-spec :family "JetBrains Mono" :weight 'light :size 26)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text")
      doom-unicode-font (font-spec :family "LXGW Wenkai Mono" )
      doom-big-font (font-spec :family "JetBrains Mono" :weight 'light :size 26)
      doom-serif-font(font-spec :family "CMU Typewriter Text" :weight 'light :size 26))

;; redo
(global-set-key (kbd "C-r" ) 'undo-fu-only-redo)

;;; set input toggle C-SPC
(global-set-key (kbd "C-SPC") 'toggle-input-method)

;;; which-key-idle-delay
;; delay setting
(setq which-key-idle-delay 0.01)
(setq which-key-idle-secondary-delay 0.01)

;;; pixel-scroll-precision-mode
(pixel-scroll-precision-mode 1)

;;; centered-window-mode
;; enable if you want
;;(centered-window-mode t)

;;; wheel zoom
(global-set-key  [C-mouse-wheel-up-event]  'text-scale-increase)
(global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)

;;; map define key
(load! "lisp/+meow-keybings.el")
(load! "lisp/+meow-setup.el")
(defun set-useful-keybings()
  (define-key doom-leader-workspaces/windows-map (kbd "t") 'treemacs-select-window)
  (global-set-key (kbd "M-j") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "M-k") 'kmacro-end-or-call-macro)
  )

;;; meow
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

;;; rime setting
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

;;; org-mode
(use-package! org-ol-tree
  :commands org-ol-tree
  :config
  (setq org-ol-tree-ui-icon-set
        (if (and (display-graphic-p)
                 (fboundp 'all-the-icons-material))
            'all-the-icons
          'unicode))
  (org-ol-tree-ui--update-icon-set))
;;; org-mode extras
(progn
  (push 'company-dabbrev-char-regexp company-backends)
  (setq company-dabbrev-char-regexp "[\\.0-9a-zA-Z-_'/]")
  (set-company-backend! 'org-mode
    'company-dabbrev-char-regexp 'company-yasnippet))
;; ob-csharp
(load! "lisp/ob-csharp.el")             ; It's org-babel functions for csharp evaluation.

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
;; change uuid to timestamp
(setq org-id-method 'ts)
;; export to gfm
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

;;; org-roam
(use-package! org-roam
  :custom
  (org-roam-complete-everywhere t)
  :config
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))
  )

(use-package! websocket
  :after org-roam)

(map!
 :map org-mode-map
 "C-M-i"  #'completion-at-point
 :map doom-leader-notes-map
 (:prefix ("r" . "roam")
  :desc "go back" "b" #'org-mark-ring-goto)
 )

;;org roam ugly hack for yas error
(set-file-template! "/roam/.+\\.org$" 'org-mode :ignore t)


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


;;; org-mode-pretty-src unusing
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

;;; lsp-mode
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
  (lsp-eldoc-enable-hover t)          ;; disable eldoc hover
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
(setq-hook! 'c++-mode-hook +format-with-lsp nil)
(setq-hook! 'c-mode-hook +format-with-lsp nil)
;;(use-package clang-format+
;;  :config
;;  (add-hook 'c-mode-common-hook #'clang-format+-mode)
;;  (add-hook 'csharp-mode-hook #'clang-format+-mode)
;;  (setq clang-format+-context 'modification)
;;  (setq clang-format+-always-enable t)
;;  )

;;; company-mode
;;set company tab complete motion
(after! company
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection)
  )
(map! :map lsp-mode-map  "<tab>"  #'company-indent-or-complete-common)

;;; csharp
(add-hook 'csharp-mode-hook '(lambda()(c-set-offset 'func-decl-cont 0)
                               (c-set-offset 'statement-cont 0)
                               (c-set-offset 'topmost-intro-cont 0)))

;;; +utils
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package wakatime-mode
  :config
  (global-wakatime-mode)
  )

(after! magit
  (magit-delta-mode +1))

(use-package! systemd
  :defer t)

(use-package! treemacs
  :config
  (setq treemacs-width 25)
  )

;;; org-templates unusing
(use-package! doct
  :commands doct)
;; (defun set-org-capture-templates ()
;;       (setq org-capture-templates
;;             (doct `(("Personal todo" :keys "t"
;;                      :icon ("checklist" :set "octicon" :color "green")
;;                      :file +org-capture-todo-file
;;                      :prepend t
;;                      :headline "Inbox"
;;                      :type entry
;;                      :template ("* TODO %?"
;;                                 "%i %a"))
;;                     ("Personal note" :keys "n"
;;                      :icon ("sticky-note-o" :set "faicon" :color "green")
;;                      :file +org-capture-todo-file
;;                      :prepend t
;;                      :headline "Inbox"
;;                      :type entry
;;                      :template ("* %?"
;;                                 "%i %a"))
;;                     ("Personal journal" :keys "j"
;;                      :icon ("checklist" :set "octicon" :color "yellow")
;;                      :file +org-capture-journal-file
;;                      :type entry
;;                      :prepend t
;;                      :target (file+olp+datatree +org-capture-journal-file)
;;                      :type entry
;;                      :template ("* %U"
;;                                 "%i %a"
;;                                 "%?"
;;                                 ))
;;                     ("Email" :keys "e"
;;                      :icon ("envelope" :set "faicon" :color "blue")
;;                      :file +org-capture-todo-file
;;                      :prepend t
;;                      :headline "Inbox"
;;                      :type entry
;;                      :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
;;                                 "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
;;                                 "about %^{topic}"
;;                                 "%U %i %a"))
;;                     ("Interesting" :keys "i"
;;                      :icon ("eye" :set "faicon" :color "lcyan")
;;                      :file +org-capture-todo-file
;;                      :prepend t
;;                      :headline "Interesting"
;;                      :type entry
;;                      :template ("* [ ] %{desc}%? :%{i-type}:"
;;                                 "%i %a")
;;                      :children (("Webpage" :keys "w"
;;                                  :icon ("globe" :set "faicon" :color "green")
;;                                  :desc "%(org-cliplink-capture) "
;;                                  :i-type "read:web")
;;                                 ("Article" :keys "a"
;;                                  :icon ("file-text" :set "octicon" :color "yellow")
;;                                  :desc ""
;;                                  :i-type "read:reaserch")
;;                                 ("Information" :keys "i"
;;                                  :icon ("info-circle" :set "faicon" :color "blue")
;;                                  :desc ""
;;                                  :i-type "read:info")
;;                                 ("Idea" :keys "I"
;;                                  :icon ("bubble_chart" :set "material" :color "silver")
;;                                  :desc ""
;;                                  :i-type "idea")))
;;                     ("Tasks" :keys "k"
;;                      :icon ("inbox" :set "octicon" :color "yellow")
;;                      :file +org-capture-todo-file
;;                      :prepend t
;;                      :headline "Tasks"
;;                      :type entry
;;                      :template ("* TODO %? %^G%{extra}"
;;                                 "%i %a")
;;                      :children (("General Task" :keys "k"
;;                                  :icon ("inbox" :set "octicon" :color "yellow")
;;                                  :extra "")
;;                                 ("Task with deadline" :keys "d"
;;                                  :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
;;                                  :extra "\nDEADLINE: %^{Deadline:}t")
;;                                 ("Scheduled Task" :keys "s"
;;                                  :icon ("calendar" :set "octicon" :color "orange")
;;                                  :extra "\nSCHEDULED: %^{Start time:}t")))
;;                     ("Project" :keys "p"
;;                      :icon ("repo" :set "octicon" :color "silver")
;;                      :prepend t
;;                      :type entry
;;                      :headline "Inbox"
;;                      :template ("* %{time-or-todo} %?"
;;                                 "%i"
;;                                 "%a")
;;                      :file ""
;;                      :custom (:time-or-todo "")
;;                      :children (("Project-local todo" :keys "t"
;;                                  :icon ("checklist" :set "octicon" :color "green")
;;                                  :time-or-todo "TODO"
;;                                  :file +org-capture-project-todo-file)
;;                                 ("Project-local note" :keys "n"
;;                                  :icon ("sticky-note" :set "faicon" :color "yellow")
;;                                  :time-or-todo "%U"
;;                                  :file +org-capture-project-notes-file)
;;                                 ("Project-local changelog" :keys "c"
;;                                  :icon ("list" :set "faicon" :color "blue")
;;                                  :time-or-todo "%U"
;;                                  :heading "Unreleased"
;;                                  :file +org-capture-project-changelog-file)))
;;                     ("\tCentralised project templates"
;;                      :keys "o"
;;                      :type entry
;;                      :prepend t
;;                      :template ("* %{time-or-todo} %?"
;;                                 "%i"
;;                                 "%a")
;;                      :children (("Project todo"
;;                                  :keys "t"
;;                                  :prepend nil
;;                                  :time-or-todo "TODO"
;;                                  :heading "Tasks"
;;                                  :file +org-capture-central-project-todo-file)
;;                                 ("Project note"
;;                                  :keys "n"
;;                                  :time-or-todo "%U"
;;                                  :heading "Notes"
;;                                  :file +org-capture-central-project-notes-file)
;;                                 ("Project changelog"
;;                                  :keys "c"
;;                                  :time-or-todo "%U"
;;                                  :heading "Unreleased"
;;                                  :file +org-capture-central-project-changelog-file)))))))

;;     (set-org-capture-templates)

;;; sis-mode unusing
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
