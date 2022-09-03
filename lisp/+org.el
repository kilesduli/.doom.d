;;; ../dotfiles/doomemacs/.doom.d/lisp/+org.el -*- lexical-binding: t; -*-

;;org roam ugly hack
(set-file-template! "/roam/.+\\.org$" 'org-mode :ignore t)

;;stolen from v
(setq org-id-method 'ts)


(use-package! org-roam
  :custom
  (org-roam-complete-everywhere t)
  :config
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))
  )

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

(map!
 :map org-mode-map
 "C-M-i"  #'completion-at-point
 :map doom-leader-notes-map
 (:prefix ("r" . "roam")
  :desc "go back" "b" #'org-mark-ring-goto)
 )

;;org-mode 只补全 ascii 字符
(progn
  (push 'company-dabbrev-char-regexp company-backends)
  (setq company-dabbrev-char-regexp "[\\.0-9a-zA-Z-_'/]")
  (set-company-backend! 'org-mode
    'company-dabbrev-char-regexp 'company-yasnippet))

;;disable line number on org mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

                                        ;latex
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
