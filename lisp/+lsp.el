;;; ../dotfiles/doomemacs/.doom.d/lisp/+lsp.el -*- lexical-binding: t; -*-

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

;;set company tab complete motion
(after! company
  (map! :map company-active-map "<tab>"  #'company-complete-selection)
  (map! :map company-active-map "TAB"  #'company-complete-selection)
  )
(map! :map lsp-mode-map  "<tab>"  #'company-indent-or-complete-common)
