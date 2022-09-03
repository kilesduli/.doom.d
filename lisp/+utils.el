;;; ../dotfiles/doomemacs/.doom.d/lisp/+utils.el -*- lexical-binding: t; -*-

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
