;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;;(package! meow)
(package! disable-mouse)
(package! org-fragtog)
(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))
(package! info-colors)
(package! systemd)
(package! rotate)
(package! ox-gfm)
(unpin! org)
(package! doct)
(package! wakatime-mode :recipe
  (:host github :repo "Borwe/wakatime-mode"))
(package! ob-csharp :recipe (:host github :repo "samwdp/ob-csharp"))
;; (package! consult-org-roam)
(package! clang-format+
  :recipe (:host github :repo "SavchenkoValeriy/emacs-clang-format-plus"))
(package! benchmark-init)
(unpin! lsp-mode)
(unpin! lsp-ui)
(package! ef-themes)
(package! kaolin-themes)
(package! htmlz-mode :recipe (:host github :repo "kilesduli/htmlz-mode"))
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))
(package! anaconda-mode :disable t)
(package! setup)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! cns
  :recipe (:host github :repo "kanglmf/emacs-chinese-word-segmentation"
           :pre-build ("make")
           :files ("cppjieba" "cnws" "*.el")))
(package! outli
  :recipe (:host github :repo "jdtsmith/outli"))
(package! denote
  :recipe (:host github :repo "protesilaos/denote"))
(package! beframe
  :recipe (:host github :repo "protesilaos/beframe"))
(package! emacs-lorem-ipsum
  :recipe (:host github :repo "jschaf/emacs-lorem-ipsum"))
(package! denote-menu
  :recipe (:host github :repo "namilus/denote-menu"))
(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime"
           :pre-build ("make" "LIBRIME_ROOT=~/.nix-profile/" "EMACS_MODULE_HEADER_ROOT=~/.nix-profile/include" "lib")
           :files ("librime-emacs.so" "*.el")))
(unpin! consult)
(unpin! lsp-java)
(package! annotate)
(package! dogears)
(unpin! org-noter)
(package! uniline
  :recipe (:host github :repo "tbanel/uniline"))
(package! emacs-buffer-name-relative
  :recipe (:host codeberg :repo "ideasman42/emacs-buffer-name-relative"))
(package! typst
  :recipe (:host codeberg :repo "meow_king/typst-ts-mode"))
(package! treesit-langs
  :recipe (:host github :repo "emacs-tree-sitter/treesit-langs"))
(package! org-super-agenda
  :recipe (:host github :repo "alphapapa/org-super-agenda"))

;; If you want to replace it with yasnippet's default snippets
;;(package! yasnippet-snippets)

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
