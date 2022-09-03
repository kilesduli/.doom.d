;;; ../dotfiles/doomemacs/.doom.d/lisp/+meow-keybing.el -*- lexical-binding: t; -*-

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
(map! :leader
      :desc "help"                         "h"   help-map
      :after projectile :desc "project" "p" projectile-command-map
      :after projectile :desc "project-search(fd)" "p s" #'+default/search-project
      (:after org :desc "Outline" "n O" #'org-ol-tree)
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
