(require 'flx-ido)
(require 'ido)
(require 'ido-vertical-mode)

;; Boring arrows be gone!
(setq ido-vertical-decorations '("\n ► "    ; left bracket around prospect list
                                 ""         ; right bracket around prospect list
                                 "\n   "    ; separator between prospects, depends on `ido-separator`
                                 "\n   …"   ; inserted at the end of a truncated list of prospects
                                 "["        ; left bracket around common match string
                                 "]"        ; right bracket around common match string
                                 " ✘"       ; no match
                                 " ✔"       ; matched
                                 " [Not readable]"
                                 " [Too big]"
                                 " ?"       ; confirm
                                 "\n ► "    ; left bracket around the sole remaining completion
                                 ""         ; right bracket around the sole remaining completion
                                 ))

(add-hook 'ido-minibuffer-setup-hook
          #'(lambda ()
              "Bump up minibuffer text size and height."
              (text-scale-set 3)
              (setq max-mini-window-height 20)))

(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(flx-ido-mode t)

(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-show-dot-for-dired t
      ido-max-file-prompt-width 0.2
      ido-use-faces nil)

(provide 'setup-ido)
