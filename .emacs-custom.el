(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-blue))
 '(custom-safe-themes
   '("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "82d2cac368ccdec2
fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(org-latex-pdf-process '("latexmk -g -pdf -pdflatex=\"%latex\" -outdir=%o %f"))
 '(package-selected-packages
   '(ox-gfm ebib ox-tufte git-commit smart-mode-line avy cargo flycheck-rust tide color-theme-sanityinc-tomorrow flx yasnippet yaml-mode web-mode typescript-mode toml-mode toml smex rust-mode paredit paradox pallet multiple-cursors multi-term markdown-mode keyfreq htmlize highlight-parentheses groovy-mode go-mode gitignore-mode gitconfig-mode git-timemachine geiser flycheck-yamllint find-file-in-project expand-region exec-path-from-shell counsel buffer-move arduino-mode ansible-doc ag))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((org-inline-image-overlays)
     (org-latex-caption-above)
     (org-hide-emphasis-markers . t)
     (org-hide-macro-markers . t)
     (org-fontify-quote-and-verse-blocks . t)
     (eval org-sbe "latex-link")
     (eval org-sbe "latex-opt-link")
     (eval org-sbe "jk-keywords")
     (eval org-sbe "pdf-process-bibtex")
     (eval org-sbe "ngz-nbsp")
     (eval org-sbe "latex-filter-footcites")
     (eval org-sbe "biblatex-cite-link")
     (eval org-sbe "biblatex-textcite-link")
     (eval org-sbe "biblatex-parencite-link")
     (eval org-sbe "biblatex-sidecite-link")
     (eval org-sbe "biblatex-multicite-link")
     (eval org-sbe "biblatex-footcite-link")
     (eval org-sbe "tufte-ebib-setup")
     (eval org-sbe "tufte-handout")
     (eval org-sbe "tufte-book")
     (eval org-sbe "user-entities")
     (eval ox-extras-activate
           '(ignore-headlines))
     (eval require 'ox-gfm)
     (eval require 'ox-extra)
     (eval require 'ox-tufte-latex)))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
