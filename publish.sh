#!/usr/bin/env bash

repo_root=$(git rev-parse --show-toplevel)
htmlize_dir=$(dirname "$(find "${repo_root}" -type f -name htmlize.el)")

emacs --batch \
      --directory "${htmlize_dir}" \
      --directory "$HOME/.emacs.d/site-lisp/org-mode/lisp" \
      --load "$HOME/.emacs.d/elisp/k20e-org-html-export.el" \
      --visit "$HOME/.emacs.d/custom.org" \
      --execute '(org-html-export-to-html)' > /dev/null 2>&1
git checkout gh-pages
mv custom.html index.html
git add index.html
git commit --message "post-commit hook regeneration."
git checkout master
git push origin --all
