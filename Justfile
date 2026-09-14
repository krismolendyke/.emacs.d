# List available recipes
default:
    @just --list

# Clean base16 packages from elpa, native compilation artifacts from eln-cache, and custom-file entries
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "==> Purging base16 packages from elpa/..."
    rm -rf elpa/*base16*

    echo "==> Purging base16 artifacts from eln-cache/..."
    if [ -d eln-cache ]; then
        find eln-cache -name "*base16*" -delete 2>/dev/null || true
    fi

    echo "==> Purging base16 entries from .emacs-custom.el..."
    if [ -f .emacs-custom.el ]; then
        emacs --batch --eval '
        (let ((file (expand-file-name ".emacs-custom.el" user-emacs-directory)))
          (when (file-exists-p file)
            (with-temp-file file
              (insert-file-contents file)
              (goto-char (point-min))
              (while (re-search-forward "[ \t\n]*base16-theme" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "[ \t\n]*\"92ef141d05dc078cfaae83a4d87d7d3ddbe7b30591e63ba19a57d1d35f9ab23f\"" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "[ \t\n]*\"6ed98f47da7556a8ce6280346e5d8e1e25bede71dc5186aa2654b93bec42d2a6\"" nil t)
                (replace-match ""))))
            (message "Successfully cleaned .emacs-custom.el"))'
    fi

    echo "==> Done. All base16 remnants removed."

# Non-destructive tangle + byte-compile lint (as documented in AGENTS.md)
lint:
    #!/usr/bin/env bash
    set -euo pipefail
    PROBE_ORG=$(mktemp /tmp/probe-XXXXXX.org)
    PROBE_EL="${PROBE_ORG%.org}.el"
    PROBE_ELC="${PROBE_ORG%.org}.elc"
    trap 'rm -f "$PROBE_ORG" "$PROBE_EL" "$PROBE_ELC"' EXIT

    echo "==> Tangling custom.org..."
    cp custom.org "$PROBE_ORG"
    emacs -Q --batch --eval "(progn (require 'org) (org-babel-tangle-file \"$PROBE_ORG\"))"

    echo "==> Byte-compiling tangled output..."
    emacs --batch --eval '(package-initialize)' \
          --eval "(add-to-list 'load-path (expand-file-name \"elisp\" user-emacs-directory))" \
          -f batch-byte-compile "$PROBE_EL"

# Regenerate published HTML (as documented in AGENTS.md)
publish:
    #!/usr/bin/env bash
    set -euo pipefail
    HTMLIZE_DIR=$(dirname "$(find ~/.emacs.d/elpa -name htmlize.el 2>/dev/null | head -1)")
    if [ -z "$HTMLIZE_DIR" ]; then
        echo "Error: htmlize package not found in elpa/" >&2
        exit 1
    fi
    echo "==> Exporting custom.org to custom.html..."
    emacs --batch \
          --directory "$HTMLIZE_DIR" \
          --load ~/.emacs.d/elisp/k20e-org-html-export.el \
          --visit ~/.emacs.d/custom.org \
          --execute '(org-html-export-to-html)'
    echo "==> Export complete: custom.html"
