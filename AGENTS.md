# AGENTS.md

Guidance for coding agents working in this repository.

## What this repo is

A personal, **literate** GNU Emacs configuration (Emacs 31 via `emacs-plus@31` on
macOS). `init.el` is only a bootstrap — essentially all configuration lives in
`custom.org` as Org Mode prose plus `emacs-lisp` source blocks. The same file is
also published as a website (GitHub Pages), so prose quality and heading
structure are part of the deliverable, not decoration.

## Load pipeline

Understanding this order is required before editing anything:

1. **`init.el`** (`k20e/…` bootstrap functions, run in sequence at the bottom of
   the file): disables tool/scroll/menu bars, adds `elisp/` to `load-path`, adds
   MELPA, runs `exec-path-from-shell` (which env vars get imported is listed
   there), then `load`s **every** `.el` file in `elisp/`, then registers
   `k20e/after-init-hook`.
2. **`after-init-hook`** → `k20e/load-custom-org` calls `org-babel-load-file` with
   `COMPILE` on **every** root-level `*.org` file: tangles all `emacs-lisp`
   blocks into `custom.el`, byte-compiles to `custom.elc`, loads it.
3. `k20e/restore-desktop` restores a `desktop` session from `~/Google`
   (`k20e/google-drive-directory`), if that directory exists.

Consequences that matter:

- **`custom.el` and `custom.elc` are generated and gitignored.** Never edit them —
  changes are silently discarded on the next start. Edit `custom.org`.
- Any new root-level `.org` file becomes part of the loaded configuration.
- Because the tangled output is byte-compiled, byte-compiler warnings surface in
  `*Warnings*` on every start. The established fix for "reference to free
  variable" on a package's own defcustoms is a bare `(defvar foo)` inside
  `:init` or inside `with-eval-after-load`, before the `setq` (see the `gcmh`
  and native-comp blocks, and commits `925e3ce` / `ebbc1f8`).
- Everything in `elisp/` loads unconditionally and alphabetically, so both
  `pragmatapro-prettify-symbols-v0.829.el` and `v0.830.el` are loaded; the
  *active* one is chosen by an explicit `load` in the `prettify-symbols-mode`
  block of `custom.org`.
- `.emacs-custom.el` is `custom-file` and is gitignored. Customize-written
  settings land there and are invisible to the published document — prefer
  explicit blocks in `custom.org`.

## Editing `custom.org`

- One top-level heading per package or topic; roughly alphabetical after the
  leading meta sections (Build Emacs, How this works, use-package, Performance
  Tuning, key bindings, k20e Defaults, k20e Custom Functions). Put configuration
  in an `emacs-lisp` block under the matching heading, with prose above
  explaining *why* — link to the upstream issue/blog post that motivated it, as
  the existing sections do.
- File-level `#+PROPERTY: header-args:emacs-lisp :tangle yes` means every
  `emacs-lisp` block is live config. Blocks in other languages default to
  `:tangle no` and are documentation only (e.g. the `brew install` and `defaults
  write` snippets).
- Configure through `use-package` consistently. Built-ins get a bare
  `(use-package foo)` "require" block followed by a `(use-package emacs :config
  …)` block that sets the actual values.
- Custom functions and variables use the `k20e/` prefix.
- TODO keywords in headings are real worklist state (`* TODO combobulate`,
  `** STARTED [1/3] …`) and are exported to the site. Don't strip or
  "clean up" them incidentally.

## Commands

There is no build system; verification is done in batch Emacs. Work on a copy so
the checks never touch the committed/generated files:

```sh
# Non-destructive tangle + byte-compile lint (the closest thing to a test suite).
# Tangling writes the .el next to the .org, so copy it out first.
cp custom.org /tmp/probe.org
emacs -Q --batch --eval "(progn (require 'org) (org-babel-tangle-file \"/tmp/probe.org\"))"
emacs --batch --eval '(package-initialize)' \
      --eval "(add-to-list 'load-path (expand-file-name \"elisp\" user-emacs-directory))" \
      -f batch-byte-compile /tmp/probe.el
```

The config is otherwise warning-clean: the only two expected warnings are free
references to `k20e/google-drive-directory` and `k20e/elisp-directory`, which are
defined in `init.el` and so are invisible when compiling the tangled file alone.
Any *other* warning is a regression worth fixing before committing.

```sh
# Regenerate the published HTML (writes custom.html beside custom.org).
emacs --batch \
      --directory "$(dirname "$(find ~/.emacs.d/elpa -name htmlize.el | head -1)")" \
      --load ~/.emacs.d/elisp/k20e-org-html-export.el \
      --visit ~/.emacs.d/custom.org \
      --execute '(org-html-export-to-html)'
```

Interactively, export with `C-c C-e h h` from `custom.org`. To smoke-test a
change end to end, restart Emacs (or `emacs --debug-init`) and read `*Warnings*`.

## Publishing the site

- `custom.html` **is committed**. `.github/workflows/static.yml` copies it to
  `index.html` and deploys Pages on every push to `master`. Commits that only
  regenerate the HTML are conventionally titled `publish`, separate from the
  content commit that changed `custom.org`.
- `custom.org` still documents a `.git/hooks/post-commit` hook that exported the
  HTML and committed it to a `gh-pages` branch. **That hook is not installed and
  there is no `gh-pages` branch** — the Actions workflow replaced it, and
  regeneration is now manual. Treat that section as historical.
- Export settings live in `elisp/k20e-org-html-export.el`, which must stay
  loadable in a bare batch Emacs (it bootstraps `use-package`/`org`/`htmlize`
  itself and must not depend on `custom.org`).
- `org.css` is read and inlined into `<head>` at export time by
  `k20e/update-org-css`. Restyling the site means editing `org.css` and
  re-exporting; the export also pulls Tufte CSS, Font Awesome, and Google Fonts
  from CDNs via `k20e/org-html-head-extra`.

## Other layout notes

- `elisp/` — hand-written Lisp, loaded wholesale at init (see caveat above).
- `snippets/` — tracked yasnippet files, but yasnippet is no longer configured
  anywhere; vestigial.
- `site-lisp/` — gone from disk, but the four `site-lisp/*` paths are **still
  committed on `master` as submodule gitlinks** (mode `160000`); `8697b30`
  dropped `.gitmodules` and the checkouts without removing the tree entries.
  `git status` therefore reports them as unstaged deletions forever, while
  `jj st` looks clean because jj cannot represent submodules and ignores those
  paths. Nothing loads from there, and the Pages workflow checks out with
  `submodules: false`, so this is cosmetic — but don't be misled by the
  disagreement between the two tools, and clearing it needs a real
  `git rm --cached site-lisp/*` commit.
- Runtime/state directories (`elpa/`, `eln-cache/`, `tree-sitter/`, `backup/`,
  `org-persist/`, `eca/`, `transient/`, `auto-save-list/`, `tramp`) are
  gitignored generated state — don't commit or hand-edit them.
