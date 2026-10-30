# Repository Guidelines

## Project Structure & Module Organization

This repository contains a personal Doom Emacs configuration. `doom.org` is the authoritative, documented source. Org Babel generates `init.el` (enabled modules), `packages.el` (package declarations), and `config.el` (runtime configuration). Do not edit these generated files directly.

`custom.el` stores Emacs Customize settings. `snippets/org-mode/` and `snippets/graphviz-dot-mode/` contain Yasnippet templates. `splash/` holds dashboard images and an accompanying `README.org`; the root `README.org` introduces the configuration.

## Build, Test, and Development Commands

- In Emacs, open `doom.org` and run `M-x org-babel-tangle` (`C-c C-v t`) to regenerate the Lisp files.
- Run `doom sync` with your installed Doom CLI after changing module selections or package declarations, then restart Emacs.
- For runtime configuration changes, restart Emacs or use `M-x doom/reload` after tangling.
- Run `git diff --check` to catch whitespace errors, and inspect `git diff` before committing.

There is no standalone application build or repository-local test runner. Development requires an existing Doom Emacs installation.

## Coding Style & Naming Conventions

Keep configuration changes and their explanations together in the appropriate `doom.org` section. Emacs Lisp blocks default to `config.el`; use explicit `:tangle init.el` or `:tangle packages.el` where appropriate. Preserve `:tangle no` on inactive examples.

Use standard Emacs Lisp indentation with spaces, aligning forms with Emacs indentation commands. Follow existing `zz/` prefixes for related custom helpers and descriptive, hyphenated names. Preserve snippet headers (`name`, `key`) and organize snippets by major mode. No repository-specific formatter or linter is configured.

## Testing Guidelines

No automated test framework or coverage threshold is defined. After configuration changes, tangle, inspect the generated diff, and check startup in Doom Emacs. Exercise the affected command, keybinding, capture template, or snippet. For visual changes, inspect the dashboard or relevant buffer. Record validation steps and any missing external dependencies in the pull request.

## Commit & Pull Request Guidelines

Recent commits use short, descriptive subjects such as “Added d2-mode” and “Improve desktop loading”; follow that style. Keep each change focused and include generated Lisp updates with their literate source. Pull requests should explain the behavior change, reference relevant issues, and describe validation. Include screenshots for visible interface or splash-image changes. Avoid committing credentials or unrelated machine-specific Customize changes.
