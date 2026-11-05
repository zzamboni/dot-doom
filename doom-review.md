# Doom configuration review

Reviewed on 2026-11-04 against the installed Emacs 30.2, Org 9.8.9,
org-gtd 4.0.7, Doom core `59cdaa32`, and modules `897f81544`.

## Organization and cleanup

`doom.org` now follows the workflows it configures: maintenance, modules, editor
settings, navigation, Org editing, tasks, publishing, automation, programming,
and external tools. Package declarations remain next to their runtime settings.
The table of contents and generated Lisp files have been regenerated.

Removed the disabled module catalog, obsolete template instructions, abandoned
Org Download configuration, disabled exporter/theme experiments, and the empty
“Other Org stuff” section. These remain recoverable from Git history. Rewrote
prose to describe the active configuration, including external dependencies,
local development recipes, and the extra `~/.slynkrc` tangle target.

No enabled Doom modules or active package capabilities were removed. The explicit
Org Appear declaration and duplicate hook were unnecessary: `(org +pretty)`
already installs and enables it. Removed the unused
`+org-roam-open-buffer-on-find-file` assignment without enabling automatic
backlinks; the current setting is `+org-roam-auto-backlinks-buffer`.

## Compatibility repairs

- **GTD:** capture templates referenced the missing
  `org-gtd-inbox-file-basename` variable. They now use the public
  `org-gtd-inbox-path` function. `C-c d s` and `C-c d f` retain their purposes
  through `org-gtd-reflect-stuck-projects` and `org-gtd-organize`. Updated agenda
  property settings to the `org-gtd-agenda-property-*` names. The installed
  package and its [upgrade manual](https://github.com/Trevoke/org-gtd.el/blob/master/doc/org-gtd.org)
  document the current APIs. The Outlook template is now conditional on macOS,
  matching its supporting package.
- **Reloads:** explicitly enable Org Super Agenda and Edna with argument `1`;
  repeated evaluation no longer toggles them off. Replaced the obsolete
  `org-edna-load` alias and removed the immediately overwritten GTD 2.1
  acknowledgment.
- **Chezmoi:** include `extensions/chezmoi-magit.el` in its package recipe and load
  it normally, eliminating a hard-coded Straight repository path. The extension
  loads with Magit or when its command is invoked.
- **Vterm:** discover Elvish using `executable-find`; the old
  `/usr/local/bin/elvish` does not exist here. The old path remains a fallback.
- **Doom:** replace the obsolete `doom-private-dir` alias with `doom-user-dir`.

Kept the Counsel path-deduplication override, the image-yank advice, deferred
export-menu setters, explicit ox-hugo installation, and existing export formats.
The installed sources still justify these. Existing helper names, anchors,
Hugo capture content, fonts, theme, and key assignments are preserved, except
for replacing the two obsolete GTD command targets with their current equivalents.

## Follow-up decisions left unchanged

- **GTD data migration:** the existing `4.0.0` acknowledgment only hides a warning.
  It does not prove that older task files were migrated. Review the v3-to-v4
  migration instructions if needed; this review does not modify task data.
- **Authentication:** the current `nreverse` puts `.netrc` ahead of
  `.authinfo.gpg` on this Linux installation, despite the old prose claiming
  otherwise. It also reverses priorities again on reload. Prefer an explicit
  ordered list once you decide the desired credential lookup order.
- **Session restore:** desktop persistence and Doom workspaces are both enabled.
  Choose one only if you no longer need the other's behavior.
- **Potential retirements:** Jira still has a placeholder URL. Org ML, Annotate,
  the CV/Leanpub checkouts, 750words, and macOS presentation automation may be
  occasional tools; their absence from other config references does not prove
  they are unused. They remain available.
- **Tangling and pins:** auto-tangling remains enabled by default in all Org
  buffers; documents can opt out. Emacs Everywhere remains explicitly unpinned.
  Making either more restrictive would change the existing policy.

## Validation

- Tangled the three repository Lisp files and checked parentheses and whitespace.
  Left the existing `~/.slynkrc` file untouched.
- Compared parsed Lisp forms before and after to distinguish reordering and
  comment cleanup from executable changes.
- Ran `doom sync -U`, rebuilding the Chezmoi recipe without upgrading packages.
- Verified a normal Doom daemon startup with desktop restoration disabled,
  explicitly loading Org, GTD, Magit, and the Chezmoi extension.
- Passed 16 fresh-session regression tests covering settings, Org hooks and
  effective keybindings, dispatch, backend loading, clipboard routing, real
  image attachment/figure export, GTD capture, Hugo capture/export, HTML export
  options, readable IDs, lexical closures, document macros, slide numbering,
  Counsel deduplication, Chezmoi, Elvish discovery, reload-safe mode enabling,
  speed keys, and PDF generation.
- Compiled a temporary copy of the cheat sheet with Typst (one landscape A4 page)
  and a sample document with Tectonic. The repository PDF is unchanged.

Test scripts, logs, baseline copies, and both failed and successful test artifacts
are retained in `/tmp/doom-review/`; the successful suite log is
`regression-4.log`. macOS application automation, live service calls, and every
language-server/compiler integration were not exercised on this Linux machine.
No commits were created.

## Follow-up: Hugo bundle assets (2026-11-05)

The initial Hugo smoke test lacked copied assets and missed a path-dependent
layout difference. In the installed ox-hugo, opening the website through
`~/Dropbox/Personal/` preserves `images/` and `attachments/` subdirectories;
opening it through the `~/Personal/` symlink can flatten them because ox-hugo
compares resolved asset paths against the unresolved buffer directory. Both
behaviors were reproduced without changing exporter versions. No explicit flat
layout setting existed before the cleanup.

The Hugo section now explicitly copies external bundle assets beside `index.md`,
preserving upstream handling of static files, existing bundle resources, and
non-bundle exports. `tests/hugo-bundle-layout.el` covers real and symlink paths,
images, PDF attachments, copy updates, and those exceptions. A fresh Doom session
also exported two real website posts through both paths to temporary sites: all
five checks passed. Logs and exported samples are in `/tmp/doom-hugo-layout/`.
The website's working tree was not modified; re-export after loading the fix.
