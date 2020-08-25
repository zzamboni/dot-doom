;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq user-full-name "Diego Zamboni"
      user-mail-address "diego@zzamboni.org")

(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-right-option-modifier 'super)))

(setq kill-whole-line t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq confirm-kill-emacs nil)

(map! "C-x b" #'counsel-recentf)

;;(map! "C-s" #'counsel-grep-or-swiper)
(map! "C-s" #'+default/search-buffer)

(map! :after magit "C-c C-g" #'magit-status)

(use-package! visual-regexp-steroids
  :defer
  :config
  (require 'pcre2el)
  (setq vr/engine 'pcre2el)
  (map! "C-c s r" #'vr/replace)
  (map! "C-c s q" #'vr/query-replace))

(after! smartparens
  (defun zz/goto-match-paren (arg)
    "Go to the matching paren/bracket, otherwise (or if ARG is not
    nil) insert %.  vi style of % jumping to matching brace."
    (interactive "p")
    (if (not (memq last-command '(set-mark
                                  cua-set-mark
                                  zz/goto-match-paren
                                  down-list
                                  up-list
                                  end-of-defun
                                  beginning-of-defun
                                  backward-sexp
                                  forward-sexp
                                  backward-up-list
                                  forward-paragraph
                                  backward-paragraph
                                  end-of-buffer
                                  beginning-of-buffer
                                  backward-word
                                  forward-word
                                  mwheel-scroll
                                  backward-word
                                  forward-word
                                  mouse-start-secondary
                                  mouse-yank-secondary
                                  mouse-secondary-save-then-kill
                                  move-end-of-line
                                  move-beginning-of-line
                                  backward-char
                                  forward-char
                                  scroll-up
                                  scroll-down
                                  scroll-left
                                  scroll-right
                                  mouse-set-point
                                  next-buffer
                                  previous-buffer
                                  previous-line
                                  next-line
                                  back-to-indentation
                                  )))
        (self-insert-command (or arg 1))
      (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
            (t (self-insert-command (or arg 1))))))
  (map! "%" 'zz/goto-match-paren))

(setq doom-font (font-spec :family "Fira Code Retina" :size 16)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 18))

(use-package! mixed-pitch
  :defer
  :config
  (setq mixed-pitch-variable-pitch-cursor nil)
  :hook
  (text-mode . mixed-pitch-mode))

(setq doom-theme 'spacemacs-light)

;;(add-hook 'window-setup-hook #'doom/quickload-session)

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist '((top . 1) (left . 1) (width . 143) (height . 55)))

(add-to-list 'safe-local-variable-values
             '(eval add-hook 'after-save-hook
                    (lambda nil
                      (org-export-to-file 'awesomecv "zamboni-vita.tex"))
                    :append :local))

(setq org-directory "~/org/")

(setq org-hide-emphasis-markers t)

(setq org-insert-heading-respect-content nil)

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)

(setq org-use-speed-commands
      (lambda ()
        (and (looking-at org-outline-regexp)
             (looking-back "^\**"))))

(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode
           (lambda () (add-hook 'after-save-hook 'org-babel-tangle :append :local)))

(use-package! ox-leanpub
  :after org
  :config
  (require 'ox-leanpub-markdown)
  (org-leanpub-book-setup-menu-markdown))

(after! org
  (setq org-agenda-files
        '("~/gtd" "~/Work/work.org.gpg" "~/org/ideas.org" "~/org/projects.org" "~/org/diary.org")))

(defun zz/add-file-keybinding (key file &optional desc)
  (let ((key key)
        (file file)
        (desc desc))
    (map! :desc (or desc file) key (lambda () (interactive) (find-file file)))))

(zz/add-file-keybinding "C-c z w" "~/Work/work.org.gpg" "work.org")
(zz/add-file-keybinding "C-c z i" "~/org/ideas.org" "ideas.org")
(zz/add-file-keybinding "C-c z p" "~/org/projects.org" "projects.org")
(zz/add-file-keybinding "C-c z d" "~/org/diary.org" "diary.org")

(setq org-roam-directory org-directory)

(use-package! org-download
  :after org
  :config
  (defun zz/org-download-paste-clipboard (&optional noask)
    (interactive "P")
    (let ((file
           (if (not noask)
               (read-string (format "Filename [%s]: " org-download-screenshot-basename)
                            nil nil org-download-screenshot-basename)
             nil)))
      (org-download-clipboard file)))
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard)
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300))

(use-package! org-mac-link
  :after org
  :config
  (setq org-mac-grab-Acrobat-app-p nil) ; Disable grabbing from Adobe Acrobat
  (setq org-mac-grab-devonthink-app-p nil) ; Disable grabbinb from DevonThink
  (map! :map org-mode-map
        "C-c g"  #'org-mac-grab-link))

(after! org-agenda
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   ;; Indent todo items by level to show nesting
                                   (todo . " %i %-12:c%l")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (setq org-agenda-include-diary t))

(use-package! holidays
  :after org-agenda
  :config
  (require 'mexican-holidays)
  (require 'swiss-holidays)
  (setq swiss-holidays-zh-city-holidays
        '((holiday-float 4 1 3 "Sechseläuten") ;; meistens dritter Montag im April
          (holiday-float 9 1 3 "Knabenschiessen"))) ;; zweites Wochenende im September
  (setq calendar-holidays
        (append '((holiday-fixed 1 1 "New Year's Day")
                  (holiday-fixed 2 14 "Valentine's Day")
                  (holiday-fixed 4 1 "April Fools' Day")
                  (holiday-fixed 10 31 "Halloween")
                  (holiday-easter-etc)
                  (holiday-fixed 12 25 "Christmas")
                  (solar-equinoxes-solstices))
                swiss-holidays
                swiss-holidays-labour-day
                swiss-holidays-catholic
                swiss-holidays-zh-city-holidays
                holiday-mexican-holidays)))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-dir-name t)))
  (org-super-agenda-mode))

(use-package! org-archive
  :after org
  :config
  (setq org-archive-location "archive.org::datetree/"))

(use-package! org-gtd
  :after org
  :config
  ;; where org-gtd will put its files. This value is also the default one.
  (setq org-gtd-directory "~/gtd/")
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (setq org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (setq org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (setq org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c d c" . org-gtd-capture) ;; add item to inbox
   ("C-c d a" . org-agenda-list) ;; see what's on your plate today
   ("C-c d p" . org-gtd-process-inbox) ;; process entire inbox
   ("C-c d n" . org-gtd-show-all-next) ;; see all NEXT items
   ("C-c d s" . org-gtd-show-stuck-projects) ;; see projects that don't have a NEXT item
   ("C-c d f" . org-gtd-clarify-finalize))) ;; the keybinding to hit when you're done editing an item in the processing phase

(after! (org-gtd org-capture)
  (add-to-list 'org-capture-templates
               '("i" "GTD item"
                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i"
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("l" "GTD item with link to where you are in emacs now"
                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i\n  %a"
                 :kill-buffer t)))

(use-package! ox-awesomecv
  :after org)

  (defun zz/org-if-str (str &optional desc)
    (when (org-string-nw-p str)
      (or (org-string-nw-p desc) str)))

  (defun zz/org-macro-hsapi-code (module &optional func desc)
    (org-link-make-string
     (concat "https://www.hammerspoon.org/docs/"
             (concat module (zz/org-if-str func (concat "#" func))))
     (or (org-string-nw-p desc)
         (format "=%s="
                 (concat module
                         (zz/org-if-str func (concat "." func)))))))

  (defun zz/org-macro-keys-code-outer (str)
    (mapconcat (lambda (s)
                 (concat "~" s "~"))
               (split-string str)
               (concat (string ?\u200B) "+" (string ?\u200B))))
  (defun zz/org-macro-keys-code-inner (str)
    (concat "~" (mapconcat (lambda (s)
                             (concat s))
                           (split-string str)
                           (concat (string ?\u200B) "-" (string ?\u200B)))
            "~"))
  (defun zz/org-macro-keys-code (str)
    (zz/org-macro-keys-code-inner str))

  (defun zz/org-macro-luadoc-code (func &optional section desc)
    (org-link-make-string
     (concat "https://www.lua.org/manual/5.3/manual.html#"
             (zz/org-if-str func section))
     (zz/org-if-str func desc)))

  (defun zz/org-macro-luafun-code (func &optional desc)
    (org-link-make-string
     (concat "https://www.lua.org/manual/5.3/manual.html#"
             (concat "pdf-" func))
     (zz/org-if-str (concat "=" func "()=") desc)))

  (defun zz/org-reformat-buffer ()
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))

  (defun afs/org-remove-link ()
      "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (let ((remove (list (match-beginning 0) (match-end 0)))
          (description (if (match-end 3)
                   (org-match-string-no-properties 3)
                   (org-match-string-no-properties 1))))
      (apply 'delete-region remove)
      (insert description))))
  (bind-key "C-c C-M-u" 'afs/org-remove-link)

(after! magit
  (setq zz/repolist "~/.elvish/package-data/elvish-themes/chain-summary-repos.json")
  (defadvice! +zz/load-magit-repositories ()
    :before #'magit-list-repositories
    (setq magit-repository-directories
          (seq-map (lambda (e) (cons e 0)) (json-read-file zz/repolist))))
  (setq magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident nil)
          ("Status" 7 magit-repolist-column-flag nil)
          ("B<U" 3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U" 3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path" 99 magit-repolist-column-path nil))))

(after! epa
  (set (if EMACS27+
           'epg-pinentry-mode
         'epa-pinentry-mode) ; DEPRECATED `epa-pinentry-mode'
       nil))
