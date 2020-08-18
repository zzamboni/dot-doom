(setq user-full-name "Diego Zamboni"
      user-mail-address "diego@zzamboni.org")

(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-right-option-modifier 'super)))

(setq doom-font (font-spec :family "Fira Code Retina" :size 16)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 18))

(use-package! mixed-pitch
  :config
  (setq mixed-pitch-variable-pitch-cursor nil)
  :hook
  (text-mode . mixed-pitch-mode))

(setq doom-theme 'spacemacs-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq confirm-kill-emacs nil)

(map! "C-x b" #'counsel-recentf)

(set (if EMACS27+
         'epg-pinentry-mode
       'epa-pinentry-mode) ; DEPRECATED `epa-pinentry-mode'
     nil)

(setq org-directory "~/org/")

(setq org-hide-emphasis-markers t)

(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode
           (lambda () (add-hook 'after-save-hook 'org-babel-tangle :append :local)))

(use-package! ox-leanpub
  :config
  (require 'ox-leanpub-markdown)
  (org-leanpub-book-setup-menu-markdown))

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
