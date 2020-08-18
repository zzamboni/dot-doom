;; [[file:config.org::*config.el][config.el:1]]
(setq user-full-name "Diego Zamboni"
      user-mail-address "diego@zzamboni.org")
;; config.el:1 ends here

;; [[file:config.org::*config.el][config.el:2]]
(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-right-option-modifier 'super)))
;; config.el:2 ends here

;; [[file:config.org::*config.el][config.el:3]]
(setq doom-font (font-spec :family "Fira Code Retina" :size 16)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 18))
;; config.el:3 ends here

;; [[file:config.org::*config.el][config.el:4]]
(use-package! mixed-pitch
  :config
  (setq mixed-pitch-variable-pitch-cursor nil)
  :hook
  (text-mode . mixed-pitch-mode))
;; config.el:4 ends here

;; [[file:config.org::*config.el][config.el:5]]
(setq doom-theme 'spacemacs-light)
;; config.el:5 ends here

;; [[file:config.org::*config.el][config.el:6]]
(setq org-directory "~/org/")
;; config.el:6 ends here

;; [[file:config.org::*config.el][config.el:7]]
(setq org-hide-emphasis-markers t)
;; config.el:7 ends here

;; [[file:config.org::*config.el][config.el:8]]
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
;; config.el:8 ends here

;; [[file:config.org::*config.el][config.el:9]]
(setq confirm-kill-emacs nil)
;; config.el:9 ends here

;; [[file:config.org::*config.el][config.el:10]]
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode
           (lambda () (add-hook 'after-save-hook 'org-babel-tangle :append :local)))
;; config.el:10 ends here

;; [[file:config.org::*config.el][config.el:11]]
(use-package! ox-leanpub
  :config
  (require 'ox-leanpub-markdown)
  (org-leanpub-book-setup-menu-markdown))
;; config.el:11 ends here

;; [[file:config.org::*config.el][config.el:12]]
(map! "C-x b" #'counsel-recentf)
;; config.el:12 ends here

;; [[file:config.org::*Reformatting an Org buffer][Reformatting an Org buffer:1]]
  (defun zz/org-reformat-buffer ()
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))
;; Reformatting an Org buffer:1 ends here

;; [[file:config.org::*Reformatting an Org buffer][Reformatting an Org buffer:2]]
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
;; Reformatting an Org buffer:2 ends here

;; [[file:config.org::*Code for org-mode macros][Code for org-mode macros:1]]
  (defun zz/org-if-str (str &optional desc)
    (when (org-string-nw-p str)
      (or (org-string-nw-p desc) str)))
;; Code for org-mode macros:1 ends here

;; [[file:config.org::*Code for org-mode macros][Code for org-mode macros:2]]
  (defun zz/org-macro-hsapi-code (module &optional func desc)
    (org-link-make-string
     (concat "https://www.hammerspoon.org/docs/"
             (concat module (zz/org-if-str func (concat "#" func))))
     (or (org-string-nw-p desc)
         (format "=%s="
                 (concat module
                         (zz/org-if-str func (concat "." func)))))))
;; Code for org-mode macros:2 ends here

;; [[file:config.org::*Code for org-mode macros][Code for org-mode macros:3]]
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
;; Code for org-mode macros:3 ends here

;; [[file:config.org::*Code for org-mode macros][Code for org-mode macros:4]]
  (defun zz/org-macro-luadoc-code (func &optional section desc)
    (org-link-make-string
     (concat "https://www.lua.org/manual/5.3/manual.html#"
             (zz/org-if-str func section))
     (zz/org-if-str func desc)))
;; Code for org-mode macros:4 ends here

;; [[file:config.org::*Code for org-mode macros][Code for org-mode macros:5]]
  (defun zz/org-macro-luafun-code (func &optional desc)
    (org-link-make-string
     (concat "https://www.lua.org/manual/5.3/manual.html#"
             (concat "pdf-" func))
     (zz/org-if-str (concat "=" func "()=") desc)))
;; Code for org-mode macros:5 ends here
