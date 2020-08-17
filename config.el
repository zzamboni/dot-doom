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
(setq doom-font (font-spec :family "Fira Code Retina" :height 160)
      doom-variable-pitch-font (font-spec :family "ETBembo" :height 180 :weight 'thin))
;; config.el:3 ends here

;; [[file:config.org::*config.el][config.el:4]]
(setq doom-theme 'spacemacs-light)
;; config.el:4 ends here

;; [[file:config.org::*config.el][config.el:5]]
(setq org-directory "~/org/")
;; config.el:5 ends here

;; [[file:config.org::*config.el][config.el:6]]
(setq org-hide-emphasis-markers t)
;; config.el:6 ends here

;; [[file:config.org::*config.el][config.el:7]]
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
;; config.el:7 ends here

;; [[file:config.org::*config.el][config.el:8]]
(setq confirm-kill-emacs nil)
;; config.el:8 ends here
