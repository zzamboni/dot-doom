(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/.config/elvish/rc.org"))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
      (lambda nil (org-export-to-file 'awesomecv "src/zamboni-vita.tex"))
      :append :local)
     (eval add-hook 'after-save-hook
      (lambda nil (org-export-to-file 'awesomecv "zamboni-resume.tex")) :append
      :local)
     (org-re-reveal-root . "revealjs/")
     (eval add-hook 'after-save-hook
      (lambda nil (org-export-to-file 'awesomecv "zamboni-vita.tex")) :append
      :local)
     (eval add-hook! after-save :append :local (zz/refresh-reveal-prez)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-dashboard-banner ((t (:inherit default))))
 '(doom-dashboard-loaded ((t (:inherit default)))))
