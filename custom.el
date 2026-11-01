(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9af2b1c0728d278281d87dc91ead7f5d9f2287b1ed66ec8941e97ab7a6ab73c0"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6" default))
 '(safe-local-variable-values
   '((flycheck-org-lint-disabled-checkers invalid-id-link link-to-local-file)
     (org-download-heading-lvl) (org-download-image-dir . "images")
     (eval add-hook 'after-save-hook
           (lambda nil (org-export-to-file 'awesomecv "src/zamboni-vita.tex"))
           :append :local)
     (eval add-hook 'after-save-hook
           (lambda nil (org-export-to-file 'awesomecv "zamboni-resume.tex"))
           :append :local)
     (org-re-reveal-root . "revealjs/")
     (eval add-hook 'after-save-hook
           (lambda nil (org-export-to-file 'awesomecv "zamboni-vita.tex"))
           :append :local)
     (eval add-hook! after-save :append :local (zz/refresh-reveal-prez)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-dashboard-banner ((t (:inherit default))))
 '(doom-dashboard-loaded ((t (:inherit default)))))
