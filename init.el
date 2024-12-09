;--------------------------------------------------------------------------------
; Emacs init file
;--------------------------------------------------------------------------------

(org-babel-load-file "~/init/settings.org")

; End of file
;--------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" default))
 '(package-selected-packages
   '(org-bullets nerd-fonts ob-clojure doom-modeline helm-swoop use-package origami dimmer all-the-icons eat centaur-tabs major-mode-hydra helm-org cycle-themes magit tabbar gnu-elpa-keyring-update))
 '(safe-local-variable-values '((epa-file-encrypt-to ekofoed@gmail\.com))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif" :height 1.4))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif" :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif" :height 1.2))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "goldenrod" :family "Sans Serif")))))
