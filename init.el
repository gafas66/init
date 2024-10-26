;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(when (>= emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "etc/ssl/certs")) ;Not same all all distros
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
;;(setq use-package-always-ensure t)
(require 'bind-key)

;(use-package auto-package-update
(setq auto-package-update-delete-old-versions t)
(setq auto-package-update-hide-results t)
;(auto-package-update-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix appearance

;(require 'elpa-color-theme-modern)
(use-package color-theme-modern
  :config
  (load-theme 'goldenrod t t)
  (enable-theme 'goldenrod))

(tool-bar-mode -1)

;(require 'ef-themes)
;(ef-themes-select 'ef-autumn)
(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "NOTE"))
(setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook))
(mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor modes

;; Below loads with M-x package-install (needed once?)
;(use-packagep clojure-mode)
;(use-package cider)
;(use-package magit)
; auto-package-update
;powerline
;color-theme-buffer-local

(use-package tabbar :config (tabbar-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My key defs

(global-set-key (kbd "C-'") 'erase-buffer)
(global-set-key (kbd "C-x r p") 'replace-rectangle)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(global-set-key [f1] 'shell)
(global-set-key [f2] 'rename-buffer)

;(global-set-key (kbd "<f5>") 'ek-hi-set)
(global-set-key (kbd "<f6>") 'highlight-changes-mode)
(global-set-key (kbd "<f7>") 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better movement between buffers/commands

(winner-mode 1)

(use-package helm :bind
  (("M-x" . helm-M-x)
   ("M-<f5>" . helm-find-files)
   ([f10]    . helm-buffers-list)
   ([S-f10]  . helm-recentf)))

(defun truncate-lines-toggle ()
  (interactive)
  (if (equal truncate-lines t)
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (redraw-display))
(global-set-key (kbd "C-t") 'truncate-lines-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(color-theme-buffer-local powerline auto-package-update fic-mode magit cider clojure-mode use-package))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
