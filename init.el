;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emacs-d "/home/kofoed/.emacs.d")
(setq is-linux (if (string= system-type "gnu/linux") t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA package system

(require 'package)

(when (< emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")         ; NOTE w/o creates no "gnu", just gnupg
  (add-to-list 'load-path (concat emacs-d "/lisp"))              ; NOTE Vital to have keys!! Download and move to lisp
  (require 'gnu-elpa-keyring-update)                             ; NOTE Now accepts gnu archives
  (add-to-list 'package-archives '("nongnu" . "http://elpa.nongnu.org/nongnu/") t))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-check-signature nil) ; KLUDGE(ESK) In case of bad signature? 'allow-unsigned is default

(package-initialize)
(package-refresh-contents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install packages

(package-install 'use-package)

(use-package bind-key :ensure t)
(use-package cider :ensure t)
(use-package color-theme-modern
  :ensure t
  :config
  (load-theme 'goldenrod t t)
  (enable-theme 'goldenrod))

(require 'my-auto-insert)          ;My auto-comments and headings
(require 'markerpen)		      ;Allow changing selected text color; NOTE not in any archive
; TODO Use hydra below?
(global-set-key (kbd "<f8>")  'markerpen1)
(global-set-key (kbd "<f9>")  'markerpen4)
(global-set-key (kbd "C-,")   'markerpen-mark-region) ; Default doesnt work
(global-set-key (kbd "C-.")   'markerpen-clear-all-marks) ; Default doesnt work

(use-package tabbar :ensure t :config (tabbar-mode))
(use-package powerline :ensure t :config (powerline-default-theme))

(use-package helm
  :init (setq tab-bar-tab-name-function nil) ; KLUDGE this is undefined for some reason
  :ensure t
  :bind
  (("M-x"    . helm-M-x)))

(use-package fic-mode :ensure t)
(setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook))
(mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes)
(setq fic-highlighted-words '("FIXME" "TODO" "NOTE" "KLUDGE" "BUG"))

(use-package treemacs
  :ensure t
  :bind (("M-0" . treemacs-select-window)
	 ("M-o" . ace-window)))

(use-package org
  :pin gnu
  :config
  (setq org-agenda-files "~/org")
  (setq org-log-done 'time)
  (setq org-return-follows-link t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
  (define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'visual-line-mode))

;;; Done installing packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various

(winner-mode 1)				;Allows revert windows content/position history w/ C-c <|> 
(ffap-bindings)				;ffap = fINDfILEaTPoint
(setq visible-bell t)
(tool-bar-mode -1)
;(scroll-bar-mode -1)

(defhydra hydra-toggle (:color blue)
  "Toggle"
  ("l" display-line-numbers-mode "line-numbers")
  ("c" column-number-mode        "columns")
  ("t" toggle-truncate-lines     "truncate")
  ("f" follow-mode               "follow")
  ("v" visual-line-mode          "visual-line")
  ("w" whitespace-mode           "whitespace"))
(global-set-key (kbd "C-c t") 'hydra-toggle/body)

;;; Various setup
;
;(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))

;;; Emacs shell setup
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook (lambda () (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

(global-set-key [f1] 'shell)
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-same-window)))
(global-set-key [f2] 'rename-buffer)

(global-set-key (kbd "C-'") 'erase-buffer)
(global-set-key (kbd "C-x r p") 'replace-rectangle)

;; Various useful settings
(global-hi-lock-mode 1)
(show-paren-mode t)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ansi colors
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "blue" "MediumOrchid1" "cyan" "white"])

(defun ek-hi-set ()
  (interactive)
  (hi-lock-mode -1)
  (hi-lock-mode)
  (highlight-lines-matching-regexp "^\\(**WARN:\\).*$" 'hi-green-b)
  (highlight-lines-matching-regexp "^\\(#WARNING\\).*$" 'hi-red-b)
  (highlight-lines-matching-regexp "^\\(**ERR\\).*$" 'hi-red-b)
  )
(global-set-key (kbd "<f5>") 'ek-hi-set)

;;;    
;;;    ;(setq undo-outer-limit 1200000000)
;;;    
;;;    ;(unless (server-running-p) (server-start)) ;; For emacs to listen
;;;    
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Screen resolution
 
;; Get screen info if on X
(if is-linux
    ;;(if (= (string-to-number (getenv "SHLVL")) 3) ;; TODO test instead for existence of X and command below
    (progn
      (setq dimensions (shell-command-to-string "xdpyinfo | grep dimension"))
      (string-match "\\([0-9]+\\)x\\([0-9]+\\) pixels (\\([0-9]+\\)x\\([0-9]+\\)" dimensions)
      (setq width  (string-to-number (match-string 1 dimensions)))
      (setq height (string-to-number (match-string 2 dimensions)))
      )
  (progn
    (setq width  1920)
    (setq height 1080)))

					; Set according to screen resolution
(cond ((> height 1590) (set-face-attribute 'default nil :height 120))
      ((= height 1080) (set-face-attribute 'default nil :height 60))
      (t nil))
;;;    
;;;    ;;; End of file
;;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    
;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(package-selected-packages (quote (helm use-package))))
;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; )
;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (tabbar gnu-elpa-keyring-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
