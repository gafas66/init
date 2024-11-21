;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;; Local Variables:
;; comment-column: 1
;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq is-linux (if (string= system-type "gnu/linux") t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA package system

(add-to-list 'load-path "~/.emacs.d/lisp") ; NOTE My local lisps
(require 'package)

(when (< emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")         ; NOTE w/o creates no "gnu", just gnupg
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
(require 'markerpen)		   ;Allow changing selected text color; NOTE not in any archive

(use-package tabbar
  :ensure t
  :config (tabbar-mode)
  (set-face-attribute 'tabbar-default    nil :background "gray60")
  (set-face-attribute 'tabbar-unselected nil :background "gray85"  :foreground "gray30" :box nil)
  (set-face-attribute 'tabbar-selected   nil :background "yellow" :foreground "blue"  :box nil :weight 'bold)
  (set-face-attribute 'tabbar-button     nil :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute 'tabbar-separator  nil :height 0.7))

(when (> emacs-major-version 26) (use-package powerline :ensure t :config (powerline-default-theme)))

(use-package helm
  :init (setq tab-bar-tab-name-function nil) ; KLUDGE this is undefined for some reason
  :ensure t
  :bind
  (("M-x" . helm-M-x)))

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
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (add-hook 'org-mode-hook 'visual-line-mode))

(use-package magit
  :ensure t
  :bind (("C-x C-g" . magit-status)))

;;; Done installing packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(setq good-themes
      '(goldenrod classic cobalt dark-blue2 desert digital-ofs1 euphoria feng-shui fischmeister
		  late-night lawrence ld-dark lethe marquardt retro-green xemacs tango-dark))

(defun ek-theme (theme) (interactive) (mapcar #'disable-theme custom-enabled-themes) (load-theme theme t t) (enable-theme theme))

(defhydra hydra-appearance (:color blue)
  ("1" (ek-theme 'goldenrod)         "goldenrod"         :column "Theme")
  ("2" (ek-theme 'classic)           "classic"           :column "Theme")
  ("3" (ek-theme 'cobalt)            "cobalt"            :column "Theme")
  ("4" (ek-theme 'feng-shui)         "feng-shui"         :column "Theme")
  ("5" (ek-theme 'late-night)        "late-night"        :column "Theme")
  ("6" (ek-theme 'retro-green)       "retro-green"       :column "Theme")
  ("7" (ek-theme 'wheat)             "wheat"             :column "Theme")
  ("8" (ek-theme 'word-perfect)      "word-perfect"      :column "Theme")
  ("9" (ek-theme 'taming-mr-arneson) "taming-mr-arneson" :column "Theme")
  ("0" (ek-theme 'light-blue)        "light-blue       " :column "Theme")

  ("l" display-line-numbers-mode "line-numbers" :column "Toggle")
  ("c" column-number-mode        "columns"      :column "Toggle")
  ("t" toggle-truncate-lines     "truncate"     :column "Toggle")
  ("f" follow-mode               "follow"       :column "Toggle")
  ("v" visual-line-mode          "visual-line"  :column "Toggle")
  ("w" whitespace-mode           "whitespace"   :column "Toggle")
  ("q" nil                       "Quit menu" :color red :column nil))
(global-set-key (kbd "C-c h") 'hydra-appearance/body)

(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("v" nil))

;(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)
;(setq org-agenda-mode-hook nil)
;(add-hook 'org-agenda-mode-hook
;	  (lambda () (define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)))


(global-unset-key [f1])
(defhydra hydra-shell-stuff (:color blue)
  "Shells"
  ("s" shell                   "shell")
  ("a" (ansi-term "/bin/bash") "ansi-term")
  ("r" rename-buffer           "Rename buffer"))
(global-set-key [f1] 'hydra-shell-stuff/body)

(global-set-key (kbd "C-'") 'erase-buffer)
(global-set-key (kbd "C-x r p") 'replace-rectangle)

(defhydra hydra-comma (:color blue)
  "Toggle"
  ("m" markerpen-mark-region      "mark region")
  ("c" markerpen-clear-all-marks  "clear all marks")
  ("r" (markerpen-mark-region 1)  "red")
  ("g" (markerpen-mark-region 2)  "grey")
  ("y" (markerpen-mark-region 3)  "yellow")
  ("b" (markerpen-mark-region 4)  "blue")
  ("u" (markerpen-mark-region 5)  "underline"))
(global-set-key (kbd "C-,") 'hydra-comma/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various

(defalias 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)				;Allows revert windows content/position history w/ C-c <|> 
(ffap-bindings)				;ffap = fINDfILEaTPoint
(setq visible-bell t)
(tool-bar-mode -1)
;(scroll-bar-mode -1)

(global-hi-lock-mode 1)
(show-paren-mode t)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Emacs shell setup
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook (lambda () (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-same-window)))

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
 '(custom-safe-themes
   '("cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" default))
 '(org-agenda-files '("~/init/tasks.org"))
 '(package-selected-packages '(cycle-themes magit tabbar gnu-elpa-keyring-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
