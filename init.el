;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;; Local Variables:
;; comment-column: 1
;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use absolute latest org-mode

(setq is-linux (and (getenv "DISPLAY") (if (string= system-type "gnu/linux") t nil)))
(setq my-org-files (list "~/init/org/Capture.org.gpg" "~/init/org/other.org.gpg" "~/init/org/home.org.gpg" "~/init/org/vec.org.gpg" "~/init/org/journal.org.gpg"))

;; Exclude if at work
(setq my-org-files (if (file-directory-p "/home/kofoed") (remove "~/init/org/other.org.gpg" my-org-files) my-org-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA package system

(add-to-list 'load-path "~/.emacs.d/lisp") ; NOTE My local lisps
(add-to-list 'load-path "~/src/all-the-icons-dired") ; NOTE My local lisps

(require 'package)

(when (< emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")         ; NOTE w/o creates no "gnu", just gnupg
  (require 'gnu-elpa-keyring-update)                             ; NOTE Now accepts gnu archives
  (add-to-list 'package-archives '("nongnu" . "http://elpa.nongnu.org/nongnu/") t))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-check-signature nil) ; KLUDGE(ESK) In case of bad signature? 'allow-unsigned is default

(package-initialize)
;; NOTE Comment out refresh when debugging/expanding/editing this file
(package-refresh-contents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install packages

(package-install 'use-package)
(setq use-package-always-ensure t)

(use-package bind-key)
(use-package cider)
(use-package color-theme-modern
  :config
  (setq d-t (if t 'goldenrod 'wheat))
  (load-theme d-t t t)
  (enable-theme d-t))

(require 'my-auto-insert)          ;My auto-comments and headings
(require 'markerpen)		   ;Allow changing selected text color; NOTE not in any archive

(if (< emacs-major-version 27)
    (use-package tabbar
      :config (tabbar-mode)
      (set-face-attribute 'tabbar-default    nil :background "gray60")
      (set-face-attribute 'tabbar-unselected nil :background "gray85"  :foreground "gray30" :box nil)
      (set-face-attribute 'tabbar-selected   nil :background "yellow" :foreground "blue"  :box nil :weight 'bold)
      (set-face-attribute 'tabbar-button     nil :box '(:line-width 1 :color "gray72" :style released-button))
      (set-face-attribute 'tabbar-separator  nil :height 0.7))
  (use-package centaur-tabs
    :demand
    :config
    (centaur-tabs-mode t)
					;(setq centaur-tabs-height 32) ; Does not affect font or icon
					;(centaur-tabs-headline-match)
					;(setq centaur-tabs-style "bar")
    (setq centaur-tabs-set-bar 'over)
					;(centaur-tabs-change-fonts "arial" 160) 
    (setq centaur-tabs-set-icons t)
    (setq centaur-tabs-icon-type 'all-the-icons)
					;(setq centaur-tabs-plain-icons t) ; Replacing icons!
					;(setq centaur-tabs-modified-marker t)
    (defun centaur-tabs-buffer-groups ()
      "Use as few groups as possible."
      (list (cond ((string-equal "*" (substring (buffer-name) 0 1))
                   (cond ((string-equal "eglot" (downcase (substring (buffer-name) 1 6)))
                          "Eglot")
			 (t
                          "Tools")))
                  ((string-equal "magit" (downcase (substring (buffer-name) 0 5)))
                   "Magit")
                  (t
                   "Default"))))
    :bind
    ("C-<left>" . centaur-tabs-backward)
    ("C-<right>" . centaur-tabs-forward)))
;; KLUDGE This is a known issue, fixed for some reason by below line
(centaur-tabs-group-by-projectile-project)

(use-package all-the-icons
  :if (display-graphic-p))
; NOTE Run all-the-icons-install-fonts

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (setq dimmer-fraction 0.2)
  (dimmer-mode t))

(use-package eat
  :config
  (define-key eat-semi-char-mode-map (kbd "M-o") 'ace-window)
  (define-key eat-semi-char-mode-map (kbd "M-0") 'treemacs-select-window))
(add-hook 'window-configuration-change-hook
  (lambda ()
    (when (string-equal major-mode "eat-mode")
      (unless (bound-and-true-p called-once)
					;(eat-term-send-string eat-terminal "exec $SHELL -l")
	(eat-term-send-string eat-terminal "ssh dsc009")
        (eat-self-input 1 'return)
        (setq-local called-once t)))))

(use-package avy
  :bind
  (("C-:" . 'avy-goto-char-2)))

;(when (> emacs-major-version 26) (use-package powerline :ensure t :config (powerline-default-theme)))
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package nerd-icons) ;then use M-x nerd-icons-install-fonts

; NOTE - can't see any difference
;(use-package spaceline
;  :ensure t
;  :config
;  (spaceline-spacemacs-theme))

(use-package helm
  :init (setq tab-bar-tab-name-function nil) ; KLUDGE this is undefined for some reason
  :bind
  (("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring)
   ("C-x C-b" . helm-mini)
   ;("C-x C-f" . helm-find-files)
   ("M-s o"   . helm-occur)))
(global-set-key (kbd "C-x C-f") 'ido-find-file)

(use-package fic-mode :ensure t)
(setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook))
(mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes)
(setq fic-highlighted-words '("FIXME" "TODO" "NOTE" "KLUDGE" "BUG"))

(use-package treemacs
  :bind (("M-0" . treemacs-select-window)
	 ("M-o" . ace-window)))

(use-package org
  :pin gnu
  :config
  (setq org-log-done 'time)
  (setq org-return-follows-link t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  (define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
  (define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  ;(setq org-directory "~/init/org")
  (setq org-default-notes-file "~/init/org/Capture.org.gpg")
  (setq org-agenda-files my-org-files)
  (define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-skip-scheduled-if-done t)
  ;(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  )

;; Below allows refile upto second heading (default only first level)
(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))

(setq org-todo-keywords
      '((sequence "TODO" "WAIT" "|" "CANCELLED" "DONE")))

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(use-package helm-org
  ;:config
  ;(add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  ;(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  )
(add-hook 'helm-mode-hook
	  (lambda ()
	    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
	    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))))

(use-package magit
  :bind (("C-x C-g" . magit-status)))

(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra)) ;Can we make this key work?

(load "all-the-icons-dired")
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;(use-package origami :ensure t) ; No keybindings?

;; Occurence using helm-swoop
(use-package helm-swoop
  :bind
  (("M-i" . helm-swoop)))

;;; Done installing packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(setq outline-minor-mode-cycle t)

(setq org-capture-templates
      '(("t" "General task"       entry (file+regexp org-default-notes-file "Tasks") "* TODO %?\n  %i\n  %a")
	("c" "C2C task"           entry (file+headline "~/init/org/vec.org.gpg" "C2C tasks" ) "* TODO %?\n  %i\n  %a")
	("p" "PCIE task"          entry (file+headline "~/init/org/vec.org.gpg" "PCIE tasks") "* TODO %?\n  %i\n  %a")
	("v" "VEC top-level task" entry (file+regexp "~/init/org/vec.org.gpg" "VEC Top.*" ) "* TODO %?\n  %i\n  %a")
	("e" "Emacs task"         entry (file+headline "~/init/org/home.org.gpg" "Emacs Tasks") "* TODO %?\n  %i\n  %a")
	("l" "Clojure task"       entry (file+headline "~/init/org/home.org.gpg" "Clojure Tasks") "* TODO %?\n  %i\n  %a")
        ("j" "Journal"            entry (file+datetree "~/init/org/journal.org.gpg")
         "* %?\nEntered on %U\n%i\n  %a")))

(setq org-agenda-custom-commands
      '(("u" "Untagged tasks" tags-todo "-{.*}")))
;	("d" "Daily Agenda"
;	 ((agenda "" ((org-agenda-span 'day)
;		      (org-deadline-warning-days 7)))))))

(setq good-themes
      '(goldenrod classic cobalt dark-blue2 desert digital-ofs1 euphoria feng-shui fischmeister
		  late-night lawrence ld-dark lethe marquardt retro-green xemacs tango-dark))

(defun ek-theme (theme) (interactive) (mapcar #'disable-theme custom-enabled-themes) (load-theme theme t t) (enable-theme theme))

(defhydra hydra-appearance (:color blue)
  ("1" (ek-theme 'wheat)             "wheat"             :column "Theme")
  ("2" (ek-theme 'goldenrod)         "goldenrod"         :column "Theme")
  ("3" (ek-theme 'classic)           "classic"           :column "Theme")
  ("4" (ek-theme 'cobalt)            "cobalt"            :column "Theme")
  ("5" (ek-theme 'feng-shui)         "feng-shui"         :column "Theme")
  ("6" (ek-theme 'late-night)        "late-night"        :column "Theme")
  ("7" (ek-theme 'retro-green)       "retro-green"       :column "Theme")
  ("8" (ek-theme 'word-perfect)      "word-perfect"      :column "Theme")
  ("9" (ek-theme 'taming-mr-arneson) "taming-mr-arneson" :column "Theme")
  ("0" (ek-theme 'light-blue)        "light-blue       " :column "Theme")

  ("l" display-line-numbers-mode "line-numbers"   :column "Toggle")
  ("c" column-number-mode        "columns"        :column "Toggle")
  ("g" hl-line-mode              "hl-line"        :column "Toggle")
  ("G" global-hl-line-mode       "hl-line GLOBAL" :column "Toggle")
  ("t" toggle-truncate-lines     "truncate"       :column "Toggle")
  ("f" follow-mode               "follow"         :column "Toggle")
  ("v" visual-line-mode          "visual-line"    :column "Toggle")
  ("w" whitespace-mode           "whitespace"     :column "Toggle")
  
  ("m" helm-all-mark-rings       "mark-rings"     :column "Helm")
  ("r" helm-register             "registers"      :column "Helm")
  ("p" helm-top                  "top"            :column "Helm")
  ("o" helm-colors               "Pick color"     :column "Helm")

  ("q" nil                       "Quit menu" :color red :column nil))
(global-set-key (kbd "C-c h") 'hydra-appearance/body)

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region")
    ("q" nil "quit"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(major-mode-hydra-define clojure-mode nil
  ("Connect"
   (("j" cider-jack-in      "jack-in")
    ("J" cider-jack-in-cljs "jack-in-cljs")
    ("c" nil "TBD connect")
    ("R" nil "TBD reconnect")
    ("Q" nil "TBD disconnect")
    ("q" nil "quit"))))

(defun my-org-insert-sub-task ()
  (interactive)
  (let ((parent-deadline (org-get-deadline-time nil)))
    (org-goto-sibling)
    (org-insert-todo-subheading t)
    (when parent-deadline
      (org-deadline nil parent-deadline))))
(define-key org-mode-map (kbd "C-c s") 'my-org-insert-sub-task)

;(global-unset-key [f1])
(defhydra hydra-shell-stuff (:color blue)
  "Shells"
  ("s" shell                   "shell")
  ("a" (ansi-term "/bin/bash") "ansi-term")
  ("e" (eat "/bin/bash" "echo hi") "eat-term")
  ("r" rename-buffer           "Rename buffer"))
(global-set-key [f2] 'hydra-shell-stuff/body)

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

;; Now shift l/r/u/d will move to that window (not start selection)
;; FIXME Crashes with org-mode date-selector
;(when (fboundp 'windmove-default-keybindings)
;  (windmove-default-keybindings))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; NOTE Fix ansi-term keys we want(!)
(add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "M-o") 'ace-window)))
(add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "M-0") 'treemacs-select-window)))
(add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "M-x") 'helm-M-x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ek-hi-set ()
  (interactive)
  (hi-lock-mode -1)
  (hi-lock-mode)
  (highlight-lines-matching-regexp "^\\(**WARN:\\).*$" 'hi-green-b)
  ;(highlight-lines-matching-regexp "^\\(#WARNING\\).*$" 'hi-red-b)
  (highlight-lines-matching-regexp "^\\(**ERR\\).*$" 'hi-red-b)
  )
(global-set-key (kbd "<f5>") 'ek-hi-set)

;    
;    ;(setq undo-outer-limit 1200000000)
;    
;    ;(unless (server-running-p) (server-start)) ;; For emacs to listen
;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" default))
 '(package-selected-packages
   '(nerd-fonts ob-clojure doom-modeline helm-swoop use-package origami dimmer all-the-icons eat centaur-tabs major-mode-hydra helm-org cycle-themes magit tabbar gnu-elpa-keyring-update))
 '(safe-local-variable-values '((epa-file-encrypt-to ekofoed@gmail\.com))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
