;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file (non-existing by default)
;; Some general info

(setq is-DL (if (file-exists-p "/projects/ASIC/ridge/") t nil))
(setq is-me (if (or (string= "ekofoed" (getenv "USER"))
		    (string= "erik" (getenv "USER"))) t nil))

(setq HOME (cond
	    (is-DL "/home/ekofoed")
	    (t (getenv "HOME"))))

(setq is-linux (if (string= system-type "gnu/linux") t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure we have our local library setup from MALPA etc

(require 'package)
(package-initialize)

;(add-to-list 'package-archives '("melpa"     . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable"     . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path (concat HOME "/.emacs.d/lisp")) ;My own packages
(package-refresh-contents)
(package-install 'use-package)
;(require 'use-package-ensure)
;(setq use-package-ensure t)

;;; Can't get below to auto-work
;;;(use-package auto-package-update
;;;	     :config
;;;	     (setq auto-package-update-delete-old-versions t)
;;;	     (setq auto-package-update-hide-results t)
;;;	     (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Done setting up package and use-package
;; .. now install desired packages

;;; (use-package clojure-mode)	      ;Syntax highlight
;;; (use-package cider)		      ;REPL in emacs
;;; (use-package paredit)		      ;Electrical grouping of parens
;;; (use-package rainbow-delimiters)      ;paranthesis in different colors
;;; (use-package magit)		      ;Git details in emacs
;;; (use-package color-theme-modern	      ;Covers goldenrod
;;;   :config
;;;   (load-theme 'goldenrod t t)
;;;   (enable-theme 'goldenrod))
;;; 					;(use-package which-key :config (which-key-mode)) ;FIXME
;;; ;(package-install 'which-key)(which-key-mode)
;;; 					;(use-package better-defaults) ;FIXME
;;; ;(package-install 'better-defaults)
;;; (use-package paredit)
;;; 					;(use-package ace-jump-mode) ;FIXME
;;; ;(package-install 'ace-jump-mode)
;;; 					;(use-package powerline :config (powerline-default-theme)) ;FIXME
;;; ;(package-install 'powerline) (powerline-default-theme)
;;; (use-package yaml-mode)
;;; 
;;; ;; Local in my ./lisp dir
;;; (use-package tabbar		      ;Tabulate similar file-types (font size?)
;;;   :config
;;;   (tabbar-mode))
;;; (use-package my-auto-insert)          ;My auto-comments and headings
;;; (use-package markerpen		      ;Allow changing selected text color
;;;   :config
;;;   (global-set-key (kbd "<f8>")  'markerpen1)
;;;   (global-set-key (kbd "<f9>")  'markerpen4)
;;;   (global-set-key (kbd "<f10>") 'markerpen9))
;;; (use-package verilog-mode	      ;For verilog files
;;;   :config
;;;   (autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
;;;   (add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode)))
;;; (use-package rake-mode)	              ;
;;; (use-package epa-file)		      ;easyPG interface for GnuGPG
;;; (use-package csv-mode)		      ;CSV files
;;; (use-package uniquify		      ;Built-in package, same file names looks diff.
;;;   :config
;;;   (setq uniquify-buffer-name-style 'reverse))
;;; (use-package other-modes)		      ;PT etc
;;; (use-package fic-mode		      ;NOTE/TODO etc in files
;;;   :config
;;;   (setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook))
;;;   (mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Done installing packages

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

(cond ((= height 2160) (set-face-attribute 'default nil :height 140))
      ((= height 1080) (set-face-attribute 'default nil :height 100))
      (t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Various setup

(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))

(winner-mode 1)				;Allows revert windows content/position history w/ C-c <|> 
(ffap-bindings)				;ffap = fINDfILEaTPoint
(setq visible-bell t)

;; Emacs shell setup
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook (lambda () (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

(global-set-key [f1] 'shell)
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-same-window)))
(global-set-key [f2] 'rename-buffer)

				      ; Setup own function keys
(defun truncate-lines-toggle ()
  (interactive)
  (if (equal truncate-lines t)
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (redraw-display))
(global-set-key (kbd "C-t") 'truncate-lines-toggle)
					; My own function keys
(global-set-key (kbd "C-'") 'erase-buffer)
(global-set-key (kbd "C-0") 'find-file-at-point)
(global-set-key (kbd "C-x r p") 'replace-rectangle)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
					; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Various useful settings
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy (lambda (pattern) t))
(tool-bar-mode -1)
(show-paren-mode t)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ansi colors
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])

(defun ek-hi-set ()
  (interactive)
  (hi-lock-mode -1)
  (hi-lock-mode)
  (highlight-lines-matching-regexp "^\\(?:Error:\\).*$" 'hi-red-b)
  (highlight-lines-matching-regexp "^\\(?:ERROR:\\).*$" 'hi-red-b)
  (highlight-lines-matching-regexp "^\\(?:ok \\).*$" 'hi-green-b)
  (highlight-lines-matching-regexp "^.*\\(?:\\[Error\\]\\).*$" 'hi-red-b)
  (highlight-lines-matching-regexp "^\\(?:Warning:\\).*$" 'hi-green-b)
  (highlight-lines-matching-regexp "^\\(?:WARN:\\).*$" 'hi-green-b)
  (highlight-lines-matching-regexp "^\\(?:\\[Warning\\]\\).*$" 'hi-green-b)
;  (highlight-lines-matching-regexp "\\(?:generics\\)" 'hi-yellow)
  )
  
(global-set-key (kbd "<f5>") 'ek-hi-set)
(global-set-key (kbd "<f6>") 'highlight-changes-mode)
(global-set-key (kbd "<f7>") 'whitespace-mode)

(setq undo-outer-limit 1200000000)

(server-start) ;; For emacs to listen

;;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(powerline ace-jump-mode better-defaults which-key use-package rainbow-delimiters paredit magit color-theme-modern cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "bitstream" :family "Courier"))))
 '(ek-blue-face ((t (:foreground "blue" :size "8pt"))) t)
 '(ek-cyan-face ((t (:foreground "cyan" :size "8pt"))) t)
 '(ek-dark-face ((t (:foreground "dark goldenrod" :size "8pt"))) t)
 '(ek-green-face ((t (:foreground "green" :size "8pt"))) t)
 '(ek-magenta-face ((t (:foreground "magenta" :size "8pt"))) t)
 '(ek-orange-face ((t (:foreground "orange3" :size "8pt"))) t)
 '(ek-red-bold-face ((t (:foreground "red" :size "8pt" :bold t))) t)
 '(ek-red-face ((t (:foreground "red" :size "8pt"))) t)
 '(ek-wheat-face ((t (:foreground "Wheat3" :size "8pt"))) t)
 '(ek-yellow-face ((t (:foreground "yellow" :size "8pt"))) t))
