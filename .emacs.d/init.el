;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some general info

(setq is-DL (if (file-exists-p "/projects/ASIC/ridge/") t nil))
(setq is-me (if (or (string= "ekofoed" (getenv "USER"))
		    (string= "erik" (getenv "USER"))) t nil))
(setq is-home (if (string= "erik" (getenv "USER")) t nil))

(setq HOME (cond
	    (is-DL "/home/ekofoed")
	    (t (getenv "HOME"))))

(setq is-linux (if (string= system-type "gnu/linux") t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup MELPA package system

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'load-path (concat HOME "/.emacs.d/lisp")) ;My own packages

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

					;(use-package clojure-mode)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Done setting up package and use-package
;; .. now install desired packages

(if is-home (use-package powerline :config (powerline-default-theme))) ;Needs xpm compiled?
(use-package helm :bind
  (("M-x"    . helm-M-x)
   ("M-<f5>" . helm-find-files)
   ([f10]    . helm-buffers-list)
   ([S-f10]  . helm-recentf)))
(use-package clojure-mode)	      ;Syntax highlight
(use-package cider)		      ;REPL in emacs
(use-package paredit)		      ;Electrical grouping of parens
(use-package rainbow-delimiters)      ;paranthesis in different colors
(use-package magit)		      ;Git details in emacs
(use-package color-theme-modern	      ;Covers goldenrod
  :config
  (load-theme 'goldenrod t t)
  (enable-theme 'goldenrod))
(use-package which-key :config (which-key-mode))
(use-package better-defaults) ; removes menu, toolbar, and scroll
(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-.") 'ace-jump-mode))
(use-package yaml-mode)
(use-package tabbar		      ;Tabulate similar file-types (font size?)
  :config
  (tabbar-mode))
 
;;; ;; Local in my ./lisp dir
(require 'my-auto-insert)          ;My auto-comments and headings
(require 'markerpen)		      ;Allow changing selected text color
(global-set-key (kbd "<f8>")  'markerpen1)
(global-set-key (kbd "<f9>")  'markerpen4)

(require 'verilog-mode)
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))
(require 'rake-mode)	              ;
(require 'epa-file)		      ;easyPG interface for GnuGPG
(epa-file-enable)
(custom-set-variables '(epg-gpg-program "/usr/bin/gpg2"))
;(require 'csv-mode)		      ;CSV files KLUDGE Incompatible with powerline
(require 'uniquify)		      ;Built-in package, same file names looks diff.
(setq uniquify-buffer-name-style 'reverse)
(require 'other-modes)		      ;PT etc
(require 'fic-mode)		      ;NOTE/TODO etc in files
(setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook))
(mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes)

;; Done installing packages
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

(unless (server-running-p) (server-start)) ;; For emacs to listen

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
;(cond ((= height 2160) (set-face-attribute 'default nil :height 140))
;      ((= height 1080) (set-face-attribute 'default nil :height 60))
;      (t nil))

;;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
