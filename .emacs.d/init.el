;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file (non-existing by default)

;; Make sure we have our local library setup

(setq is-DL (if (file-exists-p "/projects/ASIC/ridge/") t nil))
(setq is-RN (if (file-exists-p "/proj/ARTPEC7/") t nil))
(setq is-me (if (string= "ekofoed" (getenv "USER")) t nil))

(setq HOME (cond
	    (is-DL "/home/ekofoed")
	    (is-RN "/home/kofoede")
	    (t (getenv "HOME"))))

(setq ORG-FILE (concat HOME "/init/renesas_timesheet.org.gpg"))

(add-to-list 'load-path (concat HOME "/.emacs.d/lisp"))
(add-to-list 'load-path (concat HOME "/.emacs.d/bookmark+"))

;; Get screen info if on X
(if (= (string-to-number (getenv "SHLVL")) 3)
    (progn
	(setq dimensions (shell-command-to-string "xdpyinfo | grep dimension"))
	(string-match "\\([0-9]+\\)x\\([0-9]+\\) pixels (\\([0-9]+\\)x\\([0-9]+\\)" dimensions)
	(setq width  (string-to-number (match-string 1 dimensions)))
	(setq height (string-to-number (match-string 2 dimensions)))
	)
  (progn
    (setq width  1920)
    (setq height 1080)))

(when is-me
  (when (> emacs-major-version 23) ;; Works on 24 onwards
    ;; Lets get packages set up
    (require 'package) ;; You might already have this line
    (add-to-list 'package-archives
		 '("melpa" . "https://melpa.org/packages/"))
    (when (< emacs-major-version 24) ;; Never to be used in this setting
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
    
    (add-to-list 'package-archives
		 '("marmalade" . "https://marmalade-repo.org/packages/"))
    (package-initialize) ;; You might already have this line
    
    (when (not package-archive-contents)
      (package-refresh-contents))
    (defvar my-packages '(clojure-mode
			  paredit
			  clojure-mode-extra-font-locking
			  cider
			  smex
			  projectile
			  rainbow-delimiters
			  magit
			  groovy-mode))
    (dolist (p my-packages)
      (when (not (package-installed-p p))
	(package-install p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))
(load (concat HOME "/.emacs.d/lisp/markerpen.el"))

;; Below requires some other stuff -add to package and update init
;;(load (concat HOME "/.emacs.d/lisp/clojure-cheatsheet.el"))

;;; KB macros

;;; Magit short-cut
(global-set-key (kbd "C-x g") 'magit-status)

;(setq select-active-regions nil) ; KLUDGE waiting for propery-notify
;; ORG mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))

(require 'epa-file) ;easyPG interface for GnuGPG
(require 'csv-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

; Setup decent colours
(require 'color-theme)
(color-theme-initialize)
(color-theme-goldenrod)

;; Other nice stuff
(require 'other-modes);
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'tabbar)			;This gives tabs to switch between buffers in this windows
(tabbar-mode)				;Activate now

(require 'my-auto-insert)		;Insert header and footers in various files

(autoload 'folding-mode "folding" "Folding mode" t) ;mode to hide/show parts of text
(global-auto-revert-mode t)			    ;Reverts files to latest after 5 seconds if changed on disk

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;# NOTE Higlight NOTE FIXME KLUDGE words in buffer
(require 'fic-mode)
(setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook))
(mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes)

(add-hook 'ruby-mode-hook
          (lambda ()
            (outline-minor-mode)
            (setq outline-regexp " *\\(def \\|class\\|module\\)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Highlight changes in file
;;;(require 'hilit-chg)
;;;(make-empty-face 'highlight-changes-saved-face)
;;;(setq highlight-changes-face-list '(highlight-changes-saved-face))
;;;
;;;(defun DE-highlight-changes-rotate-faces ()
;;;  (let ((toggle (eq highlight-changes-mode 'passive)))
;;;    (when toggle (highlight-changes-mode t))
;;;    (highlight-changes-rotate-faces)
;;;    (when toggle (highlight-changes-mode nil))))
;;;					; Example: activate highlight changes with rotating faces for C programming
;;;(mapcar (lambda (mode)
;;;	  (add-hook mode
;;;		    (function (lambda ()
;;;				(add-hook 'local-write-file-hooks 'DE-highlight-changes-rotate-faces)
;;;				(highlight-changes-mode t)
;;;				(highlight-changes-mode t)))))
;;;	p-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rake-mode)

(winner-mode 1)				;Allows revert windows content/position history w/ C-c <|> 

;;; PAREDIT

(require 'paredit)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;;; Start Paredit Mode on the fly with `M-x enable-paredit-mode RET',
;;; or always enable it in a major mode `M' (e.g., `lisp') with:

(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;;; Customize paredit using `eval-after-load':

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "ESC M-A-C-s-)")
       'paredit-dwim)))


					; Test out ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
;(setq ido-file-extensions-order '(".tcl" ".pl"))
(ido-mode 1)

(ffap-bindings)				;ffap = fINDfILEaTPoint

;; Stop stupid bell
(setq visible-bell t)

;; Might be needed
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; NOTE  F1 is shell instead of "Help"
(global-set-key [f1] 'shell)
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-same-window)))
(global-set-key [f2] 'rename-buffer)

; NOTE Outline mode magic TAB
(eval-after-load 'outline
  '(progn
     (require 'outline-magic)
     (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Setup own function keys
(defun truncate-lines-toggle ()
  (interactive)
  (if (equal truncate-lines t)
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (redraw-display))
(global-set-key (kbd "C-t") 'truncate-lines-toggle)

(defun my-dirs ()
  (interactive)
  (dirs) (redraw-display)
  (previous-line) (move-beginning-of-line nil) (kill-line)
  (end-of-buffer)
  (insert "cd ")
  (insert (yank)))
					; My own function keys
(global-set-key (kbd "C-'") 'erase-buffer)
(global-set-key (kbd "C-0") 'find-file-at-point)
(global-set-key (kbd "C-x r p") 'replace-rectangle)
(global-set-key (kbd "M-<return>") 'my-dirs)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

					; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq revert-without-query (quote (".*")))

;; Various useful settings
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy (lambda (pattern) t))
(tool-bar-mode nil)
(show-paren-mode t)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ansi colors
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
;(setq ansi-color-map (ansi-color-make-color-map))      # this helped
;; (setq ansi-color-names-vector ; better contrast colors
;;       ["black" "red4" "green4" "yellow4"
;;        "blue3" "magenta4" "cyan4" "white"])

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					; Other stuff
;; FIXME  make completion buffers disappear after 3 seconds.
;; FIXME (add-hook 'completion-setup-hook
;; FIXME   (lambda () (run-at-time 3 nil
;; FIXME     (lambda () (kill-buffer "*Completions*")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

					; Start maximized
;; (defun toggle-fullscreen (&optional f)
;;   (interactive)
;;   (let ((current-value (frame-parameter nil 'fullscreen)))
;;     (set-frame-parameter nil 'fullscreen
;;  			 (if (equal 'fullboth current-value)
;;  			     (if (boundp 'old-fullscreen) old-fullscreen nil)
;;  			   (progn (setq old-fullscreen current-value)
;;  				  'fullboth)))))
;; 
;; (global-set-key [f11] 'toggle-fullscreen)
;; (add-hook 'after-make-frame-functions 'toggle-fullscreen)

(setq undo-outer-limit 1200000000)

(setq HEIGHT (cond
	      ((= height 1080) 90)
	      (t 120)))
(setq FONTS '(70 80 90 100 120 150))

(defun ek-font ()
  (interactive)
  (setq HEIGHT (car FONTS))
  (setq FONTS (cdr FONTS))
  (setq FONTS (append FONTS (list HEIGHT)))
  (custom-set-faces
   `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
			   :strike-through nil :overline nil :underline nil
			   :slant normal :weight normal
			   :height ,HEIGHT :width normal
			   :foundry "bitstream" :family "Courier"))))))
(global-set-key (kbd "<f4>") 'ek-font)

;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/init/ridge_timesheet.org.gpg")))
 '(package-selected-packages
   (quote
    (groovy-mode smex rainbow-delimiters projectile paredit magit clojure-mode-extra-font-locking cider)))
 '(safe-local-variable-values
   (quote
    ((outline-egexp . "# [*]+")
     (fic-mode . 1)
     (Hi-lock
      ("#.*"
       (0
	(quote hi-blue)
	t)))
     (hi-lock-mode . 1)
     (outline-minor-mode . 1))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(warning-supress-types (quote ((undo discard-info)))))
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

;;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
