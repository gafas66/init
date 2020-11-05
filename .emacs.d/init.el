;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file (non-existing by default)

(tool-bar-mode -1)

;; Bookeeping

(setq is-DL (if (file-exists-p "/projects/ASIC/ridge/") t nil))
(setq is-me (if (or (string= "ekofoed" (getenv "USER"))
		    (string= "erik" (getenv "USER"))) t nil))

(setq HOME (cond
	    (is-DL "/home/ekofoed")
	    (t (getenv "HOME"))))

(add-to-list 'load-path (concat HOME "/.emacs.d/lisp"))

(setq is-linux (if (string= system-type "gnu/linux") t nil))

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
      ((= height 1080) (set-face-attribute 'default nil :height 900))
      (t nil))

(when is-me
  (when (> emacs-major-version 25) ;; Works on 24 onwards
    ;; Lets get packages set up
    (require 'package) ;; You might already have this line
    (add-to-list 'package-archives
		 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (package-initialize) ;; You might already have this line
    
    (when (not package-archive-contents)
      (package-refresh-contents))
    (defvar my-packages '(clojure-mode paredit cider rainbow-delimiters magit color-theme-modern powerline which-key tabbar))
    (dolist (p my-packages)
      (when (not (package-installed-p p))
	(package-install p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))
(load (concat HOME "/.emacs.d/lisp/markerpen.el"))

;;; KB macros

;;; Magit short-cut
(global-set-key (kbd "C-x g") 'magit-status)

(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))

(require 'epa-file) ;easyPG interface for GnuGPG
(require 'csv-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;; Setup decent colours
;(require 'color-theme)
(load-theme 'goldenrod t t)
(enable-theme 'goldenrod)

;; Other nice stuff
(require 'other-modes)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;(require 'tabbar)			;This gives tabs to switch between buffers in this windows
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rake-mode)

(winner-mode 1)				;Allows revert windows content/position history w/ C-c <|> 

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
(add-hook 'shell-mode-hook (lambda () (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

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

(global-set-key (kbd "<f8>")  'markerpen1)
(global-set-key (kbd "<f9>")  'markerpen4)
(global-set-key (kbd "<f10>") 'markerpen9)
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

(defun ek-set-faces (HEIGHT)
  (custom-set-faces
   `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
			   :strike-through nil :overline nil :underline nil
			   :slant normal :weight normal
			   :height ,HEIGHT :width normal
			   :foundry "bitstream" :family "Courier"))))))

;(server-start) ;; For emacs to listen

;;; End of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters paredit magit color-theme-modern cider)))
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
