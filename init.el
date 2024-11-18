;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(package-install 'use-package)
(setq use-package-always-ensure t)
(require 'bind-key)

;; Tricks for solving connection to melpa
;(eval-after-load 'gnutls
;  '(add-to-list 'gnutls-trustfiles "etc/ssl/certs")) ;Not same all all distros
;(when (>= emacs-major-version 27)
;  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL useful packages

(use-package color-theme-modern
  :config
  (load-theme 'goldenrod t t)
  (enable-theme 'goldenrod))
(use-package per-buffer-theme)

(tool-bar-mode -1)
(winner-mode 1)

(use-package fic-mode)
(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "NOTE"))
(setq p-modes '(tcl-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook emacs-lisp-mode-hook python-mode-hook sh-mode))
(mapcar (lambda (mode) (add-hook mode 'fic-mode)) p-modes)

(use-package clojure-mode)
(use-package cider)
(use-package magit)

(use-package powerline
  :config
  (powerline-default-theme))

(use-package tabbar
  :config
  (tabbar-mode))
(use-package treemacs
  :ensure t
  :config
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-o") 'ace-window))

(use-package helm
  :bind
  (("M-x" . helm-M-x)))
;   ("M-<f5>" . helm-find-files)
;   ([f10]    . helm-buffers-list)
;   ([S-f10]  . helm-recentf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My key defs

(defhydra hydra-toggle (:color blue)
  "Toggle"
  ("l" display-line-numbers-mode "line-numbers")
  ("c" column-number-mode        "columns")
  ("t" toggle-truncate-lines     "truncate")
  ("f" follow-mode               "follow")
  ("v" visual-line-mode          "visual-line")
  ("w" whitespace-mode           "whitespace"))
(global-set-key (kbd "C-c t") 'hydra-toggle/body)

;(bind-keys :prefix-map toggle-map
;           :prefix "C-c x"
;           :prefix-docstring "Keymap for commands that toggle settings."
;           ("c" . column-number-mode)
;           ("d" . toggle-debug-on-error)
;           ("l" . toggle-truncate-lines)
;           ("t" . toggle-frame-tab-bar)
;           ("f" . follow-mode)
;           ("v" . visual-line-mode)
;           ("a" . auto-fill-mode)
;           ("w" . whitespace-mode)
;           ("p" . variable-pitch-mode)
;           ("r" . visible-mode))

(global-set-key (kbd "C-'") 'erase-buffer)
(global-set-key (kbd "C-x r p") 'replace-rectangle)

(global-set-key [f1] 'shell)
(global-set-key [f2] 'rename-buffer)

;(global-set-key (kbd "<f5>") 'ek-hi-set)
(global-set-key (kbd "<f6>") 'highlight-changes-mode)
(global-set-key (kbd "<f7>") 'whitespace-mode)

;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" default))
 '(package-selected-packages
   '(per-buffer-theme tabbar session pod-mode muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode company color-theme-modern browse-kill-ring boxquote bm bar-cursor apache-mode use-package treemacs powerline magit fic-mode cider))
 '(send-mail-function 'sendmail-send-it)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
