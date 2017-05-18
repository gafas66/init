;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outline-mode-easy-bindings.el
;;
;; Installation: Store this file as outline-mode-easy-bindings.el
;; somewhere in your load-path and create hooks for outline modes to
;; load this automatically, for example:

;;     (add-hook 'outline-mode-hook 'my-outline-easy-bindings)
;;     (add-hook 'outline-minor-mode-hook 'my-outline-easy-bindings)
;;
;;     (defun my-outline-easy-bindings ()
;;       (require 'outline-mode-easy-bindings nil t))


(defun outline-body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun outline-body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun outline-subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

(defun outline-subheadings-visible-p ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

(defun outline-hide-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-body-p)
                (outline-body-visible-p))
           (hide-entry) (hide-leaves))
          (t (hide-subtree)))))

(defun outline-show-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-subheadings-p)
                (not (outline-subheadings-visible-p)))
           (show-children))
          ((and (not (outline-subheadings-p))
                (not (outline-body-visible-p)))
           (show-subtree))
          ((and (outline-body-p)
                (not (outline-body-visible-p)))
           (show-entry))
          (t (show-subtree)))))


(let ((major outline-mode-map)
      (minor outline-minor-mode-map))

  (define-key major (kbd "M-<left>") 'outline-hide-more)
  (define-key major (kbd "M-<right>") 'outline-show-more)
  (define-key major (kbd "M-<up>") 'outline-previous-visible-heading)
  (define-key major (kbd "M-<down>") 'outline-next-visible-heading)
  (define-key major (kbd "C-c J") 'outline-hide-more)
  (define-key major (kbd "C-c L") 'outline-show-more)
  (define-key major (kbd "C-c I") 'outline-previous-visible-heading)
  (define-key major (kbd "C-c K") 'outline-next-visible-heading)

  (define-key minor (kbd "M-<left>") 'outline-hide-more)
  (define-key minor (kbd "M-<right>") 'outline-show-more)
  (define-key minor (kbd "M-<up>") 'outline-previous-visible-heading)
  (define-key minor (kbd "M-<down>") 'outline-next-visible-heading)
  (define-key minor (kbd "C-c J") 'outline-hide-more)
  (define-key minor (kbd "C-c L") 'outline-show-more)
  (define-key minor (kbd "C-c I") 'outline-previous-visible-heading)
  (define-key minor (kbd "C-c K") 'outline-next-visible-heading))


(provide 'outline-mode-easy-bindings)

;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
