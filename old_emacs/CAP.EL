; LISP code to alter/modify capacitance file
; Will change unit to fF and write out readable
; for powrmill. 
; Output shall be sorted with highest capacitance on top

(setq -N- "
")

(defun cadr   (module) (car (cdr module)))
(defun caddr  (module) (car (cdr (cdr module))))
(defun cadddr (module) (car (cdr (cdr (cdr module)))))

(setq list-of-deletions
      (list
       (list (concat -N- "\\+ *") " ")
       (list (concat -N- "\\*.*") "")
       (list "[ 	]+" " ")
       (list (concat -N- "[ 	]*$") "")))

(defun spice-cleanup-buffer ()
  "Removes multiple lines, multiple spaces and comments."
  (interactive)
  (while (not (null list-of-deletions))
    (goto-char (point-min))
    (while (re-search-forward (car (car list-of-deletions)) nil t)
      (replace-match (cadr (car list-of-deletions)) nil nil))
    (setq list-of-deletions (cdr list-of-deletions)))
  (goto-char (point-min))
  (while (re-search-forward "^\\.SUB" nil t)
    (replace-match (concat -N- "\\&") nil nil)))

(defun get-line ()
  "Returns current line, as a list"
  (interactive)
  (split-string (buffer-substring
		 (re-search-backward "^" nil t)
		 (re-search-forward  "$" nil t))
		" "))

(defun string-to-femtofarad (string)
  "Returns the number of femtofarads string represents"
  (interactive "s")
  (let (num-list)
    (setq num-list (split-string string "E"))
    (* (string-to-number (car num-list))
       (expt 10 (+ 15 (string-to-number (cadr num-list)))))))

(defun add-to-associative-list ()
  "Will add nodes in current buffer to associative list"
  (interactive)
  (setq node-list nil)
  (setplist 'node-list nil)
  (goto-char (point-min))
  (spice-cleanup-buffer)
  (goto-char (point-min))
  (while (not (equal (point) (point-max)))
    (let (in-line first second value)
      (setq in-line (get-line))
      (if (intern-soft (cadr in-line))
	  (setq first (intern-soft (cadr in-line)))
	(setq first (intern (cadr in-line))))
      (if (intern-soft (caddr in-line))
	  (setq second (intern-soft (caddr in-line)))
	(setq second (intern (caddr in-line))))
      (setq value (string-to-femtofarad (cadddr in-line)))
      (if (null (get 'node-list first))
	  (put 'node-list first 0.0))
      (if (null (get 'node-list second))
	  (put 'node-list second 0.0))
      (put 'node-list first
	   (+ value (get 'node-list first)))
      (put 'node-list second
	   (+ value (get 'node-list second))))
    (forward-line))
  (switch-to-buffer "*output from add-to-associative-list*")
  (setq out-list (symbol-plist 'node-list))
  (while (not (null out-list))
    (insert (format "%s %s\n" (car out-list) (cadr out-list)))
    (setq out-list (cdr (cdr out-list))))
  (sort-float-fields 2 (point-min) (point-max)))
