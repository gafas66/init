; Lisp program to modify Jon B. verilog netlist
; Written by Erik Kofoed, 1/2-1997
; My first lisp program!!
; All functions have e- prefix (for erik-)

(setq g-newline "
")

(defun cadr  (list) "" (car (cdr list)))
(defun caddr (list) "" (car (cdr (cdr list))))
(defun cadddr (list) "" (car (cdr (cdr (cdr list)))))

(setq inside-reg-exp  "\\.\\w+ *( *\\w+\\[[0-9]+:")
(setq outside-reg-exp "\\.\\w+ *( *\\w+,")

(defun e-debug ()
  "Make clear for debug"
  (interactive)
  (revert-buffer t t)
  (beginning-of-buffer))

(defun e-replace-regexp (e-regular-exp e-to-string)
  ""
  (while (re-search-forward e-regular-exp nil t)
    (replace-match e-to-string nil nil)))

;
; Cleanup before running
; i.e., remove unecessary modules++

(defun e-pre-cleanup-buffer ()
  "Will remove comments, primitives etc"
  (interactive)
  (setq list-of-deletions 
	(list
	 '("/\\*" "\\*/" nil "")
	 '("^module cds_a" "^endmodule" nil "")
	 '("^primitive" "^endprimitive" nil "")
	 '("^ *reg" "( *Q, *QT)" nil "")
	 '("^module BUFF4" "^endmodule" nil "")
	 '("^\\(VDD \\|VSS \\)" ";" nil "")
	 '("^module ANALOG_TOP" "^endmodule" nil "")
	 '(", \\.AVDD(CONST" "_0)" nil "")
	 '("^module BPAD" "^endmodule" nil "")
	 '("^module OPAD" "^endmodule" nil "")
	 '("^module IPAD_PD" "^endmodule" nil "")
	 '("^module VPPPAD" "^endmodule" nil "")
	 '("^module VSSPAD" "^endmodule" nil "")
	 '("^module VPP_PAD" "^endmodule" nil "")))
  (while (not (null list-of-deletions))
    (beginning-of-buffer)
    (while (and (not (null list-of-deletions))
		(re-search-forward (car (car list-of-deletions)) nil t))
      (setq fpoint (match-beginning 0))
      (re-search-forward (cadr (car list-of-deletions)) nil t)
      (if (null (caddr (car list-of-deletions)))
	  (delete-region fpoint (match-end 0))
	(narrow-to-region fpoint (match-end 0))
	(beginning-of-buffer)
	(e-replace-regexp (caddr (car list-of-deletions))
			  (cadddr (car list-of-deletions)))
	(widen)
	(setq list-of-deletions (cdr list-of-deletions))))
    (setq list-of-deletions (cdr list-of-deletions)))
  (e-replace-regexp "[{}]" "")
  (beginning-of-buffer))

; Cleanup buffer after running

(defun e-cleanup-buffer ()
  "Cleans up buffer, removes regexp-to-remove"
  (interactive)
  (beginning-of-buffer)
  (e-replace-regexp ";+" ";")
  (beginning-of-buffer)
  (e-replace-regexp (concat "^[ ]*;[ ]*" g-newline) "")
  (beginning-of-buffer)
  (e-replace-regexp (concat g-newline "+") g-newline))

; Select and narrow in on module

(defun e-select-module (module)
  "This function narrows in to the given module declaration."
  (interactive "s")
  (setq expr-module (concat "^module " module))
  (search-forward-regexp expr-module)
  (beginning-of-line)
  (setq fpoint (point))
  (search-forward ")")
  (narrow-to-region (point) fpoint) 
  (beginning-of-buffer)
  (search-forward "(")
  (e-replace-regexp "[^!-~]" "")
  (beginning-of-buffer)
  (e-replace-regexp "[,(]" (concat "\\&" g-newline))
  (beginning-of-buffer))


; Function to count number of violating pins from (point)
; After running, (point) is at the first violation

(defun e-count-buses ()
  "Returns the number of .name (bus[]) in the code"
  (interactive)
  (string-to-int (car (split-string (count-matches inside-reg-exp) " "))))

; Now, lets collect data on the bus, and split it in place

(defun e-split-bus-in-place ()
  "Will split .name (bus[x:y]) into .name (bus[x],..bus[y])
   Returns a list: (bus from to)"
  (interactive)
  (search-forward-regexp inside-reg-exp)
  (search-backward-regexp "[,(]")
  (search-forward-regexp "[a-zA-Z_]") (forward-char -1)
  (setq start-point (point))
  (search-forward "[") (forward-char -1)
  (setq name-of-bus (buffer-substring start-point (point)))
  (forward-char) (setq fpoint (point))
  (search-forward ":") (forward-char -1)
  (setq msb-of-bus (string-to-int (buffer-substring fpoint (point))))
  (forward-char) (setq fpoint (point))
  (search-forward "]") (forward-char -1)
  (setq lsb-of-bus (string-to-int (buffer-substring fpoint (point))))
; Delete original and generate single pins
  (forward-char)
  (delete-region start-point (point))
  (setq msb-of-bus (+ msb-of-bus 1))
  (while (not (= msb-of-bus lsb-of-bus))
    (setq msb-of-bus (- msb-of-bus 1))
    (insert (concat name-of-bus "[" msb-of-bus "]"))
    (if (not (= msb-of-bus lsb-of-bus))
	(insert ",")))
  (list name-of-bus msb-of-bus lsb-of-bus))

; After running, (point) is at the first violation

(defun e-count-violating-pins ()
  "Returns the number of violating pins in verilog"
  (interactive)
  (search-forward-regexp outside-reg-exp nil t)
  (search-backward ".")
  (string-to-int 
   (car 
    (split-string (count-matches outside-reg-exp) " "))))

; Will return number of lines on page, i.e., # of pins

(defun e-split-to-lines ()
  "Split violating pins, one per line"
  (interactive)
  (setq fpoint (point))
  (insert g-newline)
  (search-forward ")" )
  (narrow-to-region fpoint (point))
  (beginning-of-buffer)
  (e-replace-regexp "[^!-~]+" "")
  (beginning-of-buffer)
  (e-replace-regexp "," (concat "," g-newline))
  (setq out-list (split-string (count-lines-page) " "))
  (string-to-int (car (cdr (cdr out-list)))))

; Now, get name of bus
; Expects to start on period (.)

(defun e-get-bus-name ()
  "Returns the name of the current bus name."
  (interactive)
  (beginning-of-buffer)
  (forward-char)
  (setq fpoint (point))
  (search-forward "(" )
  (backward-char)
  (buffer-substring fpoint (point)))

; Let's start to work! Expects to start on period?
; 1. Split to lines/get number of lines
; 2. Get name of bus
; 3. Split to bus[#] MSb is first

(defun e-split-bus ()
  "Will split one pin per line, count and split.
  Returns: ((module-name bus-name) length)"
  (interactive)
  (setq return-list (e-split-to-lines))
  (setq num-of-pins (- return-list 1))
  (setq name-of-bus (e-get-bus-name))
  (search-forward "(")
  (forward-char -1)
  (insert (concat "[" num-of-pins "] "))
  (while (not (zerop num-of-pins))
    (setq num-of-pins (- num-of-pins 1))
    (beginning-of-line)
    (search-forward ",")
    (forward-char -1)
    (insert ")")
    (beginning-of-line)
    (forward-line)
    (insert (concat "." name-of-bus "[" num-of-pins "] " "(")))
  (end-of-buffer) (insert g-newline)
  (widen)
; Lets get the name of the module called
  (search-backward-regexp ";")
  (search-forward-regexp "[A-_a-z]")
  (forward-char -1) (setq fpoint (point))
  (search-forward " ") (forward-char -1)
  (list (list
	 (buffer-substring fpoint (point))
	 name-of-bus)
	return-list))

; Now, split the named busses in the module definitions

(defun e-module-fix (module)
  "module = ((name bus) #pins)"
  (beginning-of-buffer)
  (e-select-module (car (car module)))
  (setq bus-name (cadr (car module)))
  (setq bus-length (cadr module))
  (search-forward-regexp (concat "^" bus-name "[,)]"))
  (forward-char -1)
  (if (equal (buffer-substring (point) (+ (point) 1)) ")")
      (setq final-line (concat ")" g-newline))
    (setq final-line (concat "," g-newline)))
  (delete-region (point) (+ (point) 1))
  (setq end-line (concat "," g-newline))
  (setq bus-length (- bus-length 1))
  (insert (concat "[" bus-length "]" end-line))
  (beginning-of-line)
  (forward-line)
  (while (not (zerop bus-length))
    (setq bus-length (- bus-length 1))
    (if (zerop bus-length)
	(insert (concat bus-name "[" bus-length "]" final-line))
      (insert (concat bus-name "[" bus-length "]" end-line))))
; Add output declaration
  (beginning-of-buffer) (forward-line)
  (setq fpoint (point))
  (end-of-buffer)
  (insert ";" g-newline g-newline (buffer-substring fpoint (point)))
  (setq fpoint (point))
  (search-backward ";") 
  (beginning-of-line) (forward-line 2)
  (narrow-to-region (point) fpoint)
  (beginning-of-buffer)
  (e-replace-regexp "^" "output ")
  (beginning-of-buffer)
  (e-replace-regexp "[,)]" ";")
  (widen)
; Now remove old output/input specifications
  (setq fpoint (point)) (search-forward-regexp "^endmodule")
  (narrow-to-region fpoint (point))
  (beginning-of-buffer)
  (e-replace-regexp "^[ ]*\\(output\\|input\\)[^;]+" "")
  (widen))

; Main program

(defun e-fix-pins ()
  "Purpose is to split buses were referenced to pins"
  (interactive)
  (e-pre-cleanup-buffer)
  (while (not (zerop (e-count-buses)))
    (e-split-bus-in-place))
  (setq module-list ())
  (beginning-of-buffer)
  (setq num-of-violations (e-count-violating-pins))
  (while (not (zerop num-of-violations))
    (setq next-to-pin-list (e-split-bus))
    (if (not (assoc (car next-to-pin-list) module-list))
	(setq module-list (cons next-to-pin-list module-list))
      (setq old-def (assoc (car next-to-pin-list) module-list))
      (if (> (cadr next-to-pin-list) (cadr old-def))
	  (subst 'next-to-pin-list 'old-def module-list)))
    (setq num-of-violations (e-count-violating-pins)))
  (while (not (null module-list))
    (setq current-module (car module-list))
    (setq module-list (cdr module-list))
    (e-module-fix current-module))
  (e-cleanup-buffer)
  (print "Done removing buses"))
