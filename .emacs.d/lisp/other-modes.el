;;----------------------------------------------------------------------------------------------------------------------
;; Created: Friday, September 24 2010
;; Time-stamp: <2012-06-12 10:59:51 ekofoed>
;; Author: Erik KOFOED, <COMPANY>
;;
;; Description:
;; Various other modes (bbview, pt)
;; Using generic-x trivial mode maker (KISS - Keep It Simple Stupid!)
;;------------------------------------------------------------------------------

(modify-syntax-entry ?- "w" (standard-syntax-table))
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?. "w" (standard-syntax-table))

(require 'php-mode)
;; My colors
(make-face 'ek-red-face)
(make-face 'ek-yellow-face)
(make-face 'ek-blue-face)
(make-face 'ek-red-bold-face)
(make-face 'ek-cyan-face)
(make-face 'ek-magenta-face)
(make-face 'ek-wheat-face)
(make-face 'ek-orange-face)
(make-face 'ek-green-face)
(make-face 'ek-dark-face)
(custom-set-faces

 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "bitstream" :family "Courier"))))

 '(ek-blue-face     ((t (:foreground "blue"           :size "8pt"))) t)
 '(ek-cyan-face     ((t (:foreground "cyan"           :size "8pt"))) t)
 '(ek-dark-face     ((t (:foreground "dark goldenrod" :size "8pt"))) t)
 '(ek-green-face    ((t (:foreground "green"          :size "8pt"))) t)
 '(ek-magenta-face  ((t (:foreground "magenta"        :size "8pt"))) t)
 '(ek-orange-face   ((t (:foreground "orange3"        :size "8pt"))) t)
 '(ek-red-bold-face ((t (:foreground "red"            :size "8pt" :bold t))) t)
 '(ek-red-face      ((t (:foreground "red"            :size "8pt"))) t)
 '(ek-wheat-face    ((t (:foreground "Wheat3"         :size "8pt"))) t)
 '(ek-yellow-face   ((t (:foreground "yellow"         :size "8pt"))) t)
 )

(require 'generic-x)
;(define-generic-mode
; MODE
; COMMENT-LIST
; KEYWORD-LIST
; FONT-LOCK-LIST
; AUTO-MODE-LIST
; FUNCTION-LIST
; &optional DOCSTRING)

; SOKIT modes
(setq pt-keywords
      '(;; Setup for clocks check
	("Startpoint:" (0 (progn (setq pt-clock 0) 'default-face)))
	;; 'comments'
	("\\*\\*\\*+" .                           'font-lock-comment-face)
	;; Headlines
	(" Point .*"  .                           'ek-blue-face)
	;; violated/met
	("slack (\\(VIOLATED\\))"                 (1 'ek-red-face))
	("slack (\\(MET\\))"                      (1 'ek-green-face))
	;; propagate/ideal clock
	("clock network delay (\\(propagated\\))" (1 'ek-green-face))
	("(\\(ideal\\))"                          (1 'ek-red-bold-face))
	;; Colors for indicating type of backannotation
	(" & " .                                  'ek-green-face) ;lines with only '&' (green)
	(" \\(H\\|*\\|\\^\\|\\$\\|+\\) " .        'ek-magenta-face) ;lines with not '&' (purple)
	;; Colors for various numbers
	("data arrival time +\\(.*\\)"            (1 'ek-magenta-face))
	("data required time +\\(.*\\)"           (1 'ek-magenta-face))
	("delay (propagated) +\\([0-9.]+\\)"      (1 'ek-cyan-face)) ;propagated clock value
	("delay (ideal) +\\([0-9.]+\\)"           (1 'ek-cyan-face)) ;propagated clock value
	("clock source latency +\\([0-9.]+\\)"    (1 'ek-cyan-face)) ;propagated clock value
	("\\( -[0-9\\]+\\.[0-9]*\\)" .            'ek-red-face) ;Negative isolated numbers (light red)
	("\\b\\([0-9\\]+\\.[0-9]*\\)\\b" .        'ek-yellow-face) ;Normal numbers (yellow)
	("^ +[0-9]+ " .                           'ek-yellow-face) ;i.e., fanout is yellow
	;; Highlight anything inside parens (like cell-type)
	("(\\([a-zA-Z0-9_]+\\))"                  (1 'ek-wheat-face)) ;Anything inside paranthesis
	;; Highlight clock names at top
	("(.*clock.* \\([a-zA-Z0-9_']+\\))"       (1 (progn
						       (setq pt-clock (+ 1 pt-clock))
						       (if (= 1 pt-clock)
							   (progn
							     (setq pt-first_clock (match-string 1))
							     'ek-green-face)
							 (if (string= pt-first_clock (match-string 1))
							     'ek-green-face
							   'ek-red-face))))) ;clock name
	("Path \\(Group\\|Type\\):\\(.*\\)"       (2 'ek-yellow-face)) ;clock group
	;; Highlight scenario names when used
	("\\(Scenario\\):\\(.*\\)"
	 (1 'ek-red-face)
	 (2 'font-lock-comment-face))))

(define-generic-mode
    'pt-mode				   ;For primetime reports etc
  '("--")				   ;Comments
  nil					   ;Keywords
  pt-keywords				   ;My list of things to highlight
  '("\\.rpt$")				   ;Auto file
  nil					   ;No other functions to be called
  "PT report mode")			   ;Finally documentation

(provide 'other-modes)

;; End of file
;;----------------------------------------------------------------------------------------------------------------------
;; Local Variables:
;; comment-column: 60
;; End:
;;----------------------------------------------------------------------------------------------------------------------
