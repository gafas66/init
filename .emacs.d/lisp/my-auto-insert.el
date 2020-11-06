;;----------------------------------------------------------------------------------------------------------------------
;; Created: Friday, September 24 2010
;; Time-stamp: <2020-11-05 23:08:07 erik>
;; Author: Erik KOFOED
;;
;; Description:
;; 

;; -----------------------------------------------------------------------------
;; Auto-Insert Stuff
;; -----------------------------------------------------------------------------

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)

(setq comment-column 60)

(require 'autoinsert)
(add-hook 'find-file-hooks '(lambda () (auto-insert)))

(setq auto-insert-query nil) ;; don't ask, just do it.

(setq user-company "ESK")
(if (file-exists-p "/projects/ASIC/ridge/") (setq user-company "Synaptics\n\n"))
(if (file-exists-p "/proj/ARTPEC7") (setq user-company "Renesas\n\n"))

(setq my-footer
      "\n\n# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################\n")

(setq my-footer2
      "\n\n;; End of file
;;----------------------------------------------------------------------------------------------------------------------
;; Local Variables:
;; comment-column: 60
;; End:
;;----------------------------------------------------------------------------------------------------------------------\n")

(setq auto-insert-alist
      '(
	((cperl-mode . "Perl Program")
	 nil
	 "#! /usr/bin/env perl\n"
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Author: " (user-full-name) ", " user-company
	 "use strict;\n"
	 "use diagnostics;\n\n"
	 _ "\n"
	 my-footer)
	(("\\.csh\\'" . "C Shell")
	 nil
	 "#! /bin/csh\n"
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Author: " (user-full-name) ", " user-company
	 "# Description:\n# " _ "\n"
	 my-footer)
	(("\\.sh\\'" . "Bourne Shell")
	 nil
	 "#! /bin/sh\n"
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Time-stamp: <>\n"
	 "# Author: " (user-full-name) ", " user-company
	 "# Description:\n# " _ "\n"
	 my-footer)
	(("\\.el\\'" . "Emacs lisp file")
	 nil
	 ";;------------------------------------------------------------------------------\n"
	 ";; Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 ";; Author: "(user-full-name) ", " user-company
	 ";; Description:\n;; " _ "\n"
	 my-footer2)
	((tcl-mode . "tcl shell")
	 nil
	 "#! /usr/bin/env tclsh\n"
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Author: "(user-full-name) ", " user-company
	 "# Description:\n#\n" _ "\n"
	 my-footer)
	((ruby-mode . "Ruby mode")
	 nil
	 "#! /usr/bin/env ruby\n"
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Author: "(user-full-name) ", " user-company
	 "# Description:\n# " _ "\n"
	 my-footer)
	((makefile-mode . "Makefile mode")
	 nil
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Author: "(user-full-name) ", " user-company
	 "# Description:\n# " _ "\n"
	 my-footer)
	((makefile-gmake-mode . "GNUMakefile mode")
	 nil
	 "################################################################################\n"
	 "# Created: " (insert (format-time-string "%A, %B %e %Y" (current-time))) "\n"
	 "# Time-stamp: <>\n"
	 "# Author: "(user-full-name) ", " user-company
	 "# Description:\n# " _ "\n"
	 my-footer)))


(provide 'my-auto-insert)

;; End of file
;;----------------------------------------------------------------------------------------------------------------------
;; Local Variables:
;; comment-column: 60
;; End:
;;----------------------------------------------------------------------------------------------------------------------
