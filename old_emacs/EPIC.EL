; The purpose of these functions are to modify an lvs.spi
; into something easier for PowerMill to handle

(load-file "/pri/ek/lisp/cap.elc")

(setq clock-control "
* (ek) insertion start
X1 PWR_ADC
+ iADC_CK0 ADC_CK0
+ VDD GND AN2

X2 PWR_ADC
+ iADC_CK0B ADC_CK0B
+ VDD GND AN2

X3 PWR_ADC
+ iADC_CK0B2 ADC_CK0B2
+ VDD GND AN2

X4 PWR_ADC
+ iADC_CK_1_ ADC_CK_1_
+ VDD GND AN2

X5 PWR_ADC
+ iADC_CK_2_ ADC_CK_2_
+ VDD GND AN2

X6 PWR_ADC
+ iADC_CK_3_ ADC_CK_3_
+ VDD GND AN2
* (ek) insertion end
") 

(defun make-epic-noprom-noana ()
  "Modifies buffer so that prom and analogue part is
   removed."
  (goto-char (point-min))
  (spice-cleanup-buffer)
  (replace-regexp "^\\.SUBCKT SP13_2127.*" 
		  (concat "\\&" clock-control))
  (replace-regexp "^XSP13_ANA.*" "")
  (replace-regexp "^\\(PROMUC.* PGDO\\).*" "\\1 "))
