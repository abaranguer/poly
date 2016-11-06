;; Dividing Polynoms:
;; N: x^3 + -12*x^2 + -42
;; D: x + -3
;; -------------------------
;; Q: x^2 + -9*x + -27
;; R: -123
;;
;;    (1,3)  (-12,2)  (0,1)  (-42,0) | (1,1) (-3,0) 
;;   -(1,3)   -(-3,2)                  (1,2)(-9,1) (-27,0)
;;    -------------
;;       0    (-9,2)   (0,1)  
;;           -(-9,2) -(27,1)
;;    ----------------------
;;                0  (-27,1) (-42,0)
;;                  -(-27,1) -(81,0) 
;;    ------------------------------
;;                        0  (-123,0)
;;
;; numerical example
;; 6   5   4  3   2   1    1  2 3
;;-6 -12 -18               6 -7 0 24
;;----------
;;    -7 -14  3   2   1
;;     7  14 21
;;---------------------
;;         0 24   2   1
;;         0  0   0
;;---------------------
;;           24   2   1
;;          -24 -48 -72
;;---------------------
;;              -46 -71

(progn
  (setq coefs (list))
  (setq rest (list))

  (defun show-poly (poly)
    (if (>= (length poly) 1)
	(progn
	  (setq exponent (1- (length poly)))
	  (insert (format "(%s*(x^%d))" (car poly) exponent))
	  (if (> exponent 0)
	      (progn
		(insert "+")
		(show-poly (cdr poly)))))))

  (defun get-quotient (num den)
    (setq lnum (length num))
    (setq lden (length den))
    (setq quotient (list))

    (setq high-coef-num (car num))
    (setq high-coef-denom (car den))
    (setq coef (/ high-coef-num high-coef-denom))

    (dotimes (i lden)
      (setq quotient (append quotient (list (- (nth i num) 
                                      (* coef (nth i den)))))))
    (dotimes (i (- lnum lden))
      (setq quotient (append quotient (list (nth (+ lden i) num)))))

    (newline)
    (insert "Last rest: ")
    (show-poly quotient)
    (newline)
    (cons coef quotient))

  (defun divide (numerator denominator)
    (setq lnum (length numerator))
    (setq lden (length denominator))
    
    (if (>= lnum lden)   
	(progn 
          (setq pair (get-quotient numerator denominator))
          (setq coefs (append coefs (list (car pair))))
          (divide (cdr (cdr pair)) denominator))))

  ;; main
  (newline)
  (newline)
  (insert "Polynomial division")
  (newline)
  (setq num '(1 2 1))
  (setq den '(1 1))
  (newline)
  (insert "divide")
  (newline)
  (show-poly num)
  (newline)
  (insert "by")
  (newline)
  (show-poly den)
  (newline)
  (divide num den)
  (newline)
  (insert "Quotient: ")
  (show-poly coefs)
  (newline))