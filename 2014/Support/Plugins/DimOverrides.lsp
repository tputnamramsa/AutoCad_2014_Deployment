;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loads two commands for overriding arrow types
;; Many thanks to Luis Esqivel, who posted these to the newsgroup in response to my query
;;;;;;;;;;;;;;;;;;;;;;;;;

;;Dot override
(defun C:DOTARROW ()
(setq data (entsel))
(setq ename (car data))
(setq pt (cadr data))
(setq obj (vlax-ename->vla-object ename)) (setq elist (entget ename)) (setq p10 (cdr (assoc 10 elist))) (setq p13 (cdr (assoc 13 elist))) (setq px (list (car p13) (cadr p10) 0.0)) (if (> (distance pt p10) (distance pt px)) (vla-put-Arrowhead1Type obj acArrowDot) (vla-put-Arrowhead2Type obj acArrowDot))
(princ))

;;Arrow Override
(defun C:POINTARROW ()
(setq data (entsel))
(setq ename (car data))
(setq pt (cadr data))
(setq obj (vlax-ename->vla-object ename)) (setq elist (entget ename)) (setq p10 (cdr (assoc 10 elist))) (setq p13 (cdr (assoc 13 elist))) (setq px (list (car p13) (cadr p10) 0.0)) (if (> (distance pt p10) (distance pt px)) (vla-put-Arrowhead1Type obj acArrowDefault) (vla-put-Arrowhead2Type obj acArrowDefault))
(princ))
