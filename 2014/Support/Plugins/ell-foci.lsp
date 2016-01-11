;;;  ELL-FOCI.LSP places a point at each focus of 
;;;  a selected true ellipse (not the polyline 
;;;  approximation). Requires GEOMCAL to be loaded 
;;;  or available on the AutoCAD support path. 
;;;  Ellipse can be in any UCS, current or not. 
;;; 
;;;  By Bill Gilliss    bill.gill...@aya.yale.edu 
;;;  Comments always welcome. All rights reserved. 

(defun c:ell-foci ( / entlst ellcen x y M N H disp p1 p2) 
(setq oldsnap (getvar "osmode")) 
(setvar "osmode" 0) 
(setvar "pdmode" 34)  ;;;or change to your favorite 
(if (not (member "geomcal.arx" (arx))) (arxload "geomcal")) 


(setq entlst (entget (car (entsel "Select an ellipse: ")))) 
(if (not (= (cdr (assoc 0 entlst)) "ELLIPSE")) 
  (alert "That is not an ellipse.") 
  (progn 
    (setq ellcen (cdr  (assoc 10 entlst))) 
    (setq x  (cadr  (assoc 11 entlst))) 
    (setq y  (caddr (assoc 11 entlst))) 
    (setq M (sqrt (+ (expt x 2) (expt y 2)))) 
    (setq N (* M (cdr (assoc 40 entlst)))) 
    (setq H (sqrt (- (expt M 2) (expt N 2)))) 
    (setq disp (cdr (assoc 11 entlst))) 
    (c:cal "p1=ellcen+disp*(H/M)") 
    (c:cal "p2=ellcen-disp*(H/M)") 
    (command "point" p1) 
    (command "point" p2) 
    );progn 
);endif 


(setvar "osmode" oldsnap) 
(princ) 
);defun 

