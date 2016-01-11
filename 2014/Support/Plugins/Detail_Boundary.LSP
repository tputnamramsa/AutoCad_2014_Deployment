; Detail_Boundary.lsp 
; Create polyline boundary, with radius corners, and graphic link to callout
; Attaches existing SSM callout in drawing to link (pspace only), 
; or places new non-SSM callout (mspace or pspace)

; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

; Note: kcs_ functions used in this file are defined in kcs_base.lsp

;============================================================================|
;(defun c:k()(load (strcat rsa#plugins "Detail_Boundary"))(redraw)(c:ramsa_detail_boundary ))
(defun c:RAMSA_DETAIL_BOUNDARY (/ f:getpt1 layer ltype bname1 bname2 source rad 
                                  wid siz method pt1 pt2 pt3 pt4 ptlist prevpt 
                                  nextpt ent $boundary);
                                
  ; **********  SET VALUES AS DESIRED HERE ***********************************
  (setq ; Common
    layer (aecgeneratelayerkey "AnnoIden") ; "A-Anno-Iden"
    ltype "Dashed2" ; Dashed2 matches AEC Detail Boundary
    bname1 "Detail Tag - SSM" ; SSM callout
    bname2 "Detail Tag" ; non-SSM callout
  )
  (if (kcs_ismetric)
    (setq ; Metric (MM)
      rad 6.25 ; radius for corners
      wid 0.75  ; pline width
      siz 12.5 ; callout size (needed for proper placement) 
;       source "O:\\Reference\\AutoCAD\\2010\\Support\\RAMSA Standards 2010 MM Sans v2.dwg"
      source "O:\\Reference\\AutoCAD\\2012\\Support\\RAMSA Standards 2012 MM Sans.dwg"
    )
    (setq ; Imperial 
      rad 0.25 ; radius for corners
      wid 0.03125  ; pline width
      siz 0.5 ; callout size (needed for proper placement) 
;       source "O:\\Reference\\AutoCAD\\2010\\Support\\RAMSA Standards 2010 I Sans v2.dwg"
      source "O:\\Reference\\AutoCAD\\2012\\Support\\RAMSA Standards 2012 I Sans.dwg"
    )
  )
  ; End of CAD manager variables
  ;***************************************************************************
;   ; Convert vars for metric other than MM
;   (cond
;     ((= (getvar "insunits") 6) ; Meters
;       (setq
;         rad (/ rad 1000.0)
;         wid (/ wid 1000.0)
;         siz (/ siz 1000.0)
;       )
;     )
;     ((= (getvar "insunits") 5) ; CM  
;       (setq
;         rad (/ rad 10.0)
;         wid (/ wid 10.0)
;         siz (/ siz 10.0)
;       )
;     )
;   )
  (if (not (kcs_ispspace))
    (setq 
      rad (/ rad (getvar "cannoscalevalue"))
      wid (/ wid (getvar "cannoscalevalue"))
      siz (/ siz (getvar "cannoscalevalue"))
    )
  )  
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  (defun f:getpt1 (/ prmpt pt)
    (setq 
      method "Rectangle" ; [var is local to main function]
      prmpt "\nFirst corner of Rectangle detail box, or [Circle/Free]: "
      pt method
    )
    (while (= (type pt) 'STR)
      (setq method pt)
      (initget "Rectangle Circle Free")
      (setq pt (getpoint prmpt))
      (cond 
        ((= pt "Rectangle")
          (setq prmpt "\nFirst corner of Rectangle detail box, or [Circle/Free]: ")
        )
        ((= pt "Circle")
          (setq prmpt "\nCenter point of detail Circle, or [Rectangle/Free]: ")
        )
        ((= pt "Free")
          (setq prmpt "\nFirst point of Free boundary, or [Rectangle/Circle]: ")
        )
        ((= (type pt) 'STR)(setq pt nil)) ; user hit Enter
      )
    )
    pt
  );f:getpt1 [local function]
        
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  (kcs_pre)
  (if (null (tblsearch "ltype" ltype))
    (command "_.-linetype" "_Load" ltype "" "")
  )
  (graphscr)
  (kcs_pushvar "filletrad,plinewid,celtype,plinegen,autosnap")
  (setvar "celtype" ltype)
  (setvar "plinewid" 0)
  (setvar "plinegen" 1) ; 1 matches AEC Detail Boundary
  (setvar "filletrad" rad)
  (kcs_pusheval '(redraw))
  (princ "\nDraw Detail Boundary... ")
  (setq pt1 (f:getpt1))
  (or pt1 (exit))
  
  (kcs_pushvar "clayer,osmode")
  (setvar "clayer" layer)
  (setvar "osmode" 0)
  (cond
    ((= method "Rectangle")
      (initget 1)
      (setq pt3 (getcorner "\nOpposite corner of detail box: " pt1))      
      (setq pt2 (list (car pt1) (cadr pt3)))
      (setq pt4 (list (car pt3) (cadr pt1)))
      (command "._pline")
      (apply 'command (list pt1 pt2 pt3 pt4))
      (command "_C")
      (setq ent (entlast))
      (command "_.fillet" "_P" ent)
      (command "._pedit" ent "_Width" wid "")
    ); method "Rectangle"
    
    ((= method "Free")
      (command "._pline" pt1)
      (initget 1)
      (setq pt2 (getpoint "\nNext point of detail boundary: " (getvar "lastpoint")))
      (command pt2)
      (kcs_pusheval '(entdel (entlast)))
      (initget 1)
      (setq pt2 (getpoint "\nNext point of detail boundary: " (getvar "lastpoint")))
      (command pt2)
      (while (setq pt2 (getpoint "\nNext point of detail boundary <close>: " (getvar "lastpoint")))
        (command pt2)
      )
      (command "_Close")
      (kcs_popeval '(entdel (entlast)) nil) ; pop without doing
      (setq ent (entlast))
      (command "_.fillet" "_P" ent)
      (command "._pedit" ent "_Width" wid "")
    ); method "Free"
    
    ((= method "Circle")
      (princ "\nRadius of circle: ")
      (command "._circle" pt1)(while (> (getvar "cmdactive") 0)(command pause))
      (setq 
        rad (kcs_dxf 40 (entget (entlast)))
        pt1 (kcs_dxf 10 (entget (entlast)))
      )
      (entdel (entlast)) ; the circle
      (command "._donut" (+ (* rad 2) wid) (- (* rad 2) wid) pt1 "")
    ); method "Circle"
  );cond
  
  (setq $boundary (entlast))
  (kcs_pusheval '(setq $boundary nil)) ; these two lines must
  (kcs_pusheval '(entdel $boundary))   ; remain in this order!      
  
  (kcs_popvar "celtype" T) ; restore
  (ramsa_boundary_link $boundary size layer)
    
  (kcs_popeval '(entdel $boundary) nil) ; nil = pop without doing
  (kcs_post)   
);ramsa_detail_boundary
  
;============================================================================|
; Create the leader and place the callout
(defun ramsa_boundary_link (ent size layer / obj loop input pt1 pt0 ptlist pt2 prmpt ent)
  (setq 
    loop T
    obj (vlax-ename->vla-object ent)
    pt0 nil pt1 nil
  )
  (princ "\nFirst segment of leader line: ")
  (while loop
    (setq input (grread t 15 0)) ;
    (cond 
      ((= (car input) 5) ; drag
        (if pt0 (grdraw pt1 pt0 0)) ; undraw previous line
        (setq 
          pt1 (cadr input)
          pt0 (vlax-curve-getclosestpointto obj pt1)
        )
        (grdraw pt1 pt0 3) ; draw new line
      )
      ((= (car input) 3) ; pick point
        (setq 
          pt1 (cadr input)
          loop nil ; exit grread
        )
      )
    )
  ); while
  (setq ptlist (list pt1 pt0))
  (kcs_pushvar "orthomode")
  (if (not (kcs_hasbit 8 (getvar "autosnap"))) ; not Polar mode
    (setvar "orthomode" 1) ; turn on if Polar not on
  )
  (while (setq pt1 (getpoint "\nNext point of leader line: <done>: " pt1))
    (setq ptlist (cons pt1 ptlist))
    (grdraw (car ptlist)(cadr ptlist) 3)
  )
  (setq pt2 (polar (car ptlist) (angle (cadr ptlist)(car ptlist)) (* siz 0.5))) ; inspt
  
  (setvar "plinewid" 0)
  (kcs_usermode "off")
  (command "._pline")
  (apply 'command ptlist)
  (command "") 
  (kcs_usermode "on")

  
;   (if (kcs_ispspace)
;     (setq prmpt (strcat "\nSelect existing \"" bname1 "\" callout to attach [or <Enter> for new]... "))
;     (setq ent nil) ; SSM callouts are not placed in mspace - use non-SSM only
;   ) ;; New info: SSM callouts ARE placed in mspace
;   (setq prmpt (strcat "\nSelect existing \"" bname1 "\" callout to attach [or <Enter> for new]... "))
  (setq prmpt "\nSelect existing Detail callout to attach [or <Enter> for new]: ")
  (kcs_pusheval '(entdel (entlast))) ; delete link if canceled here
  (while prmpt
    (setvar "errno" 0)
    (setq ent (car (entsel prmpt)))
    (cond
      ((= (getvar "errno") 7) ; missed pick
        (princ "\n  Missed pick - try again... ")
      )
      ((= (getvar "errno") 52) ; Enter or Spacebar
        (setq prmpt nil) ; exit the loop
      )
;       ((or (/= (kcs_dxf 0 (entget ent)) "INSERT")(/= (kcs_dxf 2 (entget ent)) bname1))
;         (princ "\n  Select object is not a valid callout - Try again... ")
;       )
      ((/= (kcs_dxf 0 (entget ent)) "INSERT")
        (princ "\n  Select object is not a block - Try again... ")
      )
      ((and (/= (kcs_dxf 2 (entget ent)) bname1)(/= (kcs_dxf 2 (entget ent)) bname2))
        (initget "Yes No")
        (if (/= "No" (getkword 
              "\n  Selected object is not RAMSA detail callout - use it anyway <Yes>: ")
            )
          (setq prmpt nil) ; selected block accepted - exit loop
        )
      )
      (t (setq prmpt nil)) ; callout selected - exit the loop
    )
  )
  (kcs_popeval '(entdel (entlast)) nil) ; pop without doing
  (if ent 
    (progn ; SSM callout selected
      (setq pt1 (kcs_dxf 10 (entget ent)))
      (command "._move" ent "" pt1 pt2)
    )
    (progn ; place non-SSM callout
      (if (null (tblsearch "block" bname2))(progn
        (if (null ramsa_extract_block)
          (load (strcat rsa#plugins "RAMSABLOCK"))
        )
        (ramsa_extract_block bname2 source)
      ))

      (command "._-insert" bname2 pt2 1) ; block is annotative (no dimscaling needed)
      (while (> (getvar "cmdactive") 0)
        (command "")
      )
      (princ (strcat "\nCallout \"" bname2 "\" placed. "))(princ)
    ) 
  )
    
);ramsa_boundary_link

;============================================================================|
;============================================================================|

