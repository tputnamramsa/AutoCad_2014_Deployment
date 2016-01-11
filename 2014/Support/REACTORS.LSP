; REACTORS.LSP
; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

; Revised 2/2/10 for R2010
; Revised 8/30/10 for Placeview callout layer control
; Revised 3/7/12: ramsa_sysvarChanged added [KCS]
; Revised 4/23/12:  ramsa_react_xattach added [KCS]
; Revised 2/15/13  changed layer key for vports to "ViewPort" [CP/W2]

; Note: kcs_ functions used in this file are defined in kcs_base.lsp


;(defun c:k()(load (strcat rsa#support "reactors")))
;============================================================================|
;; Look for reactors already present 
;; (prevents reactor accumulation when using SDI)
(if (= (getvar "sdi") 1)
  (foreach rtype (vlr-reactors) 
    (foreach react (cdr rtype) 
      (setq temp (cdar (vlr-reactions react)))
      (if (= temp 'ramsa_CMDSTART)(setq ramsa#reactors T))
    )
  )
)

; Create the reactors (if not already done)
(if (null ramsa#reactors)(progn
  (vlr-command-reactor nil '((:vlr-commandWillStart . ramsa_cmdStart)))
  (vlr-command-reactor nil '((:vlr-commandEnded . ramsa_cmdEnd)))
  (vlr-command-reactor nil '((:vlr-commandCancelled . ramsa_cmdCancel)))
  (vlr-acdb-reactor nil '((:vlr-objectAppended . ramsa_objAppended)))
  (vlr-sysvar-reactor nil '((:vlr-sysVarChanged . ramsa_sysvarChanged)))
))
(setq ramsa#reactors T)

;============================================================================|
; Define the callback functions (the functions called directly by the reactors)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
(defun ramsa_cmdStart (reactor cmdlist / lay)
  (setq ramsa$cmdStart (car cmdlist)) ; temp global for cmdEnd
;   (princ "\nramsa_cmdStart: ")(prin1 ramsa$cmdStart) ;**************************
  (cond
    ((and (= ramsa$cmdStart "PLACEVIEW") ramsa#reactors)
      (setq 
        ramsa$clayer (getvar "clayer")
        lay (aecgeneratelayerkey "AnnoIden") ; "A-Anno-Iden" - layer for Callout
      )
      (setvar "clayer" lay)
    )
  )
  (princ)
);ramsa_cmdStart

(defun ramsa_cmdEnd (reactor cmdlist)
  (if ramsa#reactors
    (cond
      ((= ramsa$cmdStart "PLACEVIEW") (ramsa_react_placeview))
      ((= ramsa$cmdStart "CALLOUT")
        (vla-put-layer (vlax-ename->vla-object (entlast)) (aecgeneratelayerkey "AnnoIden")) ; layer for Callout  
      )
      ((= ramsa$cmdStart "XATTACH")(ramsa_react_xattach))
      ; add others here (future)
    )
    (if (member ramsa$cmdStart '("PLACEVIEW" "CALLOUT" "XATTACH"))
      (princ (strcat "\n**Reactor for " ramsa$cmdStart " did not fire - Reactors are currently disabled. "))
    )
  )
  (setq ramsa$cmdStart nil) ; prevent multiple firings
  (princ)
);ramsa_cmdEnd

(defun ramsa_cmdCancel (reactor cmdlist / obj name)
  (setq ramsa$cmdStart nil) ; prevent multiple firings
  (princ)
);ramsa_cmdCancel

(defun ramsa_objAppended (reactor obj / ent objname bname)
;   (princ "\nramsa_objAppended: ")(prin1 obj)(princ) ;**********************
  (if ramsa#reactors
    (cond 
      ((and 
          ramsa$blkname ; ramsa$blkname is set by ramsa_react_placeview 
          (setq
            ent (cadr obj) ; (#<VLA-OBJECT IAcadDatabase 000000003ed6bad8> <Entity name: 7ffff0ec430>)
            obj (vlax-ename->vla-object ent)
            objname (vla-get-ObjectName obj)
          )
          (= objname "AcDbBlockReference")
          (setq bname (vla-get-EffectiveName obj))
          (= bname ramsa$blkname)
        ); and
        (if kcs#debug (progn (princ "\nramsa_objAppended: ")(prin1 bname)))
        (setvar "clayer" ramsa$clayer) ; ramsa$clayer set by ramsa_cmdStart
        (setq ramsa$clayer nil ramsa$blkname nil)
      )
    );cond
  )
  (princ)
);ramsa_objAppended

(defun ramsa_sysvarChanged (obj data)
;   (princ "\n**ramsa_sysvarChanged: data: ")(prin1 data)(princ);********
 
  (if (= (car data) "INSUNITS")(progn
    (if ramsa#reactors
      (progn 
        (cond
          ((member (getvar "insunits") '(1 2 3 8 9 10)) (setvar "measurement" 0)) ; Imperial
          ((member (getvar "insunits") '(4 5 6 7 11 12 13 14 15 16 17)) (setvar "measurement" 1)) ; Metric
          ; no change for 0, 18, 19, 20
        ) 
        (princ "\nMEASUREMENT set to ")(prin1 (getvar "measurement")) 
        (princ (if (= (getvar "measurement") 0) " (Imperial) " " (Metric) "))
      )
      (princ "\n**ramsa_sysvarChanged (for \"INSUNITS\") did not run - Reactors are currently disabled. ")
    )
  ))      
  (princ)
);ramsa_sysvarChanged

;============================================================================|
; Define secondary functions (the functions called by the reactor functions)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Function to control layers when placing Sheet Set Manager views
; Also moves callout down a bit
; Revised 2/2/10 [KCS]
(defun ramsa_react_placeview (/ doc ps cnt vport callout vp_lay disp);
  (if kcs#debug (princ "\nramsa_react_placeview  "))(princ)
  (if ramsa#reactors
    (progn  
      (setq 
        Doc (vla-get-ActiveDocument (vlax-get-acad-object))
        ps (vla-get-paperspace doc) ; Paper space
        cnt (1- (vla-get-count ps)) ; number of items in Paper space
        callout (vla-item ps cnt)
        ramsa$blkname (vla-get-name callout) ; for ramsa_objAppended
        vport (vla-item ps (1- cnt))
        vp_lay (aecgeneratelayerkey "ViewPort") ; layer for vport
      )
      (vla-put-layer vport vp_lay)
    
      ; Move the callout downward
      (if (kcs_ismetric)
        (setq disp '(0 15.0 0))   ; MM displacement
        (setq disp '(0 0.625 0))  ; Inches displacement
      )
      (vla-move vport (kcs_point->vpoint '(0 0 0)) (kcs_point->vpoint disp))
    )
    (princ "\n**ramsa_react_placeview did not run - Reactors are currently disabled. ")
  )
; NOTE: SSM seems to retain ownership of the callout past this point, so that
;       put-layer is not effective here. Changing the current layer from the 
;       desired layer back to the previous layer at this point is also
;       non-effective - it must remain the current layer past this point to be
;       the layer the callout ends up on. 
;       Examining the callout handle tells us that the callout is actually
;       deleted and replaced after this point.
;       Solution: set callout layer current at ramsa_cmdStart, then restore
;       previous layer at ramsa_objAppended
  (princ)
);ramsa_react_placeview

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Function to change xref just attached to its own layer [4/23/12 KCS]
(defun ramsa_react_xattach (/ obj name doc layers)
  (if kcs#debug (princ "\n\n**ramsa_react_xattach \n"))
  (if (not (kcs_ispspace))
    ; Happens in Model tab or inside a viewport
    (if ramsa#reactors
      (progn
        (setq 
          obj (vlax-ename->vla-object (entlast))
          name (vla-get-name obj)
          doc (vla-get-ActiveDocument (vlax-get-acad-object))
          layers (vla-get-layers doc) ; layers collection object
        )
        (vla-add layers name) ; create the layer
        (vla-put-layer obj name) ; change xref to this layer
      )
      (princ "\n**ramsa_react_xattach did not run - Reactors are currently disabled. ")
    )
    ; Do nothing if pspace
  )
  (princ)
);ramsa_react_xattach

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


(princ)
