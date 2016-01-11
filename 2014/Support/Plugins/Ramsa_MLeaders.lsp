; Ramsa_MLeaders.lsp
; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

; Created: 4/8/10
; Revised 2/28/12 for R2012: [KCS]
;   ramsa_mod_mlstyle added [KCS]

; Note: kcs_ functions used in this file are defined in kcs_base.lsp

;============================================================================|
;(defun c:k ()(load "O:\\Reference\\AutoCAD\\2012\\Support\\Plugins\\ramsa_mleaders.lsp")(c:pml))

; Command: Place MultiLeader
(defun c:PML (/ prefix styname data font width) 
  (setq 
    prefix (ramsa_style_prefix)
    styname (ramsa_dimstyle_name prefix)
  )
  (if (not (member styname (ramsa_mlnames)))(progn
  ; Create new if not exist
    ; get data from global var set in Ramsa_Annotative_Dims.lsp
    (setq 
      data (kcs_dxf prefix ramsa#styledata) ; ("times.ttf" 1.0 "times.ttf" 1.0)
      font (car data)
      width (cadr data)
      styname (ramsa_dimstyle_name prefix); "Serif_Inch"
    ) 
    (ramsa_textstyle styname font width) ; set or make text style
    (ramsa_make_mlstyle styname)
  ))
  (ramsa_mod_mlstyle styname) ; update “Always left justify”, if not already done


  (kcs_pre)
;   (command "_.CmLeaderStyle" styname) ; Set style current (and leave it current)
  (setvar "CMLEADERSTYLE" styname) ; Set style current (and leave it current)
  (kcs_pushvar "clayer") ; push to stack to pop later
  (ramsa_text_layer); creates layer using layer key style if it doesn't exist, or makes it current
  (setvar "cmdecho" 1) ; for all prompts
  (initcommandversion 2)
  (command "_.mleader")
  (while (> (getvar "cmdactive") 0)
    (command pause)
  )
  (kcs_post) ; restores cmdecho, etc.
)

(defun ramsa_mlnames (/ names)
  (foreach item (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE") 
    (if (= (car item) 3) 
      (setq names (cons (cdr item) names))
    )
  )
  names
)

;============================================================================|
; Create mleaderstyle
(defun ramsa_make_mlstyle (styname / acadver *acad* *doc* mldict mlstyle 
                            ArrowSize BreakSize DoglegLength TextHeight )

  ; Version code number needed for getinterfaceobject 
  (setq acadver (atof (getvar "acadver")))
  (if (>= acadver 18.0) ; "2010"
    (setq acadver (rtos (atof (getvar "acadver")) 2 0)) ; "18" for R2011-2012 [2/15/12]
    (setq acadver (rtos (atof (getvar "acadver")) 2 1)) ; "18.1" for R2008, etc.
  ) ; known good from R2008 (17.1) through R2012 (18.1)
  
  (setq 
    *acad* (vlax-get-acad-object)
    *doc* (vla-get-activedocument *acad*)
    mldict (vla-item (vla-get-dictionaries *doc*) "ACAD_MLEADERSTYLE")
    mlstyle (vlax-invoke mldict 'addobject styname "AcDbMLeaderStyle")
    colorObj (vla-getinterfaceobject *acad* ; for LeaderLineColor
        (strcat "AutoCAD.AcCmColor." acadver)) ; "18" for R2010-2012
  )

  (if (kcs_ismetric)
    (setq
      ArrowSize 3.0   ;rev. from 5.0 3-5-13
      BreakSize 1.5
      DoglegLength 5.0
      TextHeight 2.40 ;rev. from 2.25 6-19-14
      LandingGap 1.0
    )
    (setq
      ArrowSize 0.125    ;rev. from 0.1875 3-5-13
      BreakSize 0.0625
      DoglegLength 0.1875
      TextHeight 0.09375
      LandingGap 0.0625  ;added 6-19-14
    )
  )

  (vlax-put-property mlstyle 'AlignSpace 0.18)
  (vlax-put-property mlstyle 'Annotative -1)
  (vlax-put-property mlstyle 'ArrowSize ArrowSize)
;   ArrowSymbol = ""
  (vlax-put-property mlstyle 'BitFlags 0)
;   Block = ""
  (vlax-put-property mlstyle 'BlockConnectionType 0)
  (vlax-put-property mlstyle 'BlockRotation 0.0)
  (vlax-put-property mlstyle 'BlockScale 1.0)
  (vlax-put-property mlstyle 'BreakSize BreakSize)
  (vlax-put-property mlstyle 'ContentType 2)
  (vlax-put-property mlstyle 'Description "RAMSA created style") ; ? shows up nowhere to user!
  (vlax-put-property mlstyle 'DoglegLength DoglegLength)
  (vlax-put-property mlstyle 'DrawLeaderOrderType 0)
  (vlax-put-property mlstyle 'DrawMLeaderOrderType 1)
  (vlax-put-property mlstyle 'EnableBlockRotation -1)
  (vlax-put-property mlstyle 'EnableBlockScale -1)
  (vlax-put-property mlstyle 'EnableDogleg -1)
  (vlax-put-property mlstyle 'EnableFrameText 0)
  (vlax-put-property mlstyle 'EnableLanding -1)
  (vlax-put-property mlstyle 'FirstSegmentAngleConstraint 0)
  (vlax-put-property mlstyle 'LandingGap LandingGap) ;rev. from 0.0625 6-19-14

  (vla-put-ColorIndex colorObj 1) ; for LeaderLineColor (Red)
  (vla-put-LeaderLineColor mlstyle colorObj) 

  (vlax-put-property mlstyle 'LeaderLineType 1)
  (vlax-put-property mlstyle 'LeaderLineTypeId "ByLayer")
  (vlax-put-property mlstyle 'LeaderLineWeight -1)
  (vlax-put-property mlstyle 'MaxLeaderSegmentsPoints 2)
  (vlax-put-property mlstyle 'name styname)
  (vlax-put-property mlstyle 'ScaleFactor 1.0)
  (vlax-put-property mlstyle 'SecondSegmentAngleConstraint 0)
  (vlax-put-property mlstyle 'TextAlignmentType 0)
  (vlax-put-property mlstyle 'TextAngleType 1)
;   TextAttachmentDirection = 0
;   TextBottomAttachmentType = 0
  (vlax-put-property mlstyle 'TextHeight TextHeight)
  (vlax-put-property mlstyle 'TextLeftAttachmentType 1)
  (vlax-put-property mlstyle 'TextRightAttachmentType 5)
  (vlax-put-property mlstyle 'TextString "")
  (vlax-put-property mlstyle 'TextStyle styname)
;   TextTopAttachmentType = 0
);ramsa_make_mlstyle

;============================================================================
; Update style, if not already done
; This is used both for existing and new styles
(defun ramsa_mod_mlstyle (styname / dict data)
  (if (= (kcs_getxrecord "RAMSA" (strcat "MLS_" styname)) "")(progn
    (setq 
;       dic (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE")
      dict (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE")))
      data (dictsearch dict styname)
      data (subst (cons 297 1) (assoc 297 data) data) ; 1=“Always left justify”
    )
    (entmod data)
    (entupd dict)
    ; Plant "done" flag in drawing for this style
    (kcs_setxrecord "RAMSA" (strcat "MLS_" styname) "1") 
  ))
);ramsa_mod_mlstyle


;============================================================================

