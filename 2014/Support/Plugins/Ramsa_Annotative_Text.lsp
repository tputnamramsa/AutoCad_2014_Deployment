; Ramsa_Annotative_Text.lsp
; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

; Revised (Version 2): 4/8/10
; 6/15/10: (initcommandversion 2);added for mtext context-ribbon

; Note: kcs_ functions used in this file are defined in kcs_base.lsp

;============================================================================|
; Command function - Serif text
; Determines stylename and size according to current units
(defun c:SERIF-TEXT ()
  (ramsa_text "Serif")
)

; Command function - Sans-Serif text
; Determines stylename and size according to current units
(defun c:SANS-TEXT ()
  (ramsa_text "Sans")
)

; Command function - Hand-font text
; Determines stylename and size according to current units
(defun c:HAND-TEXT ()
  (ramsa_text "Hand")
)

;============================================================================|
; Set or create style and layer, then create text
; ARG: prefix: "Serif", "Sans", or "Hand"
(defun ramsa_text (prefix / styname font width)
  (kcs_pre) ; error-trapping, environment control, cmdecho=0, etc.
  ; get data from global var set in Ramsa_Annotative_Dims.lsp
  (setq 
    data (kcs_dxf prefix ramsa#styledata) ; ex: ("times.ttf" 1.0 "times.ttf" 1.0)
    font (car data) 
    width (cadr data)
    styname (ramsa_dimstyle_name prefix); "Serif_Inch"
  )
  ; Check for alternate system font name
  (cond
    ((= font "times.ttf")
      (if (not (findfile "c:\\windows\\fonts\\times.ttf"))
        (setq font "times_0.ttf")
      )
    )
  )
  (ramsa_textstyle styname font width) ; set or make text style
  ;(ramsa_SetTxtAnnotative styname); make text style annotative if it isn't

  (kcs_pushvar "clayer") ; push to stack to pop later
  (ramsa_text_layer); creates layer using layer key style if it doesn't exist, or makes it current
  (initcommandversion 2); mtext command displays context-ribbon [6/15/10]
  (initdia); runs mtext command in dialog box
  (command "_mtext"); mtext command
  (princ "\nSpecify first corner: ")
  (setvar "cmdecho" 1) ; to see mtext command prompts
  (while (= (getvar "cmdactive") 1); while statement pauses routine while placing mtext 
    (command pause)
  )
  (kcs_post) ; restores pushed vars (and "cmdecho"), etc.
);ramsa_text

;============================================================================|
(defun ramsa_text_layer (/ lay)
  (setq lay (aecgeneratelayerkey "annobj"))
  (if (/= (getvar "clayer") lay)
    (setvar "clayer" lay)
  )
)

;============================================================================|
; Set (or create) text style 
(defun ramsa_textstyle (styname font width / size)
  ;(princ (strcat "\nramsa_textstyle: " styname ", " font ", "))(princ width)
  (if (tblsearch "style" styname)
    ; style exists - make it current
    (setvar "textstyle" styname)
    ; style does not exist - make it (also sets it current)
    (progn
      (princ "\nText style ")(prin1 styname)
      ; Determine size according to current units
      (cond
        ((= (getvar "insunits") 6) ; meters
          ;(setq size 0.25)
          (setq size 0.0024)
        )
        ((= (getvar "insunits") 5) ; CM
          ;(setq size 2.5)
          (setq size 0.24)
        )
        ((= (getvar "insunits") 4) ; MM
          ;(setq size 2.25)
          (setq size 2.4)
        )
        ((= (getvar "insunits") 2) ; feet
          (setq size 0.09375)
        )
        (t ; inches [insunits=1 or any oddball case]
          (setq size 0.09375)
        )
      )

      (command ".-style" styname font "A" "Y" "N" size width "0" "N" "N")
      (if (> (getvar "cmdactive") 0)
        (command "") ; for "Vertical? [Yes/No] <No>:" 
      )
      (ramsa_SetTxtAnnotative styname); make text style annotative if it isn't
      (princ " created. \n")
    )
  )
); ramsa_textstyle

;============================================================================|
(defun ramsa_SetTxtAnnotative (Name / *text* *textstl* obj app xd1 xd2 rt1 rt2)
  (vl-load-com)
  (setq  *text* (vla-get-textstyles
                  (vla-get-activedocument (vlax-get-acad-object))
                )
  ) ; #<VLA-OBJECT IAcadTextStyles 1c382384>

  (vlax-for itm  *text*
    (setq *textstl* (cons (vla-get-name itm) *textstl*))
  ) ; ("Annotative" "RomanS" "Arch-Dim" "Standard")

  ;(if (member Name *textstl*) ; KJK )
  (if (member (strcase Name) (mapcar 'strcase *textstl*)) ; KJK
    (progn
      (setq 
        obj  (vla-item *text* Name)
        app  "AcadAnnotative"
      )
      (regapp app)
      (setq xd1 (vlax-make-safearray vlax-vbInteger '(0 . 5))) ; #<safearray...>
      (vlax-safearray-fill
        xd1
        (list 1001 1000 1002 1070 1070 1002)
      )
      (setq xd2 (vlax-make-safearray vlax-vbVariant '(0 . 5)))
      (vlax-safearray-fill
        xd2
        (list "AcadAnnotative"
              "AnnotativeData"
              "{"
              (vlax-make-variant 1 vlax-vbInteger)
              (vlax-make-variant 1 vlax-vbInteger)
              "}"
        ); ("AcadAnnotative" "AnnotativeData" "{" #<variant 2 1> #<variant 2 1> "}")
      )
      (vla-setxdata obj xd1 xd2)
      (vla-getxdata obj app 'rt1 'rt2)
      (mapcar
        (function (lambda (x y)(cons x y)))
        (vlax-safearray->list rt1)
        (mapcar 'vlax-variant-value (vlax-safearray->list rt2))
      )
      ;((1001 . "AcadAnnotative") (1000 . "AnnotativeData") (1002 . "{") (1070 . 1) 
      ;(1070 . 1) (1002 . "}"))
    )
  )
);ramsa_SetTxtAnnotative

;============================================================================|

(prompt "\n RAMSATEXT20120227")(princ)
