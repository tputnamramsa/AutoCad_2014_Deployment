; Ramsa_Annotative_Dims.lsp
; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

; Revised (Version 2): 4/8/10
; Revised 2/14/12: c:PD_Con added [KCS]
; Revised 3/7/12: match style added to c:PD_CON [KCS]

; Note: kcs_ functions used in this file are defined in kcs_base.lsp

;============================================================================|
; Text style data
; NOTE: Font must be the programmatic name, as seen in -STYLE command
(setq ramsa#styledata '(
 ; FAMILY   TEXT_FONT       WIDTH     DIMTEXT_FONT    WIDTH
  ("Sans"   "swiss.ttf"     1.0       "swissc.ttf"    1.0)
  ("Serif"  "times.ttf"     1.0       "times.ttf"     1.0)
  ("Hand"   "Archquik.shx"  1.0       "Archquik.shx"  0.8)
))

;============================================================================|
; Set all the variables for the parent dim style.
(defun ramsa_SetDimVars (styname) 
  ; This section is for values that are (or might be)
  ; different for Metric vs. Imperial
  (if (kcs_ismetric)
    ; Metric values
    (progn
      (setvar "DIMADEC" 2)          ;Angular decimal places
      (setvar "DIMALT" 0)           ;Alternate units selected
      (setvar "DIMALTD" 2)          ;Alternate unit decimal places
      (setvar "DIMALTF" 0.03937)    ;Alternate unit scale factor
      (setvar "DIMALTRND" 0)        ;Alternate units rounding value
      (setvar "DIMALTTD" 2)         ;Alternate tolerance decimal places
      (setvar "DIMALTTZ" 0)         ;Alternate tolerance zero suppression
      (setvar "DIMALTU" 2)          ;Alternate units
      (setvar "DIMALTZ" 0)          ;Alternate unit zero suppression
      (setvar "DIMAPOST" "")        ;Prefix and suffix for alternate text
;       (setvar "DIMASZ" 2.25)        ;Arrow size
      (setvar "DIMASZ" 2.40)        ;Arrow size
      (setvar "DIMAUNIT" 0)         ;Angular unit format
      (setvar "DIMAZIN" 2)          ;Angular zero supression
      (setvar "DIMCEN" 1.5)         ;Center mark size
      (setvar "DIMLUNIT" 2)         ;Linear unit format
      (setvar "DIMDLE" 3.0)         ;Dimension line extension
      (setvar "DIMDLI" 9.0)         ;Dimension line spacing
      (setvar "DIMDSEP" ".")        ;Decimal separator
      (setvar "DIMEXE" 3.0)         ;Extension above dimension line
      (setvar "DIMEXO" 3.0)         ;Extension line origin offset
      (setvar "DIMFXL" 3.0)         ;Fixed Extension Line
      (setvar "DIMGAP" 1.5)         ;Gap from dimension line to text
      (setvar "DIMPOST" "")         ;Prefix and suffix for dimension text
      (setvar "DIMRND" 0.0000)      ;Rounding value
;       (setvar "DIMTXT" 2.25)        ;Text height
      (setvar "DIMTXT" 2.40)        ;Text height
      ; Unit-depended (metric) vars:
      (cond
        ((= (getvar "insunits") 6)  ;Meters
          (setvar "DIMDEC" 4)       ;Decimal places
          (setvar "DIMZIN" 3)       ;Zero suppression
        )
        ((= (getvar "insunits") 5)  ;CM
          (setvar "DIMDEC" 2)       ;Decimal places
          (setvar "DIMZIN" 3)       ;Zero suppression
        )
        (T                          ;MM (or oddball other)
;           (setvar "DIMDEC" 1)       ;Decimal places
          (setvar "DIMDEC" 0)       ;Decimal places
          (setvar "DIMZIN" 3)        ;Zero suppression
        )
      )
    )
    ; Imperial
    (progn
      (setvar "DIMADEC" 2)          ;Angular decimal places
      (setvar "DIMALT" 0)           ;Alternate units selected
      (setvar "DIMALTD" 2)          ;Alternate unit decimal places
      (setvar "DIMALTF" 25.40000)   ;Alternate unit scale factor
      (setvar "DIMALTRND" 0)        ;Alternate units rounding value
      (setvar "DIMALTTD" 2)         ;Alternate tolerance decimal places
      (setvar "DIMALTTZ" 0)         ;Alternate tolerance zero suppression
      (setvar "DIMALTU" 2)          ;Alternate units
      (setvar "DIMALTZ" 0)          ;Alternate unit zero suppression
      (setvar "DIMAPOST" "")        ;Prefix and suffix for alternate text
      (setvar "DIMASZ" 0.09375)     ;Arrow size
      (setvar "DIMAUNIT" 0)         ;Angular unit format
      (setvar "DIMAZIN" 2)          ;Angular zero supression
      (setvar "DIMCEN" 0.0625)      ;Center mark size
      (setvar "DIMDLE" 0.125)       ;Dimension line extension
      (setvar "DIMDLI" 0.375)       ;Dimension line spacing
      ;(setvar "DIMDSEP" ".")        ;Decimal separator [** INVALID FOR ARCH UNITS **]
      (setvar "DIMEXE" 0.125)       ;Extension above dimension line
      (setvar "DIMEXO" 0.125)       ;Extension line origin offset
      (setvar "DIMFXL" 0.125)       ;Fixed Extension Line
      (setvar "DIMGAP" 0.0625)      ;Gap from dimension line to text
      (setvar "DIMPOST" "")         ;Prefix and suffix for dimension text
      (setvar "DIMRND" 0.0000)      ;Rounding value
      (setvar "DIMTXT" 0.09375)     ;Text height
      ; Unit-depended (Imperial) vars:
      (if (= (getvar "insunits") 2) ; Feet
        (progn
          (setvar "DIMLUNIT" 2)         ;Linear unit format (decimal)
          (setvar "DIMDEC" 4)           ;Decimal places
          (setvar "DIMZIN" 3)           ;Zero suppression
          (setvar "DIMDSEP" ".")        ;Decimal separator [moved here 4/8/10]
        )
        ; Inches
        (progn
          (setvar "DIMLUNIT" 4)         ;Linear unit format (architectural)
          (setvar "DIMDEC" 4)           ;Decimal places
          (setvar "DIMZIN" 3)           ;Zero suppression
        )
      )
    )
  );Metric vs. Imperial values

  ; These variables apply to both Metric and Imperial
  (setvar "DIMARCSYM" 0)          ;Arc length symbol
  (setvar "DIMATFIT" 3)           ;Arrow and text fit
  (setvar "DIMBLK" ".")           ;Arrow block name
  (setvar "DIMBLK1" "ArchTick")   ;First arrow block name
  (setvar "DIMBLK2" "ArchTick")   ;Second arrow block name
  (setvar "DIMCLRD" 256)          ;Dimension line and leader color 256=bylayer, 0=byblock
  (setvar "DIMCLRE" 256)          ;Extension line color
  (setvar "DIMCLRT" 3)            ;Dimension text color
  (setvar "DIMFRAC" 0)            ;Fraction format
  (setvar "DIMFXLON" 0)           ;Off    Enable Fixed Extension Line
  (setvar "DIMJOGANG" 0.7854)     ;Radius dimension jog angle - W2 Converted value to radians 2/13/12
; (command "DIMJOGANG" 45)        ;Radius dimension jog angle
  (setvar "DIMJUST" 0)            ;Justification of text on dimension line
  (setvar "DIMLDRBLK" ".")        ;Leader block name
  (setvar "DIMLFAC" 1.00000)      ;Linear unit scale factor
  (setvar "DIMLIM" 0)             ;Off    Generate dimension limits
  (setvar "DIMLTEX1" "BYLAYER")   ;Linetype extension line 1
  (setvar "DIMLTEX2" "BYLAYER")   ;Linetype extension line 2
  (setvar "DIMLTYPE" "BYLAYER")   ;Dimension linetype
  (setvar "DIMLWD" -2)            ;Dimension line and leader lineweight
  (setvar "DIMLWE" -2)            ;Extension line lineweight
  (setvar "DIMSAH" 1)             ;On    Separate arrow blocks
  (setvar "DIMSCALE" 0)           ;Overall scale factor
  (setvar "DIMSD1" 0)             ;Off    Suppress the first dimension line
  (setvar "DIMSD2" 0)             ;Off    Suppress the second dimension line
  (setvar "DIMSE1" 0)             ;Off    Suppress the first extension line
  (setvar "DIMSE2" 0)             ;Off    Suppress the second extension line
  (setvar "DIMSOXD" 0)            ;Off    Suppress outside dimension lines
  (setvar "DIMTAD" 1)             ;Place text above the dimension line
  (setvar "DIMTDEC" 4)            ;Tolerance decimal places
  (setvar "DIMTFAC" 0.75000)      ;Tolerance text height scaling factor
  (setvar "DIMTFILL" 0)           ;Text background enabled
  (setvar "DIMTFILLCLR" 256)      ;Text background color
  (setvar "DIMTIH" 0)             ;Off    Text inside extensions is horizontal
  (setvar "DIMTIX" 0)             ;Off    Place text inside extensions
  (setvar "DIMTM" 0)              ;Minus tolerance
  (setvar "DIMTMOVE" 2)           ;Text movement
  (setvar "DIMTOFL" 1)            ;On    Force line inside extension lines
  (setvar "DIMTOH" 0)             ;Off    Text outside horizontal
  (setvar "DIMTOL" 0)             ;Off    Tolerance dimensioning
  (setvar "DIMTOLJ" 1)            ;Tolerance vertical justification
  (setvar "DIMTP" 0)              ;Plus tolerance
  (setvar "DIMTSZ" 0)             ;Tick size
  (setvar "DIMTVP" 0.00000)       ;Text vertical position
  (setvar "DIMTXSTY" styname)     ;Text style
  (setvar "DIMTZIN" 0)            ;Tolerance zero suppression
  (setvar "DIMUPT" 0)             ;Off    User positioned text
);ramsa_SetDimVars (parent settings)


(defun ramsa_SetAngularVars ()
  (if (kcs_ismetric)
    (setvar "DIMASZ" 3.75)
    (setvar "DIMASZ" 0.15625)
  )
  (setvar "DIMBLK1" ".")
  (setvar "DIMBLK2" ".")
  (setvar "DIMATFIT" 2)
  (setvar "DIMTMOVE" 0)
  (setvar "DIMSAH" 0)
  (setvar "DIMTIH" 1)
  (setvar "DIMTOH" 1)
)

(defun ramsa_SetDiameterVars ()
  (if (kcs_ismetric)
    (setvar "DIMASZ" 3.75)
    (setvar "DIMASZ" 0.15625)
  )
  (setvar "DIMBLK1" ".")
  (setvar "DIMBLK2" ".")
  (setvar "DIMCEN" 0)
  (setvar "DIMTOFL" 0)            ;Off
  (setvar "DIMATFIT" 2)
  (setvar "DIMTMOVE" 0)
  (setvar "DIMSAH" 0)
  (setvar "DIMTOH" 1)
  (setvar "DIMTAD" 0)
                    ;(setvar "DIMTIX" 0)            ;Off
                    ;(setvar "DIMSOXD" 0)            ;Off
)

(defun ramsa_SetRadialVars ()
  (if (kcs_ismetric)
    (setvar "DIMASZ" 3.75)
    (setvar "DIMASZ" 0.15625)
  )
  (setvar "DIMBLK2" ".")
  (setvar "DIMTOFL" 0)            ;Off
                    ;(setvar "DIMSOXD" 0)            ;Off
                    ;(setvar "DIMTIX" 0)            ;Off
  (setvar "DIMTIH" 1)
  (setvar "DIMTOH" 1)
  (setvar "DIMTAD" 0)
)

(defun ramsa_SetOrdinateVars ()
  (setvar "DIMTAD" 0)
                    ;(setvar "DIMATFIT" 3)
                    ;(setvar "DIMSOXD" 0)            ;Off
                    ;(setvar "DIMTIX" 0)            ;Off
                    ;(setvar "DIMTMOVE" 2)
                    ;(setvar "DIMTOFL" 1)            ;On
)

(defun ramsa_SetLeaderVars ()
  (if (kcs_ismetric)
    (progn
      (setvar "DIMASZ" 3.75)
      (setvar "DIMTXT" 3.0)
                    ;(setvar "DIMGAP" 1.5)
    )
    (progn
      (setvar "DIMASZ" 0.15625)
      (setvar "DIMTXT" 0.125)
                    ;(setvar "DIMGAP" 0.0625)
    )
  )
  (setvar "DIMTAD" 0)
                    ;(setvar "DIMTXTSTY" "SANS_Inch")
)

;============================================================================|
(defun ramsa_dims_layer (/ lay)
  (setq lay (aecgeneratelayerkey "dimline")) ; "A-Anno-Dims" (or other if LKO in effect)
  (if (/= (getvar "clayer") lay)
    (setvar "clayer" lay)
  )
)

;============================================================================|
; ARG: stybase: dimstyle base name ("SansDim", etc.)
;               or text style name ("Sans", etc.)
(defun ramsa_dimstyle_name (stybase)
  (cond
    ((= (getvar "insunits") 6) ; Meters
      (strcat stybase "_M")
    )
    ((= (getvar "insunits") 5) ; CM
      (strcat stybase "_CM")
    )
    ((= (getvar "insunits") 4) ; MM
      (strcat stybase "_MM")
    )
    ((= (getvar "insunits") 2) ; Feet
      (strcat stybase "_Foot")
    )
    (T ; Inches (1) (or other oddball)
      (strcat stybase "_Inch")
    )
  ); returns full style name ("SansDim_Inch", etc.)
);ramsa_dimstyle_name

;============================================================================|

; Create Parent style
; Also creates dimension text style with same name as dim style (ex: "SansDim_Inch")
; and main text style with corresponding name (ex: "Sans_Inch")
(defun ramsa_dimstyle (prefix / dimstybase dimstyname txtstyname stydat 
                                txtfont txtwid fimfont dimwid)
  (setq 
    dimstybase (strcat prefix "Dim")
    dimstyname (ramsa_dimstyle_name dimstybase); dim AND dimtext ("SansDim_Inch")
    txtstyname (ramsa_dimstyle_name prefix); main text style ("Sans_Inch")
    stydat (kcs_dxf prefix ramsa#styledata) ; ("swiss.ttf" 1.0 "swissc.ttf" 1.0)
    txtfont (nth 0 stydat)
    txtwid  (nth 1 stydat)
    dimfont (nth 2 stydat)
    dimwid  (nth 3 stydat)
  )

  ; Set or create dimstyle
  (if (tblsearch "dimstyle" dimstyname)
    ; if dim style already exists, make it current
    (command ".-dimstyle" "r" dimstyname)
    ; else create it
    (progn
      ; create text style for dimensions if it does not exist
      (if (null (tblsearch "style" dimstyname))(progn
        (command ".-style" dimstyname dimfont "A" "Y" "N" "0" dimwid "0" "N" "N")
        (if (> (getvar "cmdactive") 0)
          (command "") ; for "Vertical? [Yes/No] <No>:" 
        )
      ))
      (ramsa_SetDimVars dimstyname)  ; sets all dimension variables for parent dimensions style
      (command ".-dimstyle" "AN" "Y" dimstyname "s" dimstyname "Y")
      (princ "\nDim style ")(prin1 dimstyname)(princ " created. ")
    )
  )
  ; Set or create main text style
  (ramsa_textstyle txtstyname txtfont txtwid); function defined in Ramsa_Annotative_Text.lsp
); ramsa_dimstyle


; Create child style
(defun ramsa_childstyle (suffix / prefix stybase styname child)
  (setq prefix (ramsa_style_prefix))
  (if prefix (progn ; not canceled
    (setq 
      stybase  (strcat prefix "Dim")
      styname (ramsa_dimstyle_name stybase) ; "SansDim_Inch"
      child (strcat styname suffix) ; "SansDim_Inch$2"
    )
    (if (null (tblsearch "dimstyle" child))(progn
      (ramsa_dimstyle prefix) ; creates annotative dimension style and sets it current
      ; Parent exists and current. Now create angle child
      (cond
        ((= suffix "$2")(ramsa_SetAngularVars))
        ((= suffix "$3")(ramsa_SetDiameterVars))
        ((= suffix "$4")(ramsa_SetRadialVars))
        ((= suffix "$6")(ramsa_SetOrdinateVars))
        ((= suffix "$7")(ramsa_SetLeaderVars))
      )
      (command "-dimstyle" "s" child)
      (command "-dimstyle" "r" styname)
    ))
  ))
);ramsa_childstyle

;============================================================================|
;(defun c:k ()(load "O:\\Reference\\AutoCAD\\2010\\Support\\Plugins\\Ramsa_Annotative_Dims.lsp")(ramsa_style_prefix))
(defun ramsa_style_prefix (/ cstyle prefix dcl_id done)
  (setq cstyle (getvar "textstyle"))
  (cond
    ((wcmatch (strcase cstyle) "SANS*")(setq prefix "Sans"))
    ((wcmatch (strcase cstyle) "SERIF*")(setq prefix "Serif"))
    ((wcmatch (strcase cstyle) "HAND*")(setq prefix "Hand"))
    (t 
      ; Try to find a something already used to use as default
      (or (ssget "x" (list '(0 . "TEXT,MTEXT")(cons 7 (strcat (setq prefix "Sans") "*"))))
          (ssget "x" (list '(0 . "TEXT,MTEXT")(cons 7 (strcat (setq prefix "Serif") "*"))))
          (ssget "x" (list '(0 . "TEXT,MTEXT")(cons 7 (strcat (setq prefix "Hand") "*"))))
          ; standard default if none already in use
          (setq prefix "Sans")
      )
      ; Display dialog for choice
      (setq dcl_id (ramsa_dlg_init "style_prefix" nil)) ; defined in ramsadoc.lsp
      (set_tile "prefix" prefix)
      (action_tile "prefix" "(setq prefix $value)")
      (setq done (start_dialog))
      (unload_dialog dcl_id)
      (if (= done 0) ; canceled
        (setq prefix nil)
      )
    )
  )
  prefix
); ramsa_style_prefix

;============================================================================|
; Main dimension function - called by command functions below
(defun ramsa_dimcmd (cmd)
  (kcs_pre)
  (kcs_pushvar "clayer,texteval")
  (ramsa_Dims_Layer)    ; creates layer using layer key style if it doesn't exist, or makes it current
  (setvar "texteval" 1) ; needed for qleader
  (setvar "cmdecho" 1)
  (command cmd)
  (while (> (getvar "cmdactive") 0)  ; pause routine while placing dimension 
    (command pause)
  )
  (kcs_post)
);ramsa_dimcmd
 
;============================================================================|
;(defun c:k ()(load (strcat rsa#plugins "Ramsa_Annotative_Dims.lsp"))(c:pd_lin))
(defun c:PD_Lin () ; Place Dimension (Linear)
  (setq prefix (ramsa_style_prefix))
  (if prefix (progn ; not canceled
    (ramsa_dimstyle prefix)  ; creates annotative dimension style and sets it current
    (ramsa_dimcmd "_Dimlinear")
  ))
  (princ)
)

(defun c:PD_Ali () ; Place Dimension (Aligned)
  (setq prefix (ramsa_style_prefix))
  (if prefix (progn ; not canceled
    (ramsa_dimstyle prefix)  ; creates annotative dimension style and sets it current
    (ramsa_dimcmd "_DimAligned")
  ))
  (princ)
)

(defun c:PD_Arc () ; Place Dimension (Arc)
  (setq prefix (ramsa_style_prefix))
  (if prefix (progn ; not canceled
    (ramsa_dimstyle prefix)  ; creates annotative dimension style and sets it current
    (ramsa_dimcmd "_DimArc")
  ))
  (princ)
)

(defun c:PD_Jog () ; Place Dimension (Jogged)
  (ramsa_childstyle "$4")    ; creates child 
  ; [PROBLEM: DimJogged does not use this or any other child!]
  (ramsa_dimcmd "DimJogged")  
)

(defun c:PD_Ang ()
  (ramsa_childstyle "$2")    ; creates child 
  (ramsa_dimcmd "Dimangular") 
)

(defun c:PD_Dia ()
  (ramsa_childstyle "$3")    ; creates child 
  (ramsa_dimcmd "Dimdiameter")
)

(defun c:PD_Rad ()
  (ramsa_childstyle "$4")    ; creates child 
  (ramsa_dimcmd "Dimradius")  
)

(defun c:PD_Ord ()
  (ramsa_childstyle "$6")    ; creates child 
  (ramsa_dimcmd "Dimordinate")
)

(defun c:PD_Lea ()
  (ramsa_childstyle "$7")    ; creates child 
  (ramsa_dimcmd "qleader")    
)

(defun c:PD_Con (/ pick elist dimsty0 dimsty1 lay dummy entprev)
  (kcs_pre)
  (kcs_pushvar "clayer")
  (setq pick (kcs_entsel 
    "\nSelect continued Dimension: " "DIMENSION" 
    "\nLinear, Ordinate, or Angular Associative Dimension Required. "
  ))
  (if pick (progn ; nil if Enter
    (setq 
      elist (entget (car pick))
      lay (kcs_dxf 8 elist)
      dimsty0 (getvar "dimstyle")
      dimsty1 (kcs_dxf 3 elist)
    )
    (command "-dimstyle" "R" dimsty1)
    (kcs_pusheval '(command "-dimstyle" "R" dimsty0))
    (if (/= (getvar "clayer") lay)(setvar "clayer" lay))
    ; This routine works consistently only if done after creating a dimension,
    ; so create a dummy one now, to be deleted at the end:
    (command "_dimlinear" "0,0" "1,0" "0.5,0.5")
    (setq dummy (entlast))
    (vla-put-visible (vlax-ename->vla-object dummy) :vlax-false) ; make it invisible
    (kcs_pusheval '(entdel dummy)) ; runs at kcs_post (or cancel/error)
    (command "_DimContinue" "" (cadr pick)) ; just pass it the point
    (while (> (getvar "cmdactive") 0)  ; pause routine while placing dimension(s)
      (princ "\nSpecify a second extension line origin or [Undo]: ") ; no Select option
      (setq entprev (entlast))
      (command pause) ; create dim, unless Enter
      ; Recognize when user hits Enter, to end the command
      (if (eq entprev (entlast)) ; user hit Enter (no new dim created),
        (command "")            ; so end the command
      )
    )
  ))
  (kcs_post)
);c:PD_Con