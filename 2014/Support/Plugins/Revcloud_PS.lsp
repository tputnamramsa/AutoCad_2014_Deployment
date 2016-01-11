; Revcloud_PS.lsp
; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2012 Robert A.M. Stern Architects LLP

; Created 2/27/12 [KCS]
; Update for AutoCAD 2014 [W2]

; Note: kcs_ functions used in this file are defined in kcs_base.lsp

;(defun c:k()(load "O:\\Reference\\AutoCAD\\2012\\Support\\Plugins\\revcloud_PS.lsp")(c:RCPS))
(defun c:RCPS ()(ramsa_revcloud))
(defun ramsa_revcloud (/  style cloudlay cloudlayclr taglay taglayclr tagname minarc maxarc 
                          width tagsource revno alist)
  (princ "\nCreate a Revision Cloud in paper space... ")
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ; Variables for CAD Manager
  (setq
    style "Normal"
    cloudlay "A-Anno-Revs" ; "-nn" (revno) will be added later
    cloudlayclr 8
    taglay "A-Anno-Revs-Iden"
    taglayclr 1
    tagname "Revision Tag"  
  )
  (if (kcs_ismetric)
    (setq
      minarc 8
      maxarc 19
      width 0.4
;       tagsource "O:\\Reference\\AutoCAD\\2012\\Support\\RAMSA Standards 2012 MM Sans.dwg"
      tagsource "\\\\ramsacfs\\Project\\Reference\\AutoCAD\\2014\\Support\\RAMSA Standards MM Sans.dwg"
    )
    (setq
      minarc 0.3125
      maxarc 0.75
      width 0.015625
;       tagsource "O:\\Reference\\AutoCAD\\2012\\Support\\RAMSA Standards 2012 I Sans.dwg"
      tagsource "\\\\ramsacfs\\Project\\Reference\\AutoCAD\\2014\\Support\\RAMSA Standards I Sans.dwg"
    )
  ) 
  ; End of Variables for CAD Manager   
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  (if (not (kcs_ispspace))(progn
    (alert "This command is used in Paper Space only! ")
    (exit)
  ))
  (kcs_pre)
  (initget 1)
  (setq 
    revno (getint "\nRevision number: ")
    revno (itoa revno)
    cloudlay (strcat cloudlay "-" revno)
  )
  (kcs_pushvar "clayer,attreq,osmode")
  
  ; Draw cloud
  (cond
    ((= (getvar "clayer") cloudlay)()) ; already current somehow - do nothing
    ((tblsearch "layer" cloudlay)(setvar "clayer" cloudlay)) ; exists - set it current
    (t (command "._-layer" "_M" cloudlay "_C" cloudlayclr "" ""))
  )
  (command "_REVCLOUD" "_A" minarc maxarc "_S" style)
  (setvar "cmdecho" 1)
  (princ "\nSpecify start point or <Object>: ")
  (while (> (getvar "cmdactive") 0) (command pause))
  (setvar "cmdecho" 0)
  (vla-put-ConstantWidth (vlax-ename->vla-object (entlast)) width)
  (kcs_pusheval '(entdel (entlast))) ; in case of cancel at tag
  
  ; Place Revision Tag
  (cond
    ((tblsearch "layer" taglay)(setvar "clayer" taglay)) ; exists - set it current
    (t (command "._-layer" "_M" taglay "_C" taglayclr "" ""))
  )  
  (if (null (tblsearch "block" tagname))(progn
    (if (null ramsa_extract_block)
      (load (strcat rsa#plugins "RAMSABLOCK"))
    )
    (ramsa_extract_block tagname tagsource)
  ))
  (setvar "attreq" 0) ; no attrib dlg or prompting
  (setvar "osmode" 512) ; nearest
  (princ "\nPlace Revision Tag (Hint: CTRL key will cycle through corners for placement)... ") 
  (command "._-insert" tagname pause) ; block is annotative (no dimscaling needed)
  (while (> (getvar "cmdactive") 0) (command "")) 
  (kcs_popeval '(entdel (entlast)) nil) ; cancel this push
  
  ; Update the attribute
  (setq
    alist (entget (entnext (entlast))) ; attrib data list
    alist (subst (cons 1 revno) (assoc 1 alist) alist)
  )
  (entmod alist)
  (kcs_post)
)
  
  
  
  