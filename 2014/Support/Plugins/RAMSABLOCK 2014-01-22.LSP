; RAMSABLOCK.lsp
; Insert annotation symbol (for Imperial or Metric) on correct layer.
; Symbol block is extracted from source drawing if not already present.

; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

; Updated for AutoCAD 2014 - W2

; Note: kcs_ functions used in this file are defined in kcs_base.lsp

;============================================================================|
;(defun c:k()(load (strcat rsa#plugins "RAMSABLOCK"))(setq ramsa%block '("Door Tag" "doorno"))(c:RAMSABLOCK))
; Global var: ramsa%block - a list of block name and layer key 
; set in menu call prior to the command
; Ex: (setq ramsa%block '("Detail Tag" "AnnoIden")) RAMSABLOCK
(defun c:RAMSABLOCK (/ source blkname layer)

  ; **********  SET VALUES AS DESIRED HERE ***********************************
  (if (kcs_ismetric)
    (setq ; Metric (MM)
;       source "O:\\Reference\\AutoCAD\\2010\\Support\\RAMSA Standards 2010 MM Sans v2.dwg"
;       source "O:\\Reference\\AutoCAD\\2012\\Support\\RAMSA Standards 2012 MM Sans.dwg"
      source "\\\\ramsacfs\Project\\Reference\\AutoCAD\\2014\\Support\\RAMSA Standards MM Sans.dwg"
    )
    (setq ; Imperial 
;       source "O:\\Reference\\AutoCAD\\2010\\Support\\RAMSA Standards 2010 I Sans v2.dwg"
;       source "O:\\Reference\\AutoCAD\\2012\\Support\\RAMSA Standards 2012 I Sans.dwg"
      source "\\\\ramsacfs\\Project\\Reference\\AutoCAD\\2014\\Support\\RAMSA Standards I Sans.dwg"
    )
  )
  ; End of CAD manager variables
  ;***************************************************************************
  (kcs_pre)
  (setq 
    blkname (car ramsa%block)
    layer (aecgeneratelayerkey (cadr ramsa%block))
  )
  (if (null (tblsearch "block" blkname))
    (ramsa_extract_block blkname source)
  )  
  (kcs_pushvar "clayer")  
  (setvar "clayer" layer)
  (princ (strcat "\nPlace \"" blkname "\"... "))  
  (command "._-insert" blkname "_Scale" 1 "_Rot" 0) ; block is annotative (no dimscaling needed)  
  (while (> (getvar "cmdactive") 0) (command pause))
  (kcs_post)
);c:RAMSABLOCK 
  
;============================================================================|
; Extract a block from another drawing
(defun ramsa_extract_block (blkname source / tempdwg acadapp blocks dbxdoc)
  ; Make a temp copy of source file to avoid failure if source is open
  (setq tempdwg (vl-filename-mktemp nil nil ".dwg"))
  (if (null (vl-file-copy source tempdwg))(progn 
    (alert (strcat "ERROR: \n"
      "Could not access source drawing \n"
      "\"" source "\"\n"
      "to extract block \"" blkname "\""
    ))
    (exit)
  ))
  (setq 
    acadapp (vlax-get-acad-object)
    blocks (vla-get-blocks (vla-get-ActiveDocument acadapp)) ; block defs in this dwg
;      dbxdoc (vla-GetInterfaceObject acadapp "ObjectDBX.AxDbDocument.18") ; 18 = R2010-2012
    dbxdoc (vla-GetInterfaceObject acadapp "ObjectDBX.AxDbDocument.19") ; 19 = R2013-2015
  )
  (vla-open dbxDoc tempdwg)
  (vla-CopyObjects 
    dbxDoc
    (vlax-safearray-fill
       (vlax-make-safearray vlax-vbObject '(0 . 0))
       (list (vla-item (vla-get-blocks dbxDoc) blkname))
    )
    blocks
  )
  (vlax-release-object dbxDoc)
  (vl-file-delete tempdwg)
  (if (null (tblsearch "block" blkname))(progn
    (alert (strcat "ERROR: \nBlock " blkname " was not extracted from source drawing: \n" source))
    (exit)
  ))
  (vla-item blocks blkname)
);ramsa_extract_block

;============================================================================|
