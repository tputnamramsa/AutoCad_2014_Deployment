; PageSetups.lsp
; by Ken Krupa, Krupa CADD Solutions
; Copyright© 2010 Robert A.M. Stern Architects LLP

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Variables (CAD Manager settings) - alter to suit
; Be sure PageSetup names are actually in the specified template
(setq
;   rsa#PageSetupTemplate "o:\\reference\\autocad\\2010\\support\\Page Setups.dwt"
  rsa#PageSetupTemplate "o:\\reference\\autocad\\2012\\support\\Page Setups.dwt"
  rsa#PageSetupModel    "FULL SIZE PR 30X42 MS" ;"None MS"
  rsa#PageSetupPaper    "FULL SIZE PR 30X42"    ;"None"
)
; end of CAD Manager settings

;============================================================================|
; MAIN FUNCTION - Delete all PageSetups and import from template
(defun c:ramsa_PageSetup_Mod ()
  (ramsa_DeleteAllPageSetups)

  ; Import PageSetups 
  (princ "\nImporting PageSetups from template... \n")(princ)
  (command "._-PSETUPIN" rsa#PageSetupTemplate "*")
  (princ "\r                            ") ; eliminates "Press ENTER to continue: "

  ; Now assign PageSetup to PS layouts
  (princ "\nAssigning PageSetups to layouts...")(princ)
  (foreach tab (layoutlist)
    (command "._-plot" "_n" tab rsa#PageSetupPaper "" "" "_y" "_n")
  )
  ; Model tab
  (command "._-plot" "_n" "Model" rsa#PageSetupModel "" "" "_y" "_n")

  (princ "done. ")
  (princ)
);c:ramsa_PageSetup_Mod

;============================================================================|
(defun ramsa_DeleteAllPageSetups (/ doc)
  (princ "\nPurging existing PageSetups... ")
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vlax-for pltcfg (vla-get-plotconfigurations doc)
    (vla-delete pltcfg)
  )
);ramsa_DeleteAllPageSetups
