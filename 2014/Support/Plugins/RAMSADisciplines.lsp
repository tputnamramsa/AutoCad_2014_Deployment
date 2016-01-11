;;set layer key override for disciplines
;;limited to the three in house disciplines at RAMSA
;;first draft 2006-09-11
;;Charles Prettyman

;;;;;;;;;
;;function to report current dicipline in modemacro box
;(defun feedback()
;	(setq curdisc
;		(IF 
;			(= (getvar "cprofile") "Ramsa6")
;			":-)"
;			":-("
;		)
;	(command "modemacro" "$(curdisc)")
;	)
;)
;;;;;;;;;;
;;reactor to check status when drawing is opened
(vl-load-com)
(setq MyReactor1 
	(vlr-dwg-reactor nil
	   '(
		(:vlr-dwgFileOpened . feedback)
		)
	)
)


;;function to set layer key overrride for discipline field manually
(Defun c:Setdisc ()
    (if (not (member "aeclayermanagerui47.arx" (arx)))
        (arxload "aeclayermanagerui47.arx"))
    (if (not (member "AECLMGRLISP47.arx" (arx)))
        (arxload "AECLMGRLISP47.arx"))
  
(Initget "Architecture Interiors Landscape")
(setq response (getkword "What is your discipline (Architecture, Interiors, Landscape)? ")
)
(if (= response "Interiors")
  	(setq Disc "I")
  	(if (= response "Landscape")
	  	(setq Disc "L")
	  	(setq Disc "")
	 )
)

  (AECSETLAYERKEYOVERRIDE "Discipline" Disc)
;  feedback
(princ)
)
