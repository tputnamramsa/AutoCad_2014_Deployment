;;(setq check (getvar "ACADVER"))
;;(if (/= check "15.0")
;;    (progn
;;       (princ "\nIncompatible Version of AutoCAD")
;;       (princ "\nThis routine can only be used with")
;;       (princ "\nAutoCAD Release 15.0; aka AutoCAD 2000")
;;       (princ)
;;       (quit)))

;;Define Bush Command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:bush ( / bshmode)
     (initget 1 "New Convert")     
     (setq bshmode (getkword "\nDo you wish to create a \"New\" shrubline or \"Convert\" an existing line?
                               \n<\"New\" or \"Convert\">  "))
     (if (= bshmode "New")
         (bush)
         (bushit)
     )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   BUSH   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;'BUSH CREATES A NEW SHRUB LINE WHEN USER DRAWS A POLYLINE OUTLINE

(defun bush ( /  olderror bush-settings old-name-check old-name layer-check choice frz-stat
                 old-bshscale old-bshbulge 1poly fpd70 closechk num1 num2 pairtotl pairlist
                 start-pt end-pt vtxbulge se-ang incr-pair-list new90)            
     (setq olderror *error* *error* bush-error)
     (setq bush-settings 
          '("cmdecho" "ucsfollow" "orthomode" "clayer" "ucsicon"))
     (bush-mode-save bush-settings)
     (mapcar 'setvar '("cmdecho" "ucsfollow" "ucsicon") 
         '(0 0 0))
;;CHECK TO SEE IF THERE IS ALREADY A VALUE ATTACHED TO 'BLAYNAME' AND
;;THEN PROMPT FOR A LAYER NAME
     (setq old-name-check (boundp 'blayname))
     (if (= old-name-check T)
         (progn
             (setq old-name (strcase blayname))
             (setq blayname (getstring (strcat "\nName of layer <" old-name ">?  ")))
             (if (or (= blayname nil) (= blayname ""))
                 (setq blayname old-name)))
         (while (or (= blayname nil)(= blayname ""))
                (setq blayname (getstring "\n Name of layer? "))))
;;CHECK TO SEE IF CHOSEN LAYER NAME ALREADY EXISTS OR NOT
     (setq layer-check (tblsearch "LAYER" blayname))
     (setq choice nil)
;;IF LAYER NAME DOES NOT EXIST, ASK IF USER WANTS TO CREATE A LAYER BY THAT NAME
     (if (= layer-check nil)
         (progn
            (initget 1 "Yes No")
            (setq choice (getkword "\nLayer does not exist!! Do you wish to create it? <Yes or No>  "))
            (if (= choice "Yes")
               (command "-LAYER" "New" blayname "Set" blayname "")
            )
         )
     ) 
;;IF USER DOES NOT WANT TO CREATE SUCH A LAYER, ASK FOR ANOTHER NAME
     (while (= choice "No")
         (setq blayname (getstring "\nPick another layer name.  "))
         (setq layer-check (tblsearch "LAYER" blayname))
         (if (= layer-check nil)
             (progn
                (initget 1 "Yes No")
                (setq choice (getkword "\nLayer does not exist!! Do you wish to create it? <Yes or No>  "))
                (if (= choice "Yes")
                   (command "-LAYER" "New" blayname "Set" blayname "")
                )
             )
             (progn
                (setq choice "Yes")
                (command "-LAYER" "Thaw" blayname "Set" blayname "")
             )
          )
     )
;;IF LAYER DOES EXIST, MAKE SURE IT IS NOT FROZEN - THAW IT IF NECESSARY         
     (if (/= layer-check nil)
         (progn         
           (setq frz-stat (cdr (assoc 70 layer-check)))
           (if (= (boole 1 frz-stat 1) 1)
               (command "-LAYER" "thaw" blayname "Set" blayname ""))))
;;CHECK TO SEE IF AN OLD DEFAULT BSHSCALE EXISTS AND PROMPT FOR INPUT ACCORDINGLY
     (if (= (boundp 'bshscale) T)
         (progn
          (setq old-bshscale bshscale)
          (initget 6 "")
          (setq bshscale (getreal (strcat "\nWhat scale?: <" (rtos old-bshscale 2 2) ">  ")))))
     (if (or (= bshscale "")(= bshscale nil))
         (setq bshscale old-bshscale))
     (if (= bshscale 0.0)
         (progn
            (initget 7)
            (setq bshscale (getreal "\nZero is not an option!!!\nWhat scale do you want? "))
         )
     )
     (if (= (boundp 'bshscale) nil)
         (progn
          (initget 7)
          (setq bshscale (getreal "\nWhat scale?:     "))))
;;CHECK TO SEE IF AN OLD DEFAULT BSHBULGE EXISTS AND PROMPT FOR INPUT ACCORDINGLY
     (if (= (boundp 'bshbulge) T)
         (setq bshbulge (abs bshbulge))
     )
     (if (= (boundp 'bshbulge) T)
         (progn
          (setq old-bshbulge bshbulge)
          (initget 6 "")
          (setq bshbulge (getreal 
                      (strcat "\nSelect a real number between 1 and 0 to determine
                               \nthe amount of wiggliness of shrub line <" 
                                 (rtos old-bshbulge 2 2) ">  ")))))
     (if (or (= bshbulge "")(= bshbulge nil))
         (setq bshbulge old-bshbulge))
     (if (= (boundp 'bshbulge) nil)
         (progn
          (initget 6)
          (setq bshbulge (getreal "\nSelect a real number between 1 and 0 to determine
                                \nthe amount of wiggliness of shrub line. <0.75> "))))
     (if (or (= bshbulge "")(= bshbulge nil))
          (setq bshbulge 0.75)
     )
     (while (>= bshbulge 1)
          (initget 7)
          (setq bshbulge (getreal "\nReal number MUST be between 1 and 0 to determine
                                \nthe amount of wiggliness of shrub line.")
          )
     )
;;USER STARTS BY DRAWING A POLYLINE
     (prompt "\nDraw shrub outline.")
     (setq 1poly (bpline))
     (setq fp-data (entget 1poly))
     (setq fpd70 (cdr (assoc 70 fp-data)))
     (setq closechk (logand fpd70 1))
     (if (= closechk 0)
         (progn
           (setq new70 (cons '70 1))
           (setq fp-data (subst new70 (assoc 70 fp-data) fp-data))
          )
     )
     (command "_UCS" "W")
     (setq 1poly (entlast))
     (mapcar 'setvar '("osmode" "orthomode") 
         '(0 0))
;CREATE THE LIST "PAIRLIST" USING "VTXPAIRLIST" FUNCTION   - SEE BELOW
     (vtxpairlist 1poly)
;CREATE THE LIST OF DATA FOR NEW ENTITY "FP-DATA" USING "SHRBDATA" FUNCTION   - SEE BELOW
     (shrbdata blayname) 
     (setq num1 0)
     (setq num2 1)
     (setq pairtotl (length pairlist))
     (while (<= num1 (- pairtotl 2))
            (setq start-pt (cdar (nth num1 pairlist)))
            (setq end-pt (cdar (nth num2 pairlist)))
            (setq vtxbulge (cdr (assoc 42 (nth num1 pairlist))))
            (setq se-ang (angle start-pt end-pt))
            (if (= vtxbulge 0.0)
                (shrubpts start-pt end-pt)
                (shrubarcpts start-pt end-pt vtxbulge)
            )
            (setq fp-data (append fp-data incr-pair-list))
            (setq num1 (+ num1 1))
            (setq num2 (+ num2 1))
     )
;;COMPLETE THE LIST OF DATA
     (setq new90 (cons '90 (/ (- (length fp-data) 12) 4)))
     (setq fp-data (subst new90 (assoc 90 fp-data) fp-data))
     (entmod fp-data)
     (entupd 1poly)
     (command "_UCS" "P")
     (bush-mode-restore)
     (setq *error* olderror)
     (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   BPLINE   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALLOWS USER TO DRAW A POLYLINE WHILE IN THE 'BUSH' COMMAND

(defun bpline ()
     (setvar "cmdecho" 1)
     (command "pline")
     (while (= (getvar "CMDNAMES") "PLINE")
            (command pause)
     )
     (setq 1poly (entlast))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  VTXPAIRLIST  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;THIS FUNCTION LIST VERTEX POINTS IN A LWPOLYLINE.     CREATES "PAIRLIST"
;;IT IS SIMILAR TO 'ENTPAIRS' HOWEVER IT INCLUDES THE LAST VERTEX OF A CLOSED
;;LWPOLYLINE.  USED FOR CLOSED LWPOLYLINES

(defun vtxpairlist (ent / entl count entl-leng item item-no temp10 temp42 temppair)
     (setq pairlist '())
     (setq entl (entget ent))
     (setq count 0)
     (setq entl-leng (length entl))
     (while (< count entl-leng)
            (setq item (nth count entl))
            (setq item-no (car item))
            (if (= item-no 10) 
                (setq temp10 item)
            )
            (if (= item-no 42)
                (progn
                  (setq temp42 item)
                  (setq temppair (list temp10 temp42))
                  (setq pairlist (cons temppair pairlist))
                )
            )
            (setq count (+ count 1))
     )
     (setq temppair (list (assoc 10 temppair) (cons '42 0.0)))
     (setq pairlist (subst temppair (car pairlist) pairlist))
     (setq temppair (last pairlist))
     (setq pairlist (cons temppair pairlist))
     (setq pairlist (reverse pairlist))
     (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SHRBDATA  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;THIS FUNCTION CREATES THE BEGINNING OF DATA LIST.     CREATES "FP-DATA"
;;

(defun shrbdata (blayname / blaycode)
     (setq blaycode (cons 8 blayname))
     (setq fp-data (list (nth 0 fp-data)
                         (nth 1 fp-data)
                         (nth 2 fp-data)
                         (nth 3 fp-data)
                         (nth 4 fp-data)
                         (nth 5 fp-data)
                         (nth 6 fp-data)
                         (nth 7 fp-data)
                         (nth 8 fp-data)
                         (nth 9 fp-data)
                         (nth 10 fp-data)
                         (nth 11 fp-data)
                         (nth 12 fp-data)
                         (nth 13 fp-data)
                    )
     )
     (setq fp-data (subst blaycode (assoc 8 fp-data) fp-data))
)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SHRUBPTS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;THIS FUNCTION LOCATES AND LISTS SHRUB POINTS BETWEEN .   CREATES "INCR-PAIR-LIST"
;;



(defun shrubpts (1pt 2pt / incr-list incr-pt vx-ang dist listpart listseg
                           num incr-total incr start-pt end-pt incr-prop)
     (setq incr-pair-list '())
     (setq incr-prop '(1.0 0.5 0.85 1.75 0.25 0.4 0.75 1.5 0.75 0.5 1.25 2.0 0.7))
     (setq incr-list (mapcar '(lambda (x)
                              (* x bshscale))
                               incr-prop))
     (setq incr-pt 1pt)
     (setq incr10 (list (cons '10 1pt)))
     (setq bshbulge (* bshbulge -1.0))
     (setq bshblg (cons '42 bshbulge))
     (setq listseg (list bshblg '(41 . 0.0) '(40 . 0.0)))
     (setq listpart (append listseg incr10))
     (setq incr-pair-list (append listpart incr-pair-list))
     (setq vx-ang (angle 1pt 2pt))
     (setq dist (distance 1pt 2pt))
     (setq num 0)
     (setq incr-total 0)
     (setq bshbulge (* bshbulge -1.0))
     (while (< incr-total (- dist (nth 11 incr-list)))
            (setq incr (nth num incr-list))
            (setq incr-pt (polar incr-pt vx-ang incr))
            (setq incr10 (list (cons '10 incr-pt)))
            (setq bshblg (cons '42 bshbulge))
            (setq listseg (list bshblg '(41 . 0.0) '(40 . 0.0)))
            (setq listpart (append listseg incr10))
            (setq incr-pair-list (append listpart incr-pair-list))
            (setq incr-total (+ incr-total incr))
            (setq bshbulge (* bshbulge -1.0))
            (setq num (+ num 1))
            (if (= num 13)
                (setq num 0)
            )
     )
     (setq incr-pair-list (reverse incr-pair-list))
     (princ))


;;  SHRUBARCPTS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;THIS FUNCTION CREATES THE BEGINNING OF DATA LIST.   CREATES "INCR-PAIR-LIST"
;;

(defun shrubarcpts (1pt 2pt bbulge / incr-prop incr-list 
                                     hincr-pt listseg incr-total num temp42 temp10 listpart
                                     alang1 negcheck gprcenter alradius alang1 alcircum 
                                     alarclen newang subpt gbbulge gspnewang gspsubpt2)
     (setq incr-pair-list '())
     (setq incr-prop '(1.0 0.5 0.85 1.75 0.25 0.4 0.75 1.5 0.75 0.5 1.25 2.0 0.7))
     (setq incr-list (mapcar '(lambda (x)
                              (* x bshscale))
                               incr-prop))
     (setq incr-pt 1pt)
     (setq incr10 (list (cons '10 1pt)))
     (setq bshbulge (* bshbulge -1.0))
     (setq bshblg (cons '42 bshbulge))
     (setq listseg (list bshblg '(41 . 0.0) '(40 . 0.0)))
     (setq listpart (append listseg incr10))
     (setq incr-pair-list (append listpart incr-pair-list))
     (plarccen 1pt 2pt bbulge)
     (arclength 1pt 2pt gprcenter)
     (setq num 0)
     (setq incr-total (nth num incr-list))
     (while (< incr-total (- alarclen (nth 11 incr-list)))
            (getarcpt incr-total)
            (setq incr10 (list (cons '10 subpt)))
            (setq bshbulge (* bshbulge -1.0))
            (setq bshblg (cons '42 bshbulge))
            (setq listseg (list bshblg '(41 . 0.0) '(40 . 0.0)))
            (setq listpart (append listseg incr10))
            (setq incr-pair-list (append listpart incr-pair-list))
            (setq num (+ num 1))
            (if (= num 13)
                (setq num 0)
            )
            (setq incr-total (+ incr-total (nth num incr-list)))
     )
     (setq incr-pair-list (reverse incr-pair-list))
     (princ)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CURVE ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;"PLARCCEN" FINDS THE CEN OF AN ARC IN A LWPOLYLINE
;;
;;RETURNS 'NEGCHECK' WHICH INDICATES THE DIRECTION OF THE 'bbulge' ARGUMENT
;;AND 'GPRCENTER'  WHICH IS THE CENTERPOINT OF THAT BULGE.

(defun plarccen (firstpt secondpt bulge / arcdist distang gprang1 ang1abs
                 ang2abs halfdist ang2cos radius radang)
     (setq negcheck nil)
     (if (< bulge 0.0)
         (setq negcheck 1)
         (setq negcheck 0)
     )
     (setq arcdist (distance firstpt secondpt))
     (setq distang (angle firstpt secondpt))
     (setq gprang1 (* 2.0 (atan bulge)))
     (setq ang1abs (abs gprang1))
     (setq ang2abs (- (/ pi 2.0) ang1abs))
     (setq halfdist (/ arcdist 2.0))
     (setq ang2cos (cos ang2abs))
     (setq radius (/ halfdist ang2cos))
     (if (< bulge 0.0)
         (setq radang (- distang ang2abs))
         (setq radang (+ distang ang2abs))
     )
     (setq gprcenter (polar firstpt radang radius))
)

;;"ARCLENGTH" FINDS THE CEN OF AN ARC IN A LWPOLYLINE
;;
;;RETURNS 'ALRADIUS' WHICH IS THE RADIUS OF THE ARC IN POLYLINE
;;        'ALARCLEN' WHICH IS THE LENGTH OF THE ARC.
;;        'ALANG1'   WHICH IS THE ANGLE FROM THE CENTERPOINT TO THE FIRSTPOINT.
;;        'ALCIRCUM' WHICH IS THE CIRCUMFERENCE OF THE CIRCLE THAT THE ARC WOULD
                           DESCRIBE WERE IT WHOLE




(defun arclength (firstpt secondpt center / alchord alang2 alang3 
                  alprop)
     (setq alchord (distance firstpt secondpt))
     (setq alradius (distance firstpt center))
     (if (= negcheck 1)
         (progn
            (setq alang1 (angle center firstpt))
            (setq alang2 (angle center secondpt))
            (setq alang3 (- alang1 alang2))
         )
         (progn
            (setq alang1 (angle center firstpt))
            (setq alang2 (angle center secondpt))
            (setq alang3 (- alang2 alang1))
         )
     )
     (if (< alang3 0.0)
         (setq alang3 (- (* pi 2.0) (abs alang3)))
     )
     (setq alcircum (* (* pi 2.0) alradius))
     (setq alprop (/ alang3 (* pi 2.0)))
     (setq alarclen (* alcircum alprop)) 
)

;;"GETARCPT" FINDS A POINT ON AN ARC THAT IS A SPECIFIC LENGTH OF ARC.
;;MUST BE RUN AFTER "ARCLENGTH".
;;THIS ROUTINE USES THE VARIALBLES "ALRADIUS"  FROM "ARCLENGTH"
;;                                 "NEGCHECK"  FROM "PLARCCEN"
;;                                 "ALANG1"    FROM "ARCLENGTH"
;;                                 "GPRCENTER" FROM "PLARCCEN"

;;RETURNS 'NEWANG'   WHICH IS THE ANGLE FROM CENTER TO NEXTPOINT
;;        'SUBPT'    WHICH IS THE NEXT POINT

(defun getarcpt (subarclen / subang)
     (setq subang (/ subarclen alradius))
     (if (= negcheck 1)
         (setq newang (- alang1 subang))
         (setq newang (+ alang1 subang))
     )
     (setq subpt (polar gprcenter newang alradius))
)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BUSHIT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;'BUSHIT' CREATES A SHRUB-LINE FROM A CHOSEN ENTITY
;;THE ENTITY MUST BE A 'POLYLINE', 'LWPOLYLINE', 'LINE', 'ARC' OR 'CIRCLE'


(defun bushit ( / old-error bush-settings accepted bshent old-multiple old-bshbulge bshdata
                  bshtype bshcheck msg pl70 splcheck ent210 bshinfo num1 num2 fp-data pairtotl
                  start-pt end-pt vtxbulge se-ang incr-pair-list final10 final42 new90)
     (setq olderror *error* *error* bush-error)
     (setq bush-settings 
          '("cmdecho" "osmode" "ucsfollow" "orthomode" "clayer" "ucsicon"))
     (bush-mode-save bush-settings)
     (mapcar 'setvar '("cmdecho" "ucsfollow" "ucsicon" "osmode" "orthomode") 
         '(0 0 0 0 0))
     (command "UCS" "W")
     (setq accepted '("LWPOLYLINE" "ARC" "CIRCLE" "LINE" "POLYLINE"))
     (prompt "\nSelect entity which like to turn into a bush line.  ")
     (setq bshent (car (entsel)))
     (while (= bshent nil)
            (prompt "\nNothing has been selected.  Try again.")
            (setq bshent (car (entsel)))
     )
;;CHECK TO SEE IF CHOSEN MULTIPLE OR SCALE ALREADY EXISTS  - "bshscale"
     (if (= (boundp 'bshscale) T)
         (progn
          (setq old-multiple bshscale)
          (initget 6 "")
          (setq bshscale (getreal (strcat "\nWhat scale?: <" (rtos old-multiple 2 2) ">  ")))))
     (if (or (= bshscale"")(= bshscale nil))
         (setq bshscale old-multiple))
     (if (= bshscale 0.0)
         (progn
          (initget 7)
          (setq bshscale (getreal "\nZero is not an option!!!\nWhat scale do you really want?  "))
         )
     )
     (if (= (boundp 'bshscale) nil)
         (progn
          (initget 7)
          (setq bshscale (getreal "\nWhat scale?:     "))
          )
     )
;;CHECK TO SEE IF AN OLD DEFAULT BSHBULGE EXISTS AND PROMPT FOR INPUT ACCORDINGLY
     (if (= (boundp 'bshbulge) T)
         (setq bshbulge (abs bshbulge))
     )
     (if (= (boundp 'bshbulge) T)
         (progn
          (setq old-bshbulge bshbulge)
          (initget 6 "")
          (setq bshbulge (getreal 
                      (strcat "\nSelect a real number between 1 and 0 to determine
                               \nthe amount of wiggliness of shrub line <" 
                                 (rtos old-bshbulge 2 2) ">  ")))))
     (if (or (= bshbulge "")(= bshbulge nil))
         (setq bshbulge old-bshbulge))
     (if (= (boundp 'bshbulge) nil)
         (progn
          (initget 6)
          (setq bshbulge (getreal "\nSelect a real number between 1 and 0 to determine
                                \nthe amount of wiggliness of shrub line. <0.75> "))))
     (if (or (= bshbulge "")(= bshbulge nil))
          (setq bshbulge 0.75)
     )
     (while (>= bshbulge 1)
          (initget 7)
          (setq bshbulge (getreal "\nReal number MUST be between 1 and 0 to determine
                                \nthe amount of wiggliness of shrub line.")
          )
     )
     (setq bshdata (entget bshent))
     (setq bshtype (cdr (assoc 0 bshdata)))
     (setq bshcheck (member bshtype accepted))
     (if (= bshcheck nil)
         (progn
            (setq msg (strcat "\nThe entity you selected is a \"" bshtype "\"."
                              "\nIt cannot be made into a shrubline." ))
            (princ msg)
            (exit)
         )
     )
     (if (= bshtype "POLYLINE")
         (progn
            (setq pl70 (cdr (assoc 70 bshdata)))
            (setq splcheck (logand pl70 4))
            (if (= splcheck 4)
                (progn
                   (setq msg "\nThe entity you selected is a \"SPLINED POLYLINE\".
                              \nIt cannot be made into a shrubline." )
                   (princ msg)
                   (exit)
                )
                (progn 
                   (command "CONVERT" "P" "S" bshent "")
                )
            )
         )
     )
     (setq ent210 (assoc 210 bshdata))
     (if (not (equal ent210 '(210 0.0 0.0 1.0)))
         (progn
            (setq msg "\nThis entity is not parallel to the WORLD COORDINATE SYSTEM.
                       \nIt cannot be turned into a shrubline.")
            (princ msg)
            (exit)
         )
     )
     (if (= bshtype "LWPOLYLINE")
         (progn
            (setq pl70 (cdr (assoc 70 bshdata)))
            (setq closechk (logand pl70 1))
            (if (= closechk 0)
                (setq bshtype "LWPOLYLINE0")
                (setq bshtype "LWPOLYLINE1")
            )

         )
     )
;;
;;CREATE A LIST OF BUSH POINTS FOR AN ARC THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= bshtype "ARC")
         (progn
            (arctolwp bshent)
            (setq bshent (entlast))
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN LINE THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= bshtype "LINE")
         (progn
            (lintolwp bshent)
            (setq bshent (entlast))
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN CIRCLE THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= bshtype "CIRCLE")
         (progn
            (cirtolwp bshent)
            (setq bshent (entlast))
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN OPEN LWPOLYLINE
     (if (= bshtype "LWPOLYLINE0")
         (progn
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR A CLOSED LWPOLYLINE
     (if (= bshtype "LWPOLYLINE1")
         (progn
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
          )
     )
     (shrubmod)
     (entupd bshent)
     (command "_UCS" "P")
     (bush-mode-restore)
     (setq *error* olderror)
     (princ)
)

;;'SHRUBMOD' MODIFIES A SIMPLE OUTLINE LWPOLYLINE INTO A SHRUB LINE

(defun shrubmod ( )
;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
     (if (= bshtype "LWPOLYLINE1")
         (vtxpairlist bshent)
         (entpairs bshent)
     )
     (setq fp-data '())
     (repeat 14 (setq fp-data (cons (car bshinfo) fp-data))
                (setq bshinfo (cdr bshinfo))
     )
     (setq fp-data (reverse fp-data))
     (setq num1 0)
     (setq num2 1)
     (setq pairtotl (length pairlist))
     (while (<= num1 (- pairtotl 2))
            (setq start-pt (cdar (nth num1 pairlist)))
            (setq end-pt (cdar (nth num2 pairlist)))
            (setq vtxbulge (cdr (assoc 42 (nth num1 pairlist))))
            (setq se-ang (angle start-pt end-pt))
            (if (= vtxbulge 0.0)
                (shrubpts start-pt end-pt)
                (shrubarcpts start-pt end-pt vtxbulge)
            )
            (setq fp-data (append fp-data incr-pair-list))
            (if (member bshtype '("ARC" "LINE" "LWPOLYLINE0"))
                (progn 
                   (setq final10 (list (cons '10 end-pt)))
                   (setq final42 (list '(40 . 0.0) '(41 . 0.0) '(42 . 0.0))) 
                   (setq fp-data (append fp-data final10 final42))
                )
            )
            (setq num1 (+ num1 1))
            (setq num2 (+ num2 1))
     )
     (setq new90 (cons '90 (/ (- (length fp-data) 12) 4)))
     (setq fp-data (subst new90 (assoc 90 fp-data) fp-data))
     (entmod fp-data)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;LWPOLYLINE CONVERSION FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;CONVERT LINE TO LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lintolwp (ent / lindata temp-1 temp0 temp330 temp5 temp100A temp67
                       temp410 temp8 temp100B temp90 temp70 temp43 temp38 
                       temp39 tempdata linent lindata lpt1 lpt2
                       linseg)
     (setq linent ent)
     (setq lindata (entget linent))
     (setq temp-1 (assoc -1 lindata)
           temp0 (cons '0 "LWPOLYLINE")
           temp330 (assoc 330 lindata)
           temp5 (assoc 5 lindata)
           temp100A (cons '100 "AcDbEntity")
           temp67 (assoc 67 lindata)
           temp410 (assoc 410 lindata)
           temp8 (assoc 8 lindata)
           temp100B (cons '100 "AcDbPolyline")
           temp90 (cons '90 '2)
           temp70 (cons '70 '0)
           temp43 (cons '43 '0.0)
           temp38 (cons '38 '0.0)
           temp39 (cons '39 '0.0)
     )
     (setq tempdata (list temp-1 temp0 temp330 temp5 temp100A
                          temp67 temp410 temp8 temp100B temp90
                          temp70 temp43 temp38 temp39))
     (setq lpt1 (list (assoc 10 lindata)))
     (setq lpt2 (assoc 11 lindata))
     (setq lpt2 (list (subst '10 (car lpt2) lpt2)))
     (setq linseg (list '(40 . 0.0) '(41 . 0.0) '(42 . 0.0)))
     (setq lpt1 (append lpt1 linseg))
     (setq tempdata (append tempdata lpt1))
     (setq lpt2 (append lpt2 linseg))
     (setq tempdata (append tempdata lpt2))
     (entmake tempdata)
     (entdel linent)
)

;;;;;;;;;;;;;;;;;;;;;;;;;CONVERT ARC TO LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun arctolwp (ent / arcdata temp-1 temp0 temp330 temp5 temp100A temp67
                       temp410 temp8 temp100B temp90 temp70 temp43 temp38 
                       temp39 tempdata arcrad arccen apt1 apt2 arcseg1 
                       arcseg2 arc10a arc10b arclist1 arclist2 tmpbulge
                       arcpts abulge arcang1 arcang2)
     (setq arcent ent)
     (setq arcdata (entget arcent))
     (setq temp-1 (assoc -1 arcdata)
           temp0 (cons '0 "LWPOLYLINE")
           temp330 (assoc 330 arcdata)
           temp5 (assoc 5 arcdata)
           temp100A (cons '100 "AcDbEntity")
           temp67 (assoc 67 arcdata)
           temp410 (assoc 410 arcdata)
           temp8 (assoc 8 arcdata)
           temp100B (cons '100 "AcDbPolyline")
           temp90 (cons '90 '3)
           temp70 (cons '70 '0)
           temp43 (cons '43 '0.0)
           temp38 (cons '38 '0.0)
           temp39 (cons '39 '0.0)
     )
     (setq tempdata (list temp-1 temp0 temp330 temp5 temp100A
                          temp410 temp67 temp8 temp100B temp90
                          temp70 temp43 temp38 temp39))
     (setq arcrad (cdr (assoc 40 arcdata)))
     (setq arccen (cdr (assoc 10 arcdata)))
     (setq arcang1 (cdr (assoc 50 arcdata)))
     (setq arcang2 (cdr (assoc 51 arcdata)))
     (setq apt1 (polar arccen arcang1 arcrad))
     (setq apt2 (polar arccen arcang2 arcrad))
     (if (<= (- arcang2 arcang1) pi)
         (progn
            (setq abulge (arcbulg1 apt1 apt2))
            (setq tmpbulge (cons '42 abulge))
            (setq arcseg (list tmpbulge '(41 . 0.0) '(40 . 0.0)))
            (setq arc10a (list (cons '10 apt1)))
            (setq arc10b (list (cons '10 apt2)))
            (setq arclist1 (append arcseg arc10a))
            (setq arclist2 (append arcseg arc10b))
            (setq arcpts (reverse (append arclist1 arclist2)))
         )
         (progn
            (setq abulge (arcbulg2 apt1 apt2))
            (setq tmpbulge (cons '42 abulge))
            (setq arcseg (list tmpbulge '(41 . 0.0) '(40 . 0.0)))
            (setq arc10a (list (cons '10 apt2)))
            (setq arc10b (list (cons '10 apt1)))
            (setq arclist1 (append arcseg arc10a))
            (setq arclist2 (append arcseg arc10b))
            (setq arcpts (reverse (append arclist1 arclist2)))
         )
     )
     (setq tempdata (append tempdata arcpts))
     (entmake tempdata)
     (entdel arcent)
)

(defun arcbulg1 (bpt1 bpt2 / achord halfcord amidvec adif)
     (setq achord (distance bpt1 bpt2))
     (setq halfcord (/ achord 2.0))
     (setq amidvec (sqrt (- (* arcrad arcrad) (* halfcord halfcord))))
     (setq adif (- arcrad amidvec))
     (setq abulge (* (/ adif halfcord) -1.0))
)

(defun arcbulg2 (bpt1 bpt2 / achord halfcord amidvec adif)
     (setq achord (distance bpt1 bpt2))
     (setq halfcord (/ achord 2.0))
     (setq amidvec (sqrt (- (* arcrad arcrad) (* halfcord halfcord))))
     (setq adif (+ arcrad amidvec))
     (setq abulge (/ adif halfcord))
)

;;;;;;;;;;;;;;;;;;;;;;;;;CONVERT CIRCLE TO LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cirtolwp (ent / cirdata temp-1 temp0 temp330 temp5 temp100A temp67
                       temp410 temp8 temp100B temp90 temp70 temp43 temp38 
                       temp39 tempdata cirrad circen cpt1 cpt2 cirseg1 
                       cirseg2 cir10a cir10b cirpair1 cirpair2 cirpair3
                       cirpts)
     (setq cirdata (entget ent))
     (setq temp-1 (assoc -1 cirdata)
           temp0 (cons '0 "LWPOLYLINE")
           temp330 (assoc 330 cirdata)
           temp5 (assoc 5 cirdata)
           temp100A (cons '100 "AcDbEntity")
           temp67 (assoc 67 cirdata)
           temp410 (assoc 410 cirdata)
           temp8 (assoc 8 cirdata)
           temp100B (cons '100 "AcDbPolyline")
           temp90 (cons '90 '3)
           temp70 (cons '70 '1)
           temp43 (cons '43 '0.0)
           temp38 (cons '38 '0.0)
           temp39 (cons '39 '0.0)
     )
     (setq tempdata (list temp-1 temp0 temp330 temp5 temp100A
                          temp67 temp410 temp8 temp100B temp90
                          temp70 temp43 temp38 temp39))
     (setq cirrad (cdr (assoc 40 cirdata)))
     (setq circen (cdr (assoc 10 cirdata)))
     (setq cpt1 (polar circen 0.0 cirrad))
     (setq cpt2 (polar circen pi cirrad))
     (setq cirseg1 (list '(42 . 1.0) '(41 . 0.0) '(40 . 0.0)))
     (setq cirseg2 (list '(42 . 0.0) '(41 . 0.0) '(40 . 0.0)))
     (setq cir10a (list (cons '10 cpt1)))
     (setq cir10b (list (cons '10 cpt2)))
     (setq cirpair1 (append cirseg1 cir10a))
     (setq cirpair2 (append cirseg1 cir10b))
     (setq cirpair3 (append cirseg2 cir10a))
     (setq cirpts (reverse (append cirpair3 cirpair2 cirpair1)))
     (setq tempdata (append tempdata cirpts))
     (entmake tempdata)
     (entdel ent)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ENTPAIRS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;THIS FUNCTION LIST VERTEX POINTS IN A LWPOLYLINE.     CREATES "PAIRLIST"

(defun entpairs (ent / entl count entl-leng item item-no temp10 temp42 temppair)
     (setq pairlist '())
     (setq entl (entget ent))
     (setq count 0)
     (setq entl-leng (length entl))
     (while (< count entl-leng)
            (setq item (nth count entl))
            (setq item-no (car item))
            (if (= item-no 10) 
                (setq temp10 item)
            )
            (if (= item-no 42)
                (progn
                  (setq temp42 item)
                  (setq temppair (list temp10 temp42))
                  (setq pairlist (cons temppair pairlist))
                )
            )
            (setq count (+ count 1))
     )
     (setq pairlist (reverse pairlist))
     (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BUSH ERROR HANDLING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bush-error (s)
     (if (/= s "Function cancelled")
         (princ (strcat "\nError: " s)))
     (bush-mode-restore)
     (setq *error* olderror)
     (princ))
     
(defun bush-mode-save (a)
     (setq bush-mode-list '())
     (repeat (length a)
             (setq bush-mode-list (append bush-mode-list 
                       (list (list (car a)(getvar (car a))))))
             (setq a (cdr a))))

(defun bush-mode-restore ()
     (repeat (length bush-mode-list)
             (setvar (caar bush-mode-list)(cadar bush-mode-list))
             (setq bush-mode-list (cdr bush-mode-list))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   C:BUSHID   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;IDENTIFIES AND SETS AS DEFAULT THE SCALE ("bshscale"), THE LAYER ("blayname") AND 
;;THE AMOUNT OF BULGE ("bshbulge") OF AN EXISTING SHRUB LINE.

(defun c:bushid ( / bent bentdata bentwhat bentlen bentclose bentbulge b10list 
                    b10cnt bent8 bent42 bentitem bushpt1 bushpt2 msg)
     (setq bent (car (entsel "\nSelect a shrub polyline. ")))
     (setq bentdata (entget bent))
     (setq bentwhat (cdr (assoc 0 bentdata)))
     (while (/= bentwhat "LWPOLYLINE")
            (prompt "\nThis is not a valid shrub polyline.
                     \nSelect another shrub polyline. ")
            (setq bent (car (entsel))) 
            (setq bentdata (entget bent))
            (setq bentwhat (cdr (assoc 0 bentdata)))
     )
     (setq bentlen (cdr (assoc 90 bentdata)))
     (while (< bentlen 16)
            (prompt "\nThis is either not a valid shrub polyline,
                     \nor is too short to give accurate information.")                  
            (setq bent (car (entsel "\nSelect another shrub polyline. ")))
            (setq bentdata (entget bent))
            (setq bentwhat (cdr (assoc 0 bentdata)))
            (while (/= bentwhat "LWPOLYLINE")
                   (setq bent (car (entsel "\nThis is not a valid shrub polyline.
                                            \nSelect another shrub polyline. ")))
                   (setq bentdata (entget bent))
                   (setq bentwhat (cdr (assoc 0 bentdata)))
            )
            (setq bentlen (cdr (assoc 90 bentdata)))
     )
     (setq bentclose (cdr (assoc 70 bentdata)))
     (setq bentbulge (abs (cdr (assoc 42 bentdata))))
     (if (or (>= bentbulge 1.0) (= bentbulge 0.0))
         (alert "\n                           WARNING!
                 \nThis does not appear to be a valid shrub polyline,
                 \n       or has been altered since it was created.
                 \n
                 \nResulting information may not be accurate.")
      )
     (setq b10list '())
     (setq b10cnt 0)
     (setq bent8 0)
     (setq bent42 0.0)
     (while (< b10cnt 2)
            (setq bentitem (car bentdata))
            (if (= (car bentitem) '10)
                (progn
                   (setq b10cnt (+ 1 b10cnt))
                   (setq b10list (cons (cdr bentitem) b10list))
                )
            )
            (if (= (car bentitem) '8)
                (setq bent8 (cdr bentitem))
            )
            (if (= (car bentitem) '42) 
                (setq bent42 (abs (cdr bentitem)))
            )
            (setq bentdata (cdr bentdata))
     )
     (setq b10list (reverse b10list))
     (setq bushpt1 (car b10list))
     (setq bushpt2 (cadr b10list))
     (setq bshscale (distance bushpt1 bushpt2))
     (setq bshbulge bent42)
     (setq bshbulge (abs bshbulge))
     (setq blayname bent8)
     (setq msg (strcat "\nThe shrubline you have selected is on layer \"" 
                          (strcase blayname) "\".
                        \nThe scale is " (rtos bshscale 2 2) ", and it has a "
                          (rtos bshbulge 2 2) " wiggle."))
     (princ msg)
     (princ)
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MBUSH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;'MBUSH' CREATES SHRUB-LINES FROM A CHOSEN SET OF ENTITIES
;;THE ENTITIES MUST BE A 'POLYLINE', 'LWPOLYLINE', 'LINE', 'ARC' OR 'CIRCLE'


(defun c:mbush ( / old-error bush-settings accepted bshent old-multiple old-bshbulge bshdata
                  bshtype bshcheck msg pl70 splcheck ent210 bshinfo num1 num2 fp-data pairtotl
                  start-pt end-pt vtxbulge se-ang incr-pair-list final10 final42 new90)
     (setq olderror *error* *error* bush-error)
     (setq bush-settings 
          '("cmdecho" "osmode" "ucsfollow" "orthomode" "clayer" "ucsicon"))
     (bush-mode-save bush-settings)
     (mapcar 'setvar '("cmdecho" "ucsfollow" "ucsicon" "osmode" "orthomode") 
         '(0 0 0 0 0))
     (command "UCS" "W")
     (setq accepted '("LWPOLYLINE" "ARC" "CIRCLE" "LINE" "POLYLINE"))
     (prompt "\nSelect lines you want to convert to shrublines")
     (setq bushset (ssget))
     (while (= bushset nil)
            (prompt "\nNothing has been selected.  Try again.")
            (setq bushset (ssget))
     )
;;CHECK TO SEE IF CHOSEN MULTIPLE OR SCALE ALREADY EXISTS  - "bshscale"
     (if (= (boundp 'bshscale) T)
         (progn
          (setq old-multiple bshscale)
          (initget 6 "")
          (setq bshscale (getreal (strcat "\nWhat scale?: <" (rtos old-multiple 2 2) ">  ")))))
     (if (or (= bshscale"")(= bshscale nil))
         (setq bshscale old-multiple))
     (if (= bshscale 0.0)
         (progn
          (initget 7)
          (setq bshscale (getreal "\nZero is not an option!!!\nWhat scale do you really want?  "))
         )
     )
     (if (= (boundp 'bshscale) nil)
         (progn
          (initget 7)
          (setq bshscale (getreal "\nWhat scale?:     "))
          )
     )
;;CHECK TO SEE IF AN OLD DEFAULT BSHBULGE EXISTS AND PROMPT FOR INPUT ACCORDINGLY
     (if (= (boundp 'bshbulge) T)
         (setq bshbulge (abs bshbulge))
     )
     (if (= (boundp 'bshbulge) T)
         (progn
          (setq old-bshbulge bshbulge)
          (initget 6 "")
          (setq bshbulge (getreal 
                      (strcat "\nSelect a real number between 1 and 0 to determine
                               \nthe amount of wiggliness of shrub line <" 
                                 (rtos old-bshbulge 2 2) ">  ")))))
     (if (or (= bshbulge "")(= bshbulge nil))
         (setq bshbulge old-bshbulge))
     (if (= (boundp 'bshbulge) nil)
         (progn
          (initget 6)
          (setq bshbulge (getreal "\nSelect a real number between 1 and 0 to determine
                                \nthe amount of wiggliness of shrub line. <0.75> "))))
     (if (or (= bshbulge "")(= bshbulge nil))
          (setq bshbulge 0.75)
     )
     (while (>= bshbulge 1)
          (initget 7)
          (setq bshbulge (getreal "\nReal number MUST be between 1 and 0 to determine
                                \nthe amount of wiggliness of shrub line.")
          )
     )
     (setq bushsscnt 0)
     (setq bushsslen (sslength bushset))
     (while (< bushsscnt bushsslen)
            (setq bshent (ssname bushset bushsscnt))
            (setq bshdata (entget bshent))
            (setq bshtype (cdr (assoc 0 bshdata)))
            (setq bshcheck (member bshtype accepted))
            (setq ent210 (assoc 210 bshdata))
            (setq splcheck nil)
            (if (= bshtype "POLYLINE")
                (progn
                   (setq pl70 (cdr (assoc 70 bshdata)))
                   (setq splcheck (logand pl70 4))
                )
            )
            (if (or (or (= bshcheck nil)
                    (not (equal ent210 '(210 0.0 0.0 1.0))))
                    (= splcheck 4))
                (setq bushsscnt (+ 1 bushsscnt))
                (progn
                   (mbush)
                   (setq bushsscnt (+ 1 bushsscnt))
                )
            )
      )
     (command "_UCS" "P")
     (bush-mode-restore)
     (setq *error* olderror)
     (princ)
)



(defun mbush ()
     (if (= bshtype "POLYLINE")
         (command "CONVERT" "P" "S" bshent "")
     )
     (if (= bshtype "LWPOLYLINE")
         (progn
            (setq pl70 (cdr (assoc 70 bshdata)))
            (setq closechk (logand pl70 1))
            (if (= closechk 0)
                (setq bshtype "LWPOLYLINE0")
                (setq bshtype "LWPOLYLINE1")
            )

         )
     )
;;
;;CREATE A LIST OF BUSH POINTS FOR AN ARC THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= bshtype "ARC")
         (progn
            (arctolwp bshent)
            (setq bshent (entlast))
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN LINE THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= bshtype "LINE")
         (progn
            (lintolwp bshent)
            (setq bshent (entlast))
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN CIRCLE THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= bshtype "CIRCLE")
         (progn
            (cirtolwp bshent)
            (setq bshent (entlast))
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN OPEN LWPOLYLINE
     (if (= bshtype "LWPOLYLINE0")
         (progn
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR A CLOSED LWPOLYLINE
     (if (= bshtype "LWPOLYLINE1")
         (progn
            (setq bshdata (entget bshent))
;;PRESERVE 'BSHDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq bshinfo bshdata)
          )
     )
     (shrubmod)
     (entupd bshent)
)
