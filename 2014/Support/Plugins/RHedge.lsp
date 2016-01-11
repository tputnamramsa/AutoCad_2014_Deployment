;;(setq check (getvar "ACADVER"))
;;(if (/= check "15.0")
;;    (progn
;;       (princ "\nIncompatible Version of AutoCAD")
;;       (princ "\nThis routine can only be used with")
;;       (princ "\nAutoCAD Release 15.0 aka AutoCAD 2000")
;;       (princ)
;;       (quit)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   C:HEDGE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:hedge ( / mode-chk OKmode old-mode)


;;CHECK TO SEE IF THERE IS ALREADY A VALUE ATTACHED TO "HDGMODE" AND
;;THEN PROMPT FOR CREATING A NEW HEDGE LINE OR CONVERTING AN OLD ONE.
     (setq OKmode (list "CONVERT" "C" "NEW" "N" ""))
     (setq mode-chk (boundp 'hdgmode))
     (if (= mode-chk T)
         (setq hdgmode (strcase hdgmode))
     )
     (if (and (= mode-chk T)(= (member (strcase hdgmode) OKmode) nil))
         (setq mode-chk nil)
     )
    (if (and (= mode-chk T)(= hdgmode ""))
         (setq mode-chk nil)
     )
     (if (= mode-chk T)
         (progn
             (setq old-mode (strcase hdgmode))
             (if (or (= old-mode "N") (= old-mode "NEW"))
                 (progn
                    (setq hdgmode (strcase (getstring 
                                  "\nDo you wish to create a \"New\" hedgeline or \"Convert\" an existing line? <\"New\">  ")))
                    (while (= (member (strcase hdgmode) OKmode) nil) 
                           (prompt "\nIncorrect input.") 
                           (setq hdgmode (strcase (getstring 
                                  "\nDo you wish to create a \"New\" hedgeline or \"Convert\" an existing line? <\"New\">  ")))
                    )
                    (if (= hdgmode "")
                        (setq hdgmode "New")
                    )
                 )
             ) 
             (if (or (= old-mode "C") (= old-mode "CONVERT"))
                 (progn
                    (setq hdgmode (strcase (getstring 
                                  "\nDo you wish to create a \"New\" hedgeline or \"Convert\" an existing line? <\"Convert\">  ")))
                    (while (= (member (strcase hdgmode) OKmode) nil) 
                           (prompt "\nIncorrect input.") 
                           (setq hdgmode (strcase (getstring 
                                  "\nDo you wish to create a \"New\" hedgeline or \"Convert\" an existing line? <\"Convert\">  ")))
                    )
                    (if (= hdgmode "")
                        (setq hdgmode "Convert")
                    )
                 )
             ) 
         )
         (progn
            (initget 1 "New Convert")
            (setq hdgmode (strcase (getkword "\nDo you wish to create a \"New\" hedgeline or \"Convert\" an existing line?  ")))
         )
            
     )
     (if (or (= hdgmode "N")(= hdgmode "NEW"))
         (setq hdgmode "New")
     )      
     (if (= hdgmode "New")
         (hedge)
         (hedgeit)
     )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    HEDGE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;"HEDGE" DRAWS A HEDGE LINE.
;;USER MUST DRAW A HEDGE LINE IN A COUNTER-CLOCKWISE DIRECTION
;;
;;VARIABLES SAVED DURING SESSION ARE "hlayname" AND "hmultiple"

(defun hedge ( / olderror hdg-settings old-name-check old-name layer-check choice frz-stat
                   old-multiple 1poly fp-data num1 num2 pairtotl start-pt end-pt vtxbulge
                   se-ang new90 hincr-pair-list savedata polyinfo polydata hdgtype nsnum)
;;CREATE A TEMPORARY NAMED UCS TO RESTORE AFTER ROUTINE IS FINISHED - "xucsname"
     (tempxucs)
     (setq olderror *error* *error* hdg-error)
     (setq hdg-settings 
          '("cmdecho" "osmode" "ucsfollow" "orthomode" "clayer" "ucsicon"))
     (hdg-mode-save hdg-settings)
     (mapcar 'setvar '("cmdecho" "ucsfollow" "ucsicon") 
         '(0 0 0))
;;CHECK TO SEE IF THERE IS ALREADY A VALUE ATTACHED TO 'HLAYNAME' AND
;;THEN PROMPT FOR A LAYER NAME
     (setq old-name-check (boundp 'hlayname))
     (if (= old-name-check T)
         (progn
             (setq old-name (strcase hlayname))
             (setq hlayname (getstring (strcat "\nName of layer <" old-name ">?  ")))
             (if (or (= hlayname nil) (= hlayname ""))
                 (setq hlayname old-name)))
         (while (or (= hlayname nil)(= hlayname ""))
                (setq hlayname (getstring "\n Name of layer? "))))
;;CHECK TO SEE IF CHOSEN LAYER NAME ALREADY EXISTS OR NOT
     (setq layer-check (tblsearch "LAYER" hlayname))
     (setq choice nil)
;;IF LAYER NAME DOES NOT EXIST, ASK IF USER WANTS TO CREATE A LAYER BY THAT NAME
     (if (= layer-check nil)
         (progn
            (initget 1 "Yes No")
            (setq choice (getkword "\nLayer does not exist!! Do you wish to create it? <Yes or No>  "))
            (if (= choice "Yes")
               (command "LAYER" "New" hlayname "Set" hlayname "")))) 
;;IF USER DOES NOT WANT TO CREATE SUCH A LAYER, ASK FOR ANOTHER NAME
     (while (= choice "No")
         (setq hlayname (getstring "\nPick another layer name.  "))
         (setq layer-check (tblsearch "LAYER" hlayname))
         (if (= layer-check nil)
             (progn
                (initget 1 "Yes No")
                (setq choice (getkword "\nLayer does not exist!! Do you wish to create it? <Yes or No>  "))
                (if (= choice "Yes")
                   (command "LAYER" "New" hlayname "Set" hlayname "")
                )
             )
             (progn
                (setq choice "Yes")
                (command "LAYER" "Set" hlayname "")
             )
         )
     )
;;IF LAYER DOES EXIST, MAKE SURE IT IS NOT FROZEN - THAW IT IF NECESSARY         
     (if (/= layer-check nil)
         (progn         
           (setq frz-stat (cdr (assoc 70 layer-check)))
           (if (= (boole 1 frz-stat 1) 1)
               (command "LAYER" "thaw" hlayname "Set" hlayname ""))))
;;CHECK TO SEE IF CHOSEN MULTIPLE OR SCALE ALREADY EXISTS  - "hmultiple"
     (if (= (boundp 'hmultiple) T)
         (progn
          (setq old-multiple hmultiple)
          (initget 6 "")
          (setq hmultiple (getreal (strcat "\nWhat scale?: <" (rtos old-multiple 2 2) ">  ")))))
     (if (or (= hmultiple "")(= hmultiple nil))
         (setq hmultiple old-multiple))
     (if (= hmultiple 0.0)
         (progn
          (initget 7)
          (setq hmultiple (getreal "\nZero is not an option!!!\nWhat scale do you really want?  "))
         )
     )
     (if (= (boundp 'hmultiple) nil)
         (progn
          (initget 7)
          (setq hmultiple (getreal "\nWhat scale?:     "))))
;;USER STARTS BY DRAWING POLYLINE 
     (prompt "\nDraw hedge line in a COUNTER-CLOCKWISE direction.")
     (setq 1poly (hpline))
     (setvar "CMDECHO" 0)
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
     (setq fp-data (entget 1poly))
;;RESERVE ORIGINAL POLYLINE DATA FOR POSSIBLE REVERSAL 
     (setq savedata fp-data)
     (mapcar 'setvar '("osmode" "orthomode") 
         '(0 0))
;CREATE THE LIST "PAIRLIST" USING "VTXPAIRLIST" FUNCTION   - SEE BELOW
     (vtxpairlist 1poly)
;CREATE THE LIST OF DATA FOR NEW ENTITY "FP-DATA" USING "HDGEDATA" FUNCTION   - SEE BELOW
     (hdgedata hlayname) 
     (setq num1 0)
     (setq num2 1)
     (setq pairtotl (length pairlist))
     (while (<= num1 (- pairtotl 2))
            (setq start-pt (cdar (nth num1 pairlist)))
            (setq end-pt (cdar (nth num2 pairlist)))
            (setq vtxbulge (cdr (assoc 42 (nth num1 pairlist))))
            (setq se-ang (angle start-pt end-pt))
            (if (= vtxbulge 0.0)
                (hedgepts start-pt end-pt)
                (hedgearcpts start-pt end-pt vtxbulge)
            )
            (setq fp-data (append fp-data hincr-pair-list))
            (setq num1 (+ num1 1))
            (setq num2 (+ num2 1))
     )
;;COMPLETE THE LIST OF DATA
     (setq new90 (cons '90 (/ (- (length fp-data) 12) 4)))
     (setq fp-data (subst new90 (assoc 90 fp-data) fp-data))
     (setq fpd70 (cdr (assoc 70 fp-data)))
     (setq closechk (logand fpd70 1))
     (if (= closechk 0)
         (progn
           (setq new70 (cons '70 1))
           (setq fp-data (subst new70 (assoc 70 fp-data) fp-data))
         )
     )
     (entmod fp-data)
     (setq OKlist (list "N" "NO" "YES" "Y" ""))
     (setq hedgeOK (strcase (getstring "\nAre the indentations on the correct side of the hedge boundary \(\"Yes\" or \"No\"\)? <Yes> ")))
     (while (= (member hedgeOK OKlist) nil)
            (prompt "\nIncorrect input. ")
            (setq hedgeOK (strcase (getstring "\nAre the indentations on the correct side of the hedge boundary \(\"Yes\" or \"No\"\)? <Yes> ")))
            (if (= hedgeOK "")
                (setq hedgeOK "YES")
            )
     )
     (if (or (= hedgeOK "NO") (= hedgeOK "N"))
         (progn
            (entmod savedata)
            (revpoly savedata)
            (setq savedata polyinfo)
            (vtxpairlist (cdr (assoc -1 savedata)))
            (setq polydata polyinfo)
            (setq fp-data '())
            (repeat 14 (setq fp-data (cons (car polydata) fp-data))
                       (setq polydata (cdr polydata))
            )
            (setq fp-data (reverse fp-data))
            (setq fp-data (subst (cons '8 hlayname) (assoc 8 fp-data) fp-data))
            (setq hdgtype "LWPOLYLINE1")
            (hedgemod)
         )
     )
     (entupd 1poly)
     (command "_UCS" "Restore" xucsname)
     (command "_UCS" "Delete" xucsname)
     (hdg-mode-restore)
     (setq *error* olderror)
     (princ)
)



(defun hpline ()
     (setvar "cmdecho" 1)
     (command "pline")
     (while (= (getvar "CMDNAMES") "PLINE")
            (command pause)
     )
     (setq 1poly (entlast))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newstart (npt / numtotal numlen numlist strpart)
     (setq nsnum (abs (fix (* (car npt) (cadr npt)))))
     (while (>= nsnum 100000.00)
            (setq nsnum (/ nsnum 10))
            (setq nsnum (fix nsnum))
     )
     (while (> nsnum 9)
            (setq nsnum (itoa nsnum))
            (setq numlen (strlen nsnum))
            (setq numlist '())
            (while (> numlen 0)
                   (setq strpart (substr nsnum 1 1))
                   (setq numlist (cons strpart numlist))
                   (setq nsnum (substr nsnum 2 numlen))
                   (setq numlen (strlen nsnum))
            )
            (setq numtotal 0)
            (repeat (length numlist)
                    (setq numtotal (+ numtotal (atoi (car numlist))))
                    (setq numlist (cdr numlist))
            )
            (setq nsnum numtotal)
     )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

(defun hedgepts (1pt 2pt / hincr-prop hincr-prop-sub hincr-list hincr-sublist hincr-pt incr10 listseg 
                           listpart vx-ang vx-subang1 vx-subang2 dist num incr-total subpt) 

     (setq hincr-pair-list '())
     (setq hincr-prop '(0.75 1.6 0.5 1.4 0.6 1.85 1.6 0.8 2.1))
     (setq hincr-prop-sub '(0.12 0.2 0.28 0.22 0.18 0.32 0.20 0.2 0.26))
     (setq hincr-list (mapcar '(lambda (x)
                              (* x hmultiple))
                               hincr-prop))
     (setq hincr-sublist (mapcar '(lambda (x)
                              (* x hmultiple))
                               hincr-prop-sub))
                              
     (setq hincr-pt 1pt)
     (setq incr10 (list (cons '10 1pt)))
     (setq listseg (list '(42 . 0.0) '(41 . 0.0) '(40 . 0.0)))
     (setq listpart (append listseg incr10))
     (setq hincr-pair-list (append listpart hincr-pair-list))
     (setq vx-ang (angle 1pt 2pt))
     (setq vx-subang1 (+ vx-ang 1.18))
     (setq vx-subang2 (- vx-ang 1.18))    
     (setq dist (distance 1pt 2pt))
     (setq num 0)
     (setq incr-total 0)
     (while (< incr-total (- dist (nth 8 hincr-list)))
            (setq incr (nth num hincr-list))
            (setq sub-incr (nth num hincr-sublist))
            (setq hincr-pt (polar hincr-pt vx-ang incr))
            (setq incr10 (list (cons '10 hincr-pt)))
            (setq listpart (append listseg incr10))
            (setq hincr-pair-list (append listpart hincr-pair-list))
            (setq subpt (polar hincr-pt vx-subang1 sub-incr))
            (setq incr10 (list (cons '10 subpt)))
            (setq listpart (append listseg incr10))
            (setq hincr-pair-list (append listpart hincr-pair-list))
            (setq subpt (polar subpt vx-subang2 sub-incr))
            (setq incr10 (list (cons '10 subpt)))
            (setq listpart (append listseg incr10))
            (setq hincr-pair-list (append listpart hincr-pair-list))
            (setq incr-total (+ incr-total incr))
            (setq num (+ num 1))
            (if (= num 9)
                (setq num (- (newstart hincr-pt) 1))
            )
     )
     (setq hincr-pair-list (reverse hincr-pair-list))
     (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hedgearcpts (1pt 2pt hbulge / hincr-prop hincr-prop-sub hincr-list hincr-sublist 
                                     hincr-pt listseg incr-total num temp42 temp10 listpart
                                     alang1 negcheck gprcenter alradius alang1 alcircum 
                                     alarclen newang subpt gbbulge gspnewang gspsubpt2)
     (setq hincr-pair-list '())
     (setq hincr-prop '(0.75 1.6 0.5 1.4 0.6 1.85 1.6 0.8 2.1))
     (setq hincr-prop-sub '(0.12 0.2 0.28 0.22 0.18 0.32 0.20 0.2 0.26))
     (setq hincr-list (mapcar '(lambda (x)
                              (* x hmultiple))
                               hincr-prop))
     (setq hincr-sublist (mapcar '(lambda (x)
                              (* x hmultiple))
                               hincr-prop-sub))
                              
     (setq hincr-pt 1pt)
     (setq listseg (list '(41 . 0.0) '(40 . 0.0)))
     (plarccen 1pt 2pt hbulge)
     (arclength 1pt 2pt gprcenter)
     (setq incr-total 0)
     (setq num 0)
     (getarcpt (nth num hincr-list))
     (getbulge hincr-pt subpt)
     (setq temp42 (cons '42 gbbulge))
     (setq temp10 (list (cons '10 hincr-pt)))
     (setq listpart (cons temp42 listseg))
     (setq listpart (append listpart temp10))
     (setq hincr-pair-list (append listpart hincr-pair-list))
     (getsubpts (nth num hincr-sublist))
     (setq incr-total (+ incr-total (nth num hincr-list)))
     (setq hincr-pt gspsubpt2)
     (setq alang1 gspnewang)
     (setq num 1)
     (while (< incr-total (- alarclen (nth 8 hincr-list)))
            (getarcpt (nth num hincr-list))
            (getbulge hincr-pt subpt)
            (setq temp42 (cons '42 gbbulge))
            (setq temp10 (list (cons '10 hincr-pt)))
            (setq listpart (cons temp42 listseg))
            (setq listpart (append listpart temp10))
            (setq hincr-pair-list (append listpart hincr-pair-list))
            (setq incr-total (+ incr-total (nth num hincr-list)))
            (getsubpts (nth num hincr-sublist))
            (setq hincr-pt gspsubpt2)
            (setq alang1 gspnewang)
            (setq num (+ num 1))
            (if (= num 9)
                (setq num (- (newstart hincr-pt) 1))
            )
     )
     (setq hincr-pt gspsubpt2)
     (getbulge hincr-pt 2pt)
     (setq temp42 (cons '42 gbbulge))
     (setq temp10 (list (cons '10 hincr-pt)))
     (setq listpart (cons temp42 listseg))
     (setq listpart (append listpart temp10))
     (setq hincr-pair-list (append listpart hincr-pair-list))
     (setq hincr-pair-list (reverse hincr-pair-list))
     (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CURVE ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;"PLARCCEN" FINDS THE CEN OF AN ARC IN A LWPOLYLINE

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun getarcpt (subarclen / subang)
     (setq subang (/ subarclen alradius))
     (if (= negcheck 1)
         (setq newang (- alang1 subang))
         (setq newang (+ alang1 subang))
     )
     (setq subpt (polar gprcenter newang alradius))
)


(defun getbulge (bpt1 bpt2 / gbchord halfcord gbmidvec gbdif)
     (setq gbchord (distance bpt1 bpt2))
     (setq halfcord (/ gbchord 2.0))
     (setq gbmidvec (sqrt (- (* alradius alradius) (* halfcord halfcord))))
     (setq gbdif (- alradius gbmidvec))
     (setq gbbulge (/ gbdif halfcord))
     (if (and (= negcheck 1)(> gbbulge 0.0))
         (setq gbbulge (* gbbulge -1.0))
     )
)

;;"GETSUBPTS" FINDS THE TWO INTERMEDIATE SUBPOINTS ON AN ARC FOR HEDGE LINE.
;;MUST BE RUN AFTER "GETARCPT".
;;THIS ROUTINE USES THE VARIALBLES "ALRADIUS"  FROM "ARCLENGTH"
;;                                 "NEGCHECK"  FROM "PLARCCEN"
;;                                 "NEWANG"    FROM "GETARCPT"
;;                                 "SUBPT"     FROM "GETARCPT"
;;                                 "GPRCENTER" FROM "PLARCCEN"

(defun getsubpts (gsplen / gspdist1 gspdist2 gspbigang gsphalfnewang gspprop
                           gsparclen gsptemp10 gsplistpart gspsubpt1)
     (setq gsplistseg (list '(42 . 0) '(41 . 0) '(40 . 0)))
     (setq gspdist1 (* (cos 1.18) gsplen))
     (setq gspdist2 (* (sin 1.18) gsplen))
     (setq gspbigang (* (/ gspdist1 alradius) 2.0))
     (setq gspdist3 (+ gspdist2 (- alradius (* alradius (cos (/ gspbigang 2.0))))))
     (if (= negcheck 1)
         (setq gspnewang (- newang gspbigang))
         (setq gspnewang (+ newang gspbigang))
     )
     (if (= negcheck 1)
         (setq gsphalfnewang (- newang (/ gspbigang 2.0)))
         (setq gsphalfnewang (+ newang (/ gspbigang 2.0)))
     )
     (setq gspsubpt2 (polar gprcenter gspnewang alradius))
     (if (= negcheck 1)
         (setq gspsubpt1 (polar gprcenter gsphalfnewang (+ alradius gspdist3)))
         (setq gspsubpt1 (polar gprcenter gsphalfnewang (- alradius gspdist3)))
     )
;     (setq gspchord (distance gspsubpt1 gspsubpt2))
     (setq gspprop (/ gspbigang (* pi 2.0)))
     (setq gsparclen (* alcircum gspprop))
     (setq gsptemp10 (list (cons '10 subpt)))
     (setq gsplistpart (append gsplistseg gsptemp10))
     (setq hincr-pair-list (append gsplistpart hincr-pair-list))     
     (setq gsptemp10 (list (cons '10 gspsubpt1)))
     (setq gsplistpart (append gsplistseg gsptemp10))
     (setq hincr-pair-list (append gsplistpart hincr-pair-list))
     (setq incr-total (+ incr-total gsparclen))
)

;;  VTXPAIRLIST  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;THIS FUNCTION LIST VERTEX POINTS IN A LWPOLYLINE.     CREATES "PAIRLIST"
;;

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

(defun hdgedata (hlayname / hlaycode)
     (setq hlaycode (cons 8 hlayname))
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
     (setq fp-data (subst hlaycode (assoc 8 fp-data) fp-data))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEDGEIT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;'HEDGEIT' CREATES A HEDGE-LINE FROM A CHOSEN ENTITY
;;THE ENTITY MUST BE A 'POLYLINE', 'LWPOLYLINE', 'LINE', 'ARC' OR 'CIRCLE'


(defun hedgeit ( / olderror hdg-settings accepted hdgent old-multiple hdgdata hdgtype hdgcheck
                     msg pl70 splcheck ent210 arcdata fp-data lindata cirdata lwpdata hedgeOK 
                     arcinfo lininfo cirinfo polyinfo hincr-pair-list negcheck nsnum)
;;CREATE A TEMPORARY NAMED UCS TO RESTORE AFTER ROUTINE IS FINISHED - "xucsname"
     (tempxucs)
     (setq olderror *error* *error* hdg-error)
     (setq hdg-settings 
          '("cmdecho" "osmode" "ucsfollow" "orthomode" "clayer" "ucsicon"))
     (hdg-mode-save hdg-settings)
     (mapcar 'setvar '("cmdecho" "ucsfollow" "ucsicon" "osmode" "orthomode") 
         '(0 0 0 0 0))
     (command "UCS" "W")
     (setq accepted '("LWPOLYLINE" "ARC" "CIRCLE" "LINE" "POLYLINE"))
     (prompt "\nSelect entity which like to turn into a hedge line.  ")
     (setq hdgent (car (entsel)))
     (while (= hdgent nil)
            (prompt "\nNothing has been selected.  Try again.")
            (setq hdgent (car (entsel)))
     )
;;CHECK TO SEE IF CHOSEN MULTIPLE OR SCALE ALREADY EXISTS  - "hmultiple"
     (if (= (boundp 'hmultiple) T)
         (progn
          (setq old-multiple hmultiple)
          (initget 6 "")
          (setq hmultiple (getreal (strcat "\nWhat scale?: <" (rtos old-multiple 2 2) ">  ")))))
     (if (or (= hmultiple "")(= hmultiple nil))
         (setq hmultiple old-multiple))
     (if (= hmultiple 0.0)
         (progn
          (initget 7)
          (setq hmultiple (getreal "\nZero is not an option!!!\nWhat scale do you really want?  "))
         )
     )
     (if (= (boundp 'hmultiple) nil)
         (progn
          (initget 7)
          (setq hmultiple (getreal "\nWhat scale?:     "))))
     (setq hdgdata (entget hdgent))
     (setq hdgtype (cdr (assoc 0 hdgdata)))
     (setq hdgcheck (member hdgtype accepted))
     (if (= hdgcheck nil)
         (progn
            (setq msg (strcat "\nThe entity you selected is a \"" hdgtype "\"."
                              "\nIt cannot be made into a hedgeline." ))
            (princ msg)
            (exit)
         )
     )
     (if (= hdgtype "POLYLINE")
         (progn
            (setq pl70 (cdr (assoc 70 hdgdata)))
            (setq splcheck (logand pl70 4))
            (if (= splcheck 4)
                (progn
                   (setq msg "\nThe entity you selected is a \"SPLINED POLYLINE\".
                              \nIt cannot be made into a hedgeline." )
                   (princ msg)
                   (exit)
                )
                (progn 
                   (command "CONVERT" "P" "S" hdgent "")
                )
            )
         )
     )
     (setq ent210 (assoc 210 hdgdata))
     (if (not (equal ent210 '(210 0.0 0.0 1.0)))
         (progn
            (setq msg "\nThis entity is not parallel to the WORLD COORDINATE SYSTEM.
                       \nIt cannot be turned into a hedgeline.")
            (princ msg)
            (exit)
         )
     )
     (if (= hdgtype "LWPOLYLINE")
         (progn
            (setq pl70 (cdr (assoc 70 hdgdata)))
            (setq closechk (logand pl70 1))
            (if (= closechk 0)
                (setq hdgtype "LWPOLYLINE0")
                (setq hdgtype "LWPOLYLINE1")
            )

         )
     )
;;
;;CREATE A LIST OF HEDGE POINTS FOR AN ARC THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= hdgtype "ARC")
         (progn
            (arctolwp hdgent)
            (setq hdgent (entlast))
            (setq hdgdata (entget hdgent))
;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
            (entpairs hdgent)
;;PRESERVE 'HDGDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq arcdata hdgdata)
            (setq fp-data '())
            (repeat 14 (setq fp-data (cons (car arcdata) fp-data))
                       (setq arcdata (cdr arcdata))
            )
            (setq fp-data (reverse fp-data))
         )
     )
;;
;;CREATE A LIST OF HEDGE POINTS FOR AN LINE THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= hdgtype "LINE")
         (progn
            (lintolwp hdgent)
            (setq hdgent (entlast))
            (setq hdgdata (entget hdgent))
;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
            (entpairs hdgent)
;;PRESERVE 'HDGDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq lindata hdgdata)
            (setq fp-data '())
            (repeat 14 (setq fp-data (cons (car lindata) fp-data))
                       (setq lindata (cdr lindata))
            )
            (setq fp-data (reverse fp-data))
         )
     )
;;
;;CREATE A LIST OF HEDGE POINTS FOR AN CIRCLE THAT HAS BEEN CONVERTED INTO A LWPOLYLINE
     (if (= hdgtype "CIRCLE")
         (progn
            (cirtolwp hdgent)
            (setq hdgent (entlast))
            (setq hdgdata (entget hdgent))
;;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
            (entpairs hdgent)
;;PRESERVE 'HDGDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq cirdata hdgdata)
            (setq fp-data '())
            (repeat 14 (setq fp-data (cons (car cirdata) fp-data))
                       (setq cirdata (cdr cirdata))
            )
            (setq fp-data (reverse fp-data))
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR AN OPEN LWPOLYLINE
     (if (= hdgtype "LWPOLYLINE0")
         (progn
            (setq hdgdata (entget hdgent))
;;CREATE THE LIST "PAIRLIST" USING "VTXPAIRLIST" FUNCTION   - SEE BELOW
            (entpairs hdgent)
;;PRESERVE 'HDGDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq lwpdata hdgdata)
            (setq fp-data '())
            (repeat 14 (setq fp-data (cons (car lwpdata) fp-data))
                       (setq lwpdata (cdr lwpdata))
            )
            (setq fp-data (reverse fp-data))
         )
     )
;;CREATE A LIST OF HEDGE POINTS FOR A CLOSED LWPOLYLINE
     (if (= hdgtype "LWPOLYLINE1")
         (progn
            (setq hdgdata (entget hdgent))
;;CREATE THE LIST "PAIRLIST" USING "VTXPAIRLIST" FUNCTION   - SEE BELOW
            (vtxpairlist hdgent)
;;PRESERVE 'HDGDATA' FOR FUTURE POSSIBLE REVERSAL
            (setq lwpdata hdgdata)
            (setq fp-data '())
            (repeat 14 (setq fp-data (cons (car lwpdata) fp-data))
                       (setq lwpdata (cdr lwpdata))
            )
            (setq fp-data (reverse fp-data))
         )
     )
     (hedgemod)
     (setq OKlist (list "N" "NO" "YES" "Y" ""))
     (setq hedgeOK (strcase (getstring "\nAre the indentations on the correct side of the hedge boundary \(\"Yes\" or \"No\"\)? <Yes> ")))
     (while (= (member hedgeOK OKlist) nil)
            (prompt "\nIncorrect input. ")
            (setq hedgeOK (strcase (getstring "\nAre the indentations on the correct side of the hedge boundary \(\"Yes\" or \"No\"\)? <Yes> ")))
            (if (= hedgeOK "")
                (setq hedgeOK "YES")
            )
     )
     (if (or (= hedgeOK "N")(= hedgeOK "NO"))
         (progn
            (entmod hdgdata)
;;
;;TO REVERSE "ARC"
            (if (= hdgtype "ARC")
                (progn 
                   (revarc hdgdata)
                   (setq hdgdata arcinfo)
;;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
                   (entpairs hdgent)
                   (setq arcdata arcinfo)
                   (setq fp-data '())
                   (repeat 14 (setq fp-data (cons (car arcdata) fp-data))
                              (setq arcdata (cdr arcdata))
                   )
                   (setq fp-data (reverse fp-data))
                )
            )
;;
;;TO REVERSE "LINE"
            (if (= hdgtype "LINE")
                (progn 
                   (revlin hdgdata)
                   (setq hdgdata lininfo)
;;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
                   (entpairs hdgent)
                   (setq lindata lininfo)
                   (setq fp-data '())
                   (repeat 14 (setq fp-data (cons (car lindata) fp-data))
                              (setq lindata (cdr lindata))
                   )
                   (setq fp-data (reverse fp-data))
                )
            )
;;
;;TO REVERSE "CIRCLE"   
            (if (= hdgtype "CIRCLE")
                (progn 
                   (revcir hdgdata)
                   (setq hdgdata cirinfo)
;;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
                   (entpairs hdgent)
                   (setq cirdata cirinfo)
                   (setq fp-data '())
                   (repeat 14 (setq fp-data (cons (car cirdata) fp-data))
                              (setq cirdata (cdr cirdata))
                   )
                   (setq fp-data (reverse fp-data))
                )
            )
;;
;;TO REVERSE CLOSED "LWPOLYLINE"   
            (if (= hdgtype "LWPOLYLINE0")
                (progn 
                   (revpoly hdgdata)
                   (setq hdgdata polyinfo)
;;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
                   (entpairs hdgent)
                   (setq polydata polyinfo)
                   (setq fp-data '())
                   (repeat 14 (setq fp-data (cons (car polydata) fp-data))
                              (setq polydata (cdr polydata))
                   )
                   (setq fp-data (reverse fp-data))
                )
            )
;;TO REVERSE CLOSED "LWPOLYLINE"   
            (if (= hdgtype "LWPOLYLINE1")
                (progn 
                   (revpoly hdgdata)
                   (setq hdgdata polyinfo)
;;CREATE THE LIST "PAIRLIST" USING "ENTPAIRS" FUNCTION   - SEE BELOW
                   (vtxpairlist hdgent)
                   (setq polydata polyinfo)
                   (setq fp-data '())
                   (repeat 14 (setq fp-data (cons (car polydata) fp-data))
                              (setq polydata (cdr polydata))
                   )
                   (setq fp-data (reverse fp-data))
                )
            )
            (hedgemod)
        )

     )
     (entupd hdgent)
     (command "_UCS" "Restore" xucsname)
     (command "_UCS" "Delete" xucsname)
     (hdg-mode-restore)
     (setq *error* olderror)
     (princ)
)


(defun hedgemod ( / num1 num2 pairtotl start-pt end-pt vtxbulge se-ang final10 final42 new90)
     (setq num1 0)
     (setq num2 1)
     (setq pairtotl (length pairlist))
     (while (<= num1 (- pairtotl 2))
            (setq start-pt (cdar (nth num1 pairlist)))
            (setq end-pt (cdar (nth num2 pairlist)))
            (setq vtxbulge (cdr (assoc 42 (nth num1 pairlist))))
            (setq se-ang (angle start-pt end-pt))
            (if (= vtxbulge 0.0)
                (hedgepts start-pt end-pt)
                (hedgearcpts start-pt end-pt vtxbulge)
            )
            (setq fp-data (append fp-data hincr-pair-list))
            (if (member hdgtype '("ARC" "LINE" "LWPOLYLINE0"))
                (progn 
                   (setq final10 (list (cons '10 end-pt)))
                   (setq final42 (list '(40 . 0.0) '(41 . 0.0) '(42 . 0.0))) 
                   (setq fp-data (append fp-data final10 final42))
                )
            )
            (setq num1 (+ num1 1))
            (setq num2 (+ num2 1))
     )
     (if (= hdgtype "LWPOLYLINE1")
         (progn
            (setq fpd70 (cdr (assoc 70 fp-data)))
            (setq closechk (logand fpd70 1))
            (if (= closechk 0)
                (progn
                  (setq new70 (cons '70 1))
                  (setq fp-data (subst new70 (assoc 70 fp-data) fp-data))
                )
            )
         )
     )
     (setq new90 (cons '90 (/ (- (length fp-data) 12) 4)))
     (setq fp-data (subst new90 (assoc 90 fp-data) fp-data))
     (entmod fp-data)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;LWPOLYLINE CONVERSION FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;CONVERT LINE TO LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;REVERSE LINE/LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revlin (ldata / revdata linstart linend)
     (setq revdata ldata)
     (setq lininfo '())
     (repeat 14 (setq lininfo (cons (car revdata) lininfo))
                (setq revdata (cdr revdata))
     )
     (setq lininfo (reverse lininfo))
     (setq linstart '())
     (repeat 4 (setq linstart (cons (car revdata) linstart))
               (setq revdata (cdr revdata))
     )
     (setq linstart (reverse linstart))
     (setq linend '())
     (repeat 4 (setq linend (cons (car revdata) linend))
               (setq revdata (cdr revdata))
     )
     (setq linend (reverse linend))
     (setq lininfo (append lininfo linend))
     (setq lininfo (append lininfo linstart))
     (setq lininfo (append lininfo revdata))
     (entmod lininfo)
     (entupd (cdar lininfo))
)



;;;;;;;;;;;;;;;;;;;;;;;;;CONVERT ARC TO LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
                          temp67 temp410 temp8 temp100B temp90
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;REVERSE ARC/LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revarc (adata / revdata arcstart arcend new42)
     (setq revdata adata)
     (setq arcinfo '())
     (repeat 14 (setq arcinfo (cons (car revdata) arcinfo))
                (setq revdata (cdr revdata))
     )
     (setq arcinfo (reverse arcinfo))
     (setq arcstart '())
     (repeat 4 (setq arcstart (cons (car revdata) arcstart))
               (setq revdata (cdr revdata))
     )
     (setq new42 (cons '42 (* (cdar arcstart) -1.0)))
     (setq arcstart (subst new42 (assoc 42 arcstart) arcstart))
     (setq arcstart (reverse arcstart))
     (setq arcend '())
     (repeat 4 (setq arcend (cons (car revdata) arcend))
               (setq revdata (cdr revdata))
     )
     (setq new42 (cons '42 (* (cdar arcend) -1.0)))
     (setq arcend (subst new42 (assoc 42 arcend) arcend))
     (setq arcend (reverse arcend))
     (setq arcinfo (append arcinfo arcend))
     (setq arcinfo (append arcinfo arcstart))
     (setq arcinfo (append arcinfo revdata))
     (entmod arcinfo)
     (entupd (cdar arcinfo))
)

;;;;;;;;;;;;;;;;;;;;;;;;;CONVERT CIRCLE TO LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;REVERSE CIRCLE/LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revcir (cdata / revdata cirstart cirhalf new42)
     (setq revdata cdata)
     (setq cirinfo '())
     (repeat 14 (setq cirinfo (cons (car revdata) cirinfo))
                (setq revdata (cdr revdata))
     )
     (setq cirinfo (reverse cirinfo))
     (setq cirstart '())
     (repeat 4 (setq cirstart (cons (car revdata) cirstart))
               (setq revdata (cdr revdata))
     )
     (setq new42 (cons '42 (* (cdar cirstart) -1.0)))
     (setq cirstart (subst new42 (assoc 42 cirstart) cirstart))
     (setq cirstart (reverse cirstart))
     (setq cirhalf '())
     (repeat 4 (setq cirhalf (cons (car revdata) cirhalf))
               (setq revdata (cdr revdata))
     )
     (setq new42 (cons '42 (* (cdar cirhalf) -1.0)))
     (setq cirhalf (subst new42 (assoc 42 cirhalf) cirhalf))
     (setq cirhalf (reverse cirhalf))
     (setq cirinfo (append cirinfo cirstart))
     (setq cirinfo (append cirinfo cirhalf))
     (setq cirinfo (append cirinfo revdata))
     (entmod cirinfo)
     (entupd (cdar cirinfo))
)


;;;;;;;;;;;;;;;;;;;;;;;;;REVERSING LWPOLYLINE DIRECTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revpoly (polydata / revdata 10-list 42-list closechk vtxseg rp-vtx 
                           rp-bulge pi70 last10)
     (setq revdata polydata)
     (setq polyinfo '())
     (repeat 14 (setq polyinfo (cons (car revdata) polyinfo))
                (setq revdata (cdr revdata))
     )
     (setq polyinfo (reverse polyinfo))
     (10s revdata)
     (setq 10-list (reverse 10-list))
     (42s revdata)
     (setq 42-list (reverse 42-list))
     (setq pi70 (cdr (assoc 70 polyinfo)))
     (setq closechk (logand pi70 1))
     (if (= closechk 0)
         (progn
            (setq 42-list (cdr 42-list))
            (setq 42-list (append 42-list '(0.0)))
         )
         (progn
            (setq 10-list (reverse 10-list))
            (setq last10 (list (car 10-list)))
            (setq 10-list (cdr 10-list))
            (setq 10-list (append 10-list last10))
            (setq 10-list (reverse 10-list))
         )
     )
     (setq vtxseg (list '(40 . 0.0) '(41 . 0.0)))
     (repeat (length 10-list)
             (setq rp-vtx (list (cons '10 (car 10-list))))
             (setq rp-bulge (list (cons '42 (* (car 42-list) -1.0))))
             (setq polyinfo (append polyinfo rp-vtx vtxseg rp-bulge))
             (setq 10-list (cdr 10-list))
             (setq 42-list (cdr 42-list))
     )
     (setq revdata (list (last revdata)))
     (setq polyinfo (append polyinfo revdata))
     (entmod polyinfo)
     (entupd (cdar polyinfo))
)



;;LIST VERTICES IN LWPOLYLINE
(defun 10s (g-list / p-list plentnam pl-len pl-count pl-ent pl-pt)
     (setq p-list g-list)
     (setq 10-list '())
     (setq plentnam (cdar p-list))
     (setq pl-len (length p-list))
     (setq pl-count 0)
     (while (< pl-count pl-len)
            (setq pl-ent (nth pl-count p-list))
            (if (= (car pl-ent) '10)
                (progn
                     (setq pl-pt (cdr pl-ent))
                     (setq 10-list (cons pl-pt 10-list))

                )
            )
            (setq pl-count (+ pl-count 1))
     )
     (setq 10-list (reverse 10-list))
)

;;LIST BULGES OF VERTICES IN LWPOLYLINE
(defun 42s (g-list / p-list plentnam pl-len pl-count pl-ent pl-pt)
     (setq p-list g-list)
     (setq 42-list '())
     (setq plentnam (cdar p-list))
     (setq pl-len (length p-list))
     (setq pl-count 0)
     (while (< pl-count pl-len)
            (setq pl-ent (nth pl-count p-list))
            (if (= (car pl-ent) '42)
                (progn
                     (setq pl-pt (cdr pl-ent))
                     (setq 42-list (cons pl-pt 42-list))

                )
            )
            (setq pl-count (+ pl-count 1))
     )
     (setq 42-list (reverse 42-list))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ENTPAIRS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;C:HEDGEID;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:hedgeid (/ hent hentdata hentwhat hentlen h10list h10cnt hent8
                     msg negcheck alarclen alradius gprcenter)
     (alert "\n  \"HEDGEID\" will not always give you accurate information.
             \n         To help insure that information will be accurate,
             \n                    select an unaltered, \"hedgeline\".
             \n\n                  If you do not get the correct results
             \n                      try selecting another hedgeline.")
     (setq hent (car (entsel "\nSelect a \"hedge\" polyline. ")))
     (while (= hent nil)
            (setq hent (car (entsel "\nNothing selected. Select a \"hedge\" polyline.")))
     )
     (setq hentdata (entget hent))
     (setq hentwhat (cdr (assoc 0 hentdata)))
     (while (/= hentwhat "LWPOLYLINE")
            (prompt "\nThis is not a valid hedge polyline.
                     \nSelect another polyline. ")
            (setq hent (car (entsel))) 
            (while (= hent nil)
                   (setq hent (car (entsel "\nNothing selected. Select a \"hedge\" polyline.")))
            )
            (setq hentdata (entget hent))
            (setq hentwhat (cdr (assoc 0 hentdata)))
     )
     (setq hentlen (cdr (assoc 90 hentdata)))
     (while (< hentlen 30)
            (prompt "\nThis is probably not a valid shrub polyline,
                     \nor is too short to give accurate information.")                  
            (setq hent (car (entsel "\nSelect another hedge polyline. ")))
            (while (= hent nil)
                   (setq hent (car (entsel "\nNothing selected. Select a \"hedge\" polyline.")))
            )
            (setq hentdata (entget hent))
            (setq hentwhat (cdr (assoc 0 hentdata)))
            (while (/= hentwhat "LWPOLYLINE")
                   (setq hent (car (entsel "\nThis is not a valid hedge polyline.
                                            \nSelect another hedge polyline. ")))
                   (while (= hent nil)
                          (setq hent (car (entsel "\nNothing selected. Select a \"hedge\" polyline.")))
                   )
                   (setq hentdata (entget hent))
                   (setq hentwhat (cdr (assoc 0 hentdata)))
            )
            (setq hentlen (cdr (assoc 90 hentdata)))
     )
     (shortvtx hentdata)
     (setq hent8 (cdr (assoc 8 hentdata)))
     (setq hlayname hent8)
     (setq msg (strcat "\nThe hedgeline you have selected is on layer \"" 
                          (strcase hlayname) "\".
                        \nThe scale is " (rtos hmultiple 2 1)))
     (princ msg)
     (princ)
)


(defun shortvtx (datalist / data70 closechk pairlist negcheck gprcenter alradius
                            alcircum alang1 alarclen proptest length1 length2 vtx1
                            vtx2 vtx3 vtxblg1 vtxblg2 hdgscale num1 num2 num3)
     (setq data70 (cdr (assoc 70 datalist)))
     (setq closechk (logand data70 1))
     (if (= closechk 0)
         (entpairs (cdr (assoc -1 datalist)))
         (vtxpairlist (cdr (assoc -1 datalist)))
     )
     (setq datalen (length pairlist))
     (setq num1 0
           num2 1
           num3 2
           length1 nil
           length2 nil
           proptest nil
           hdgscale nil)
     (while (< num3 datalen)
          (setq vtx1 (cdar (nth num1 pairlist))
                vtx2 (cdar (nth num2 pairlist))
                vtx3 (cdar (nth num3 pairlist))
                vtxblg1 (cdr (assoc 42 (nth num1 pairlist)))
                vtxblg2 (cdr (assoc 42 (nth num2 pairlist)))
          )
          (if (= vtxblg1 0.0)
              (setq length1 (distance vtx1 vtx2))
              (progn 
                (plarccen vtx1 vtx2 vtxblg1)
                (arclength vtx1 vtx2 gprcenter)
                (setq length1 alarclen)
              )
          )
          (if (= vtxblg2 0.0)
              (setq length2 (distance vtx2 vtx3))
              (progn 
                (plarccen vtx2 vtx3 vtxblg2)
                (arclength vtx2 vtx3 gprcenter)
                (setq length2 alarclen)
              )
          )
          (setq proptest (rtos (/ length1 length2) 2 2))
          (setq proptest (atof proptest))
          (if (= proptest 6.25)
              (progn 
                 (setq hdgscale (/ length2 0.12))
                 (setq num3 datalen)
              )
          )
          (setq num1 (+ 1 num1)
                num2 (+ 1 num2)
                num3 (+ 1 num3))
     )
     (if (= hdgscale nil)
         (progn
            (alert "\nThe scale of this \"hedgeline\" could not be determined")
            (exit)
         )
         (progn
            (setq hdgscale (rtos hdgscale 2 1))
            (setq hmultiple (atof hdgscale))
         )
     )
)

;;CREATES A TEMPORARY NAMED UCS FROM CURRENT UCS 
;;ALL SYMBOLS AND VARIABLES ARE ELIMINATED EXCEPT "XUCSNAME"

(defun tempxucs ()
; / oldlprec xucs10 xucs11 xucs12 xucs2 ucscheck ucsdata)
     (setq oldlprec (getvar "LUPREC"))
     (setvar "LUPREC" 6)
     (setq xucs10 (cons '10 (getvar "UCSORG")))
     (setq xucs11 (cons '11 (getvar "UCSXDIR")))
     (setq xucs12 (cons '12 (getvar "UCSYDIR")))
     (setq xucsname "x")
     (setq ucscheck (tblobjname "UCS" xucsname))
     (while (/= ucscheck nil)
            (setq xucsname (strcat xucsname "x"))
            (setq ucscheck (tblobjname "UCS" xucsname))
     )
     (setq xucs2 (cons '2 xucsname))
     (setq ucsdata (list '(0 . "UCS")
                         '(100 . "AcDbSymbolTableRecord") 
                         '(100 . "AcDbUCSTableRecord")
                          xucs2
                         '(70 . 0)
                          xucs10
                          xucs11
                          xucs12
                    )
     )
     (entmake ucsdata)
     (setvar "LUPREC" oldlprec)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ERROR HANDLING;;;;;;;;;;;;;;;;;;;;;;;;;


(defun hdg-error (s)
     (if (/= s "Function cancelled")
         (princ (strcat "\nError: " s)))
     (setvar "CMDECHO" 0)
     (command "_UCS" "Restore" xucsname)
     (command "_UCS" "Delete" xucsname)
     (hdg-mode-restore)
     (setq *error* olderror)
     (princ))
     
(defun hdg-mode-save (a)
     (setq hdg-mode-list '())
     (repeat (length a)
             (setq hdg-mode-list (append hdg-mode-list 
                       (list (list (car a)(getvar (car a))))))
             (setq a (cdr a))))

(defun hdg-mode-restore ()
     (repeat (length hdg-mode-list)
             (setvar (caar hdg-mode-list)(cadar hdg-mode-list))
             (setq hdg-mode-list (cdr hdg-mode-list))))