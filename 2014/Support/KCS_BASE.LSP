; KCS_BASE.LSP - Common functionality for KCS applications
; Copyright© 2000-2012, Krupa CADD Solutions
; Provided for use by Robert A.M. Stern Architects LLP

; Revised 2/27/12 for R2012 [KCS]
; Revised 2/27/12 - function added: kcs_entsel, kcs_setxrecord, kcs_getxrecord [KCS]
; Revised 5/30/12 - kcs_isxref function added [KCS]

;----------------------------------------------------------------------------
(defun kcs_error (msg) 
  (if (not (member (strcase msg 't) '("function cancelled" "quit / exit abort" "console break" "ads error in evaluation"))) 
    (prompt msg)
  ) 
  (kcs_post)
)

(defun kcs_setacadvar (var val)
  (if (< (atof (getvar "acadver")) 15)
    (setvar var val)
    (progn
      (vl-load-com)
      (if (= (type val) 'LIST)
        (setvar var val)
        (vla-setvariable (vla-get-activedocument(vlax-get-acad-object)) var val)
      )
    )
  )
)

(defun kcs_pre () 
  (setq kcs#ssgetfirst (cadr (ssgetfirst))) ; 11/14/01
  (if (not kcs#pre) (progn 
    (setvar "cmdecho" 0)
    (setq kcs$usermode nil)
    (setq kcs#pushvar nil kcs#pusheval nil)
    (kcs_pushvar "expert,savetime,regenmode,users4,cmdecho")
    (if (not kcs#debug)
      (setq kcs#errold *error* *error* kcs_error)
    )
    (command "_.undo" "_auto" "_off" "_.undo" "_end" "_.undo" "_group") 
    (setvar "expert" 5) (setvar "savetime" 0)(setvar "regenmode" 0)
    (setvar "users4" "LISP")
    (setq kcs#pre t)
  ))
)

(defun kcs_post () 
  (if kcs#pre (progn 
    (while (> (getvar "cmdactive") 0)(command nil))
    (setq kcs#ssgetfirst nil) ; 11/14/01
    (if (not kcs#debug)(setq *error* kcs#errold kcs#pre nil kcs#errold nil))
    (kcs_popvar '("expert" "savetime") t) 
    (kcs_popeval nil 't) 
    (kcs_popvar nil 't) 
    (kcs_pushvar "cmdecho")
    (setvar "cmdecho" 0)
    (kcs_popvar nil 't) 
    (command "_.undo" "_auto" "_on" "_.undo" "_end") 
    (gc)
  )) 
  (princ)
)

(defun kcs_pushvar (vars / val)
  (if (= (type vars) 'STR)(setq vars (kcs_str2list vars)))
  (foreach var vars
    (if (setq val (getvar var))
      (setq kcs#pushvar (cons (cons var val) kcs#pushvar))
      (princ "\nERROR: Invalid name passed.")
    )
  )
  kcs#pushvar ; return for testing
)
(defun kcs_popvar (vars eflag / idx vp cmdecho)
  (if (= (getvar "cmdactive") 0) (progn
    (setq cmdecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    (command "graphscr")
    (setvar "cmdecho" cmdecho)
  ))
  (if (null vars)
    ; Do ALL pushed vars
    (progn
      ;(if kcs#debug (princ "\nPOPPING ALL VARS "));********
      (if eflag (foreach vp kcs#pushvar (kcs_setacadvar (car vp)(cdr vp))))
      (setq kcs#pushvar nil)
    )
    ; Do specified vars only
    (progn
      (if (= (type vars) 'STR)(setq vars (kcs_str2list vars)))
      ;(if kcs#debug (progn (princ "\nkcs#PUSHVAR(1): ")(prin1 kcs#pushvar)));********
      (foreach var vars
        (if (setq idx (kcs_idxlist var (mapcar 'car kcs#pushvar)))(progn
          ;(if kcs#debug (prin1 VAR));********
          (setq 
            vp (nth idx kcs#pushvar)
            kcs#pushvar (kcs_choplist kcs#pushvar idx)
          )
          (if eflag (kcs_setacadvar (car vp)(cdr vp)))
        ))
      )
      ;(if kcs#debug (progn (princ "\nkcs#PUSHVAR(2): ")(prin1 kcs#pushvar)));********
    )
  )
  kcs#pushvar ; return for testing
)

(defun kcs_pusheval (expr) 
  (setq kcs#pusheval (cons expr kcs#pusheval))
)

(defun kcs_popeval (expr eflag / idx cmdecho) 
  (if (= (getvar "cmdactive") 0) (progn
    (setq cmdecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
  ))
  (if expr 
    ; Do specified eval only
    (if (setq idx (kcs_idxlist expr kcs#pusheval))(progn
      (if eflag (eval expr))
      (setq kcs#pusheval (kcs_choplist kcs#pusheval idx))
    ))
    ; Do ALL evals
    (progn 
      (if eflag (foreach expr kcs#pusheval (eval expr)))
      (setq kcs#pusheval nil) 
    )
  ) 
  (if (= (getvar "cmdactive") 0)
    (setvar "cmdecho" cmdecho)
  )
)

(defun kcs_usermode (mode / vlist action)
; mode: "off" for program, "on" for user
  (setq vlist '("blipmode" "highlight" "elevation" "thickness" "osmode"))
  (if (= (strcase mode 't) "off")
    (if (not kcs$usermode); do nothing if already done
      (progn  
        (if kcs#debug (princ "\nUSER MODES OFF "));********
        (kcs_pushvar vlist)
        (if kcs#debug (progn (princ "\nkcs#PUSHVAR(UserMode_ON): ")(prin1 kcs#pushvar)));********
        (mapcar '(lambda (v) (setvar v 0)) vlist)
        (setq kcs$usermode t)
        (kcs_pusheval '(setq kcs$usermode nil))
        (setq action 'T)
      )
      (if kcs#debug (princ "\nUSER MODES ALREADY OFF "));********
    )
    ; On
    (if kcs$usermode 
      (progn
        (if kcs#debug (princ "\nUSER MODES ON "));********
        (kcs_popvar vlist 't)
        (kcs_popeval '(setq kcs$usermode nil) 'T)
        (setq action 'T)
      )
      (if kcs#debug (princ "\nUSER MODES ALREADY ON "));********
    )
  )
  action ; return 'T if did something, nil if no change
)

;----------------------------------------------------------------------------
; DEBUGGING
; save original *error* at initial load and keep it:
(if (null kcs#errsave)(setq kcs#errsave *error*))
;
(defun c:kcs_TEST () 
  (setq kcs#debug t kcs#pusheval nil kcs#pushvar nil kcs#pre nil) ;
  (defun *error* (s)(textscr)(vl-bt))
  (setvar "cmdecho" 0) (setvar "expert" 0)
  ;(if (= (getvar "pdmode") 0)(setvar "pdmode" 3))
  (princ "\nTEST mode:  Debug is ON, Error trapping is OFF. ")
  (princ)
)
(defun c:kcs_NORM ()
  (setq kcs#debug nil *error* kcs#errsave kcs#pusheval nil kcs#pushvar nil kcs#pre nil);
  (setvar "expert" 0)(setvar "highlight" 1)(setvar "blipmode" 1)
  ;(if (/= (getvar "pdmode") 0)(setvar "pdmode" 0))
  (if (/= (getvar "aperture") 5)(setvar "aperture" 5))
  (if (/= (getvar "pickbox") 3)(setvar "pickbox" 3))
  (princ "\nNORMAL mode:  Menuecho and Cmdecho are OFF.  Error trapping is ON.")
  (princ)
)
; Test function (abridged version)
(if (null kcs_varstat)
  (defun kcs_varstat ({prompt} {vlist})
    (if kcs#debug (progn
      (princ "\n------------------------------")
      (foreach {v} {vlist}
        (terpri)(princ {v})(princ ": ")(prin1 (eval {v}))
      )
      (princ (strcat " << " {prompt}))(princ)
      (alert {prompt})
    ))
  )
)

;=============================================================================
; General purpose functions also used by functions above
(defun kcs_idxlist (val lst / n idx) 
  (if (= (type val) 'STR)
    (progn
      (setq 
        val (strcase val)
        n 0
      )
      (foreach item lst
        (if (= (type item) 'STR) (setq item (strcase item)))
        (if (and (null idx)(= item val))(setq idx n))
        (setq n (1+ n))
      )
    )
    ; not strings
    (progn
      (setq 
        idx (member val lst)
        idx (if idx (- (length lst) (length idx)) nil)
      )
    )
  )
  idx
)

(defun kcs_choplist (alist pos / nlist idx) 
  (if (and alist pos (<= pos (1- (length alist)))) 
    (progn 
      (setq idx 0) 
      (while (< idx pos) 
        (setq nlist (append nlist (list (nth idx alist))) idx (1+ idx))
      ) 
      (setq idx (1+ idx)) 
      (while (<= idx (1- (length alist))) 
        (setq nlist (append nlist (list (nth idx alist))) idx (1+ idx))
      ) 
      nlist
    ) 
    alist
  )
)

; Convert comma-delimited string to list of strings
(defun kcs_str2list (str1 / n list1) 
  (setq n 1 list1 nil) 
  (while (<= n (strlen str1)) 
    (cond 
      ((and (= (substr str1 n 1) ",") (> n 1) (= (substr str1 (1- n) 1) "\\")) 
        (setq str1 (strcat (substr str1 1 (- n 2)) (substr str1 n)))
      ) 
      ((= (substr str1 n 1) ",") 
        (setq list1 (cons (substr str1 1 (1- n)) list1) str1 (substr str1 (1+ n)) n 1)
      ) 
      (t (setq n (1+ n)))
    )
  ) 
  (reverse (cons str1 list1))
)
; Convert list of strings to comma-delimited string
(defun kcs_list2str (list1 / str1)
  (setq str1 (car list1))
  (foreach item (cdr list1)
    (setq str1 (strcat str1 "," item))
  )
  str1
)

; replace character(s) in a string with other character(s)
(defun kcs_strsubst (string find replace / return findlen count test)
   (setq 
      findlen (strlen find)
      count 1
      return ""
   )
   (while (/= "" (setq test (substr string count findlen)))
      (if (= test find)
        (setq 
          return (strcat return replace)
          count (+ count findlen)
        )
        (setq 
          return (strcat return (substr string count 1))
          count (1+ count)
        )
      )
   )
   return
)

;============================================================================|

; More general purpose functions - used in other program files   
;============================================================================|
(defun kcs_dxf (item list1 / return)
  (if (setq return (cdr (assoc item list1)))
     return
     (if (= item 39) 0.0) ; thickness - return 0 if not present
  )
)

;=============================================================================
(defun kcs_hasbit (fish pool / result)
  (if (= fish 0) 
    (setq result (= pool 0))
    (setq result (= (logand fish pool) fish))
  )
  result
);kcs_hasbit 

;=============================================================================
(defun kcs_ismetric ()
  (= (getvar "measurement") 1)
)

(defun kcs_ispspace ()
  (not (or (= (getvar "tilemode") 1) (> (getvar "cvport") 1)))
);kcs_ispspace 

(defun kcs_isxref (bname / blkdata)
  (and 
    (setq blkdata (tblsearch "block" bname)) ; not detached
    (kcs_hasbit 4 (kcs_dxf 70 blkdata)) ; 4=isxref (attached or overlay)
  )
);kcs_isxref 
;=============================================================================
; Convert point (list of reals) to SafeArray for ActiveX functions
(defun kcs_point->vpoint (pt / arraySpace)
  ; allocate space for an array of 2d points stored as doubles
  (setq  arraySpace
    (vlax-make-safearray
      vlax-vbdouble    ; element type (5)
      (cons 0 (- (length pt) 1))  ; array dimension
    )
  )
  (vlax-safearray-fill arraySpace pt)
)

;=============================================================================
; Select entity of specific type(s) - loop on missed pick or wrong entity pick,
; exit only on Enter or good pick
; ARG: prmpt
; ARG: etypestr: a wcmatch string for one or more entity types to accept
; ARG: errstr: a string to display if wrong entity type is selected
(defun kcs_entsel (prmpt etypestr errstr / done pick)
  (while (not done)
    (setvar "errno" 0)
    (setq pick (entsel prmpt))
    (cond
      ((= (getvar "errno") 52) (setq done T)) ; Enter or Spacebar - bail out (pick is nil)
      ((= (getvar "errno") 7) (princ "\nNo object selected - try again... "))
      ((wcmatch (kcs_dxf 0 (entget (car pick))) etypestr) (setq done T)) ; good pick
      (T (princ errstr)) ; wrong object picked
    )
  );while
  pick
);kcs_entsel

;=============================================================================
; Set/Get value in drawing
;;; Adds a single string item record to a dictionary
;;; Creates dictionary if not present
;;; Overwrites any previous entry for specified key
;;; Arguments:   dictName = Name of dictionary to access (string)
;;;            recordName = Keyname for the record object
;;;                 value = string value
;;; Returns: Entity name of new xrecord
;;; ex: (kcs_setxrecord "KCS" "SomeKey" "Some Value")
(defun kcs_setxrecord (dictName key value / dataList xname dict)
  ; Create dictionary if not exist
  (if (not (dictsearch (namedobjdict) dictName))
    (dictadd (namedobjdict) dictName 
        (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))
  )
  (setq dict (cdr (assoc -1 (dictsearch (namedobjdict) dictName))))
  ; Remove key if exists
  (if (dictsearch dict key) 
    (dictremove dict key)
  )
  (setq    
    dataList  (list (cons 1 value))
    dataList  (append (list '(0 . "XRECORD") '(100 . "AcDbXrecord")) dataList)
    xname     (entmakex dataList)
  )
  ; Add key and value
  (if xname (dictadd dict key xname))
);kcs_setxrecord 

;;; Returns stored item (or "" if not exist)
(defun kcs_getxrecord (dictName key / dict value)
  (setq dict (cdr (assoc -1 (dictsearch (namedobjdict) dictName))))
  (if dict (setq value (cdr (assoc 1 (dictsearch dict key)))))
  (if (null value)(setq value ""))
  value
);kcs_getxrecord 

;============================================================================
;============================================================================
;============================================================================
;============================================================================
