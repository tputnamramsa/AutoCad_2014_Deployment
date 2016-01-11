(defun c:rpath (/)
  (vlax-For blk
	    (vla-Get-Blocks (vla-Get-ActiveDocument (vlax-Get-Acad-Object)))
    (if	(and (= (vla-Get-IsXref blk) :vlax-True)
	     (/= (vl-string-search "." (vla-get-path blk)) 0)
	)
      (vla-put-path
	blk
	(LM:XRef:Full->Relative
	  (vl-string-right-trim "\\" (getvar 'DWGPREFIX))
	  (vla-get-path blk)
	)
      )
    )
  )
  (vl-cmdf "_.qsave")
  (princ)
)

(defun LM:XRef:Full->Relative ( dir path / p q )
    (setq dir (vl-string-right-trim "\\" dir))
    (cond
        (   (and
                (setq p (vl-string-position 58  dir))
                (setq q (vl-string-position 58 path))
                (not (eq (strcase (substr dir 1 p)) (strcase (substr path 1 q))))
            )
            path
        )
        (   (and
                (setq p (vl-string-position 92  dir))
                (setq q (vl-string-position 92 path))
                (eq (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            (LM:Xref:Full->Relative (substr dir (+ 2 p)) (substr path (+ 2 q)))
        )
        (   (and
                (setq q (vl-string-position 92 path))
                (eq (strcase dir) (strcase (substr path 1 q)))
            )
            (strcat ".\\" (substr path (+ 2 q)))
        )
        (   (eq "" dir)
            path
        )
        (   (setq p (vl-string-position 92 dir))
            (LM:Xref:Full->Relative (substr dir (+ 2 p)) (strcat "..\\" path))
        )
        (   (LM:Xref:Full->Relative "" (strcat "..\\" path)))
    )
)