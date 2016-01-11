;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defines block flattening routine
;; from a newsgroup post

(defun c:Flatblk ( / blocks stpt enpt inspt )
(setq blocks
(vla-get-blocks
(vla-get-activedocument
(vlax-get-acad-object))))
(vlax-for blk blocks
(vlax-for item blk
(if (= "AcDbLine" (vlax-get item 'ObjectName))
(progn
(setq stpt (vlax-get item 'StartPoint))
(setq stpt (subst 0.0 (caddr stpt) stpt))
(vlax-put item 'StartPoint stpt)
(setq enpt (vlax-get item 'EndPoint))
(setq enpt (subst 0.0 (caddr enpt) enpt))
(vlax-put item 'EndPoint enpt)
)
)
(if (= "AcDbAttributeDefinition" (vlax-get item 'ObjectName))
(progn
(setq inspt (vlax-get item 'InsertionPoint))
(setq inspt (subst 0.0 (caddr inspt) inspt))
(vlax-put item 'InsertionPoint inspt)
)
)
)
)
(princ)
) ;end
