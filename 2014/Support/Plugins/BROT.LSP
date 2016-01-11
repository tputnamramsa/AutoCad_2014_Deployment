(defun c:BROT ( / ss ang ) (vl-load-com)

(if (and (setq ss (ssget "_:L" '((0 . "INSERT"))))
(setq ang (getangle "\nSpecify Rotation Angle: "))
)
(
(lambda ( i / e o )
(while (setq e (ssname ss (setq i (1+ i))))
(vla-put-rotation (setq o (vlax-ename->vla-object e))
(+ (vla-get-rotation o) ang)
)
)
)
-1
)
)

(princ)
)
