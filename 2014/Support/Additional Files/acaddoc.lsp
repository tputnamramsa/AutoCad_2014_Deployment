;;load ramsadoc.lsp from network

; Updated for AutoCAD 2014 [W2]

; This file resides on O:\ 
; The following condition statement allows it to work 
; properly for either version:
(cond
  ((= (atof (getvar "acadver")) 18.2)
    (setq rsa#path "o:\\reference\\autocad\\2012\\")
  )  
  ((= (atof (getvar "acadver")) 18.0)
    (setq rsa#path "o:\\reference\\autocad\\2010\\")
  )
  ((= (atof (getvar "acadver")) 19.1)                                   ;Edited for
    (setq rsa#path "\\\\ramsacfs\\Project\\Reference\\AutoCAD\\2014\\") ;ACAD 2014 [W2]
  )
) ; [KCS]

(setq
  rsa#support (strcat rsa#path "Support\\")
  rsa#content (strcat rsa#path "Content\\")
  rsa#plugins (strcat rsa#path "Support\\Plugins\\")
)

(load (strcat rsa#support "ramsadoc.lsp"))
