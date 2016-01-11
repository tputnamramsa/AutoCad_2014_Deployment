(vl-load-com)
;; Import named user coordinate systems from selected source file.
(defun c:ImportUCS (/ UCSs fullpath srcdoc srcucss cnt name ucslst 
                       DocAtPath)

  ;; Argument: full path.
  ;; Returns a document object. An ODBX doc if the file isn't open.
  ;; Otherwise a doc contained in the active documents collection.
  (defun DocAtPath (path / *acad* version file srcdoc)
    (setq *acad* (vlax-get-acad-object))
    ;check the documents collection
    (vlax-for x (vla-get-documents *acad*)
      (if (= (strcase path) (strcase (vlax-get x 'FullName)))
        (setq srcdoc x)
      )
    )
    ;otherwise use ObjectDBX
    (if (not srcdoc)
      (cond
        ;2004 or later. Allow for future versions like 
        ;"ObjectDBX.AxDbDocument.17" by Tony Tanzillo
        ((> (setq version (atoi (getvar "AcadVer"))) 15)
          (setq srcdoc 
            (vla-GetInterfaceObject *acad* 
              (strcat "ObjectDBX.AxDbDocument." (itoa version))))
          (vla-open srcdoc path)
        )
        ;prior to 2004
        (T
          (if
            (and
              (vl-catch-all-error-p
                (vl-catch-all-apply 
                  'vla-GetInterfaceObject
                    (list *acad* "ObjectDBX.AxDbDocument")))
              (setq file (findfile "AxDb15.dll"))
            )
            (startapp "regsvr32.exe" (strcat "/s \"" file "\""))
          )
          (setq srcdoc (vla-GetInterfaceObject *acad* "ObjectDBX.AxDbDocument"))
          (vla-open srcdoc path)
        )
      )
    )
    srcdoc
  ) ;end


  ;; primary function
  (setq UCSs 
    (vla-get-UserCoordinateSystems
      (vla-get-activedocument (vlax-get-acad-object))))
  (and 
    (setq fullpath (getfiled "Source File" (getvar "dwgprefix") "dwg" 0))
    (setq srcdoc (DocAtPath fullpath))
    (setq srcucss (vla-get-UserCoordinateSystems srcdoc))
    (< 0 (vlax-get srcucss 'Count))
    (setq cnt 0)
    (vlax-for x srcucss
      (setq name (vlax-get x 'Name))
      (if 
        (and
          (not (tblsearch "ucs" name))
          (not (wcmatch name "_Active*"))
        )
        (setq ucslst (cons x ucslst)
              cnt (1+ cnt)
        )
      )
    )
    (not (vlax-invoke srcdoc 'copyobjects ucslst UCSs))
    (princ (strcat "\nNumber of coordinate systems imported: " (itoa cnt)))
  ) ;and
  (if srcdoc (vlax-release-object srcdoc))
  (princ)
) ;end