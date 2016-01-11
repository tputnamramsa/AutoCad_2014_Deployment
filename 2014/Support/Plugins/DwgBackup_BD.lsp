;DwgBackup_BD.lsp
;William Work
;V1.0 3-01-2012
;V1.1 5-04-2012 Added error trapping for
;               file not yet saved.
;V1.2 7-06-2012 Added error trapping for
;               nonstandard project directory structure.
;V1.3 2-11-2013 Corrected apparent parsing error.        

;The BACKUP command creates a date stamped copy
;of the current state of the current drawing and places
;it in the project's Record Sets/Backup Drawings directory.

;=====================================================
; ** DATE PARSER **
;=====================================================
(defun NOW ( / date yyyy mm dd h m s)
  (setq date (rtos (getvar "cdate") 2 6)
      yyyy (substr date 1 4)
        mm (substr date 5 2)
        dd (substr date 7 2)
         h (substr date 10 2)
         m (substr date 12 2)
         s (substr date 14 2)
  )
  (strcat yyyy "-" mm "-" dd "-" h m " ")
)
;=====================================================
; ** MAIN FUNCTION **
;=====================================================
(defun c:BACKUP ( / 
                    dn         ;drawing name and extension
                    fs         ;file specification
                    ft         ;drawing name
                    pat1       ;first search pattern
                    pat2       ;second search pattern
                    sl1        ;string length of first search pattern
                    sl2        ;string length of second search pattern
                    sp         ;counting base point
                    p1         ;start of project name
                    p2         ;end of project name
                    projname   ;project name
                    filepath   ;destination file path
                    bfs        ;backup file specification
                    se         ;current value of expert
                 )
;->Obtain filename specification
  (setq dn (getvar 'DWGNAME))
  (if
    (= (setq fs (findfile dn)) nil)
      (progn (alert (strcat dn " has not been saved."))(exit))
  )
  (setq ft (vl-string-trim ".dwg" dn))
;->Is the file in a Project directory?
  (setq pat1 "\\Project\\")
  (setq pat2 "\\")
  (setq sl1 (strlen pat1))
  (setq sl2 (strlen pat2))
  (if
    (= (setq sp (vl-string-search pat1 fs 0)) nil)
      (progn (alert (strcat dn " is not a project file."))(exit))
  )
  (setq p1 (+ sp sl1))
  (setq p2 (vl-string-search pat2 fs p1))
  (setq projname (substr fs (1+ P1)(- p2 p1)))
;->Is the directory structure standard?
  (setq filepath (strcat "o:" pat1 projname "\\drawings\\record sets\\"))
  (if
    (= (vl-file-directory-p filepath) nil)
      (progn (alert (strcat "Project " projname " has a nonstandard directory structure."))(exit))
  )
  (setq filepath (strcat "o:" pat1 projname "\\drawings\\record sets\\Backup Drawings\\"))
;->Make destination directory if required 
  (vl-mkdir filepath)
;->Construct filename    
  (setq bfs (strcat filepath (now) ft))
;->Wblock entire drawing
  (setq se (getvar "expert"))
  (setvar "expert" 2)
  (command "wblock" bfs "*")
  (setvar "expert" se)
;->Report results to command line
  ;(prompt "Backup file")
  ;(terpri)
  ;(prompt (strcase (strcat bfs ".dwg")))
  ;(terpri)
  ;(prompt (strcat "created for drawing " (strcase dn) "."))
  (alert (strcat "Backup file " (strcase (strcat bfs ".dwg")) " created for drawing " (strcase dn) "."))
  (prin1)
)