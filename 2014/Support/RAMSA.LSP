;;Set plot offset to offset from edge of paper, not edge of printable area
(setvar "plotoffset" 1)

;;Set savetime to 15 minutes - prevent it from constantly resetting to 0
(setvar "savetime" 15)

;;Disable background plotting
(setvar "backgroundplot" 2)

;;Set the defaut save format
(setenv "DefaultFormatForSave" "60") ;48=2010 60=2013

(prompt "\nramsa.lsp run 2014")
(princ)
