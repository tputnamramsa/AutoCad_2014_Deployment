rem At the command prompt
rem Type DGNHotfix compuertname
rem Example DGNhotfix EXT370
rem @echo off
Set ext=%1
Copy /Y "\\ramsacfs\Project\Reference\AutoCAD\2014\Support\DGN_Hotfix\AcDgnLS.dbx" "\\%ext%\C$\Program Files\Autodesk\AutoCAD 2014
Copy /Y "\\ramsacfs\Project\Reference\AutoCAD\2014\Support\DGN_Hotfix\DgnLsPurge.dll" "\\%ext%\C$\Program Files\Autodesk\AutoCAD 2014
