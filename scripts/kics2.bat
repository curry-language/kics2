@echo off
REM Batch file to call KiCS2

REM The installation directory of KiCS2
set KICS2HOME=`echo KICS2HOME must be defined here!`

REM Set 'kics2' executable
set KICS2=%KICS2HOME%\bin\.local\kics2i.exe

%KICS2% %*
