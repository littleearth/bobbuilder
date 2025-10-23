@echo off
if [%1]==[] goto usage
if [%2]==[] goto usage
if [%3]==[] goto usage

set folder=%1
set platform=%2
set config=%3

call ..\..\..\resources\copy-resources.cmd client %folder% %platform% %config%
IF NOT ERRORLEVEL 0 GOTO ERROR
call ..\..\..\resources\copy-library.cmd mysql %folder%
IF NOT ERRORLEVEL 0 GOTO ERROR
call ..\..\..\resources\copy-library.cmd 7zip %folder%
IF NOT ERRORLEVEL 0 GOTO ERROR
call ..\..\..\resources\copy-library.cmd hunspell %folder%
IF NOT ERRORLEVEL 0 GOTO ERROR
call ..\..\..\resources\copy-library.cmd freeimage %folder%
IF NOT ERRORLEVEL 0 GOTO ERROR
goto exit

:usage
echo.
echo Incorrect parameters
echo Configure Delphi Post Build Events
echo post-build.cmd "$(OutputDir)" $(Platform) $(Config)
goto error


:error
EXIT /B 1

:exit