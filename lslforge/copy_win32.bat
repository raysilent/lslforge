@echo off

whoami /groups | find "S-1-16-12288" > nul

if %errorlevel% == 0 (
  stack exec strip haskell\.stack-work\dist\ca59d0ab\build\LslForge\LslForge.exe
  copy haskell\.stack-work\dist\ca59d0ab\build\LslForge\LslForge.exe eclipse\lslforge-win32-x86\os\win32\x86
  timeout /t 60
) else (
  echo. 
  echo You are NOT Administrator. Exiting...
  echo. 
  timeout /t 60
)

