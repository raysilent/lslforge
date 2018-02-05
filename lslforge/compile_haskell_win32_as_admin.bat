@echo off

whoami /groups | find "S-1-16-12288" > nul

if %errorlevel% == 0 (
  cd haskell
  stack install
  cd ..
  codegen.bat
  timeout /t 60
) else (
  echo. 
  echo You are NOT Administrator. Exiting...
  echo. 
  timeout /t 60
)
