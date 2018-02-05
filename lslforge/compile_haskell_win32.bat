@echo off

cd haskell
stack install
cd ..
codegen.bat
timeout /t 60
