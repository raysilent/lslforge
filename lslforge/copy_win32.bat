@echo off

cd haskell
rem stack exec strip %UserProfile%\AppData\Roaming\local\bin\LSLForge.exe
cd ..
xcopy /y %UserProfile%\AppData\Roaming\local\bin\LSLForge.exe eclipse\lslforge-win32-x86\os\win32\x86\

rem timeout /t 60
