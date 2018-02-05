@echo on

cd haskell
stack install

cd ..
@echo off
ren eclipse\lslforge\src\lslforge\generated\Tuple2.java Tuple2.save
ren eclipse\lslforge\src\lslforge\generated\Tuple3.java Tuple3.save
del eclipse\lslforge\src\lslforge\generated\*.java
ren eclipse\lslforge\src\lslforge\generated\*.save *.java
@echo on
haskell\.stack-work\dist\ca59d0ab\build\LslForge\LslForge.exe _CodeGen_ eclipse\lslforge\src\lslforge\generated lslforge.generated

REM 
REM stack exec strip -v haskell\.stack-work\dist\ca59d0ab\build\LslForge\LslForge.exe
copy haskell\.stack-work\dist\ca59d0ab\build\LslForge\LslForge.exe eclipse\lslforge-win32-x86\os\win32\x86

eclipse\lslforge-win32-x86\os\win32\x86\LslForge.exe Version

timeout /t 60
