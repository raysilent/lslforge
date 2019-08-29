@echo off

echo.
echo ---- Compiling Haskell...
echo.
cd haskell
stack install --executable-stripping

cd ..

echo.
echo ---- Generating Java Code...
echo.
call codegen.bat

echo.
echo ---- Copying Haskell Executable...
echo.
call copy_win32.bat

rem timeout /t 60
