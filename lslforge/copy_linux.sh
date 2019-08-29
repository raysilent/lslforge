#!/bin/sh
(cd haskell;stack install)
# strip haskell/dist/build/LslForge/LSLForge
strip ~/.local/bin/LSLForge
cp ~/.local/bin/LSLForge eclipse/lslforge-linux-x86/os/linux/x86
