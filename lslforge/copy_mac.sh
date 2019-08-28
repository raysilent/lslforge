#!/bin/sh
(cd haskell;stack install)
strip ~/.local/bin/LSLForge
cp ~/.local/bin/LSLForge eclipse/lslforge-macos-x86_64/os/macos/x86_64
