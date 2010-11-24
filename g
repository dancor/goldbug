#!/bin/sh
set -e
if [[ ! -f src/ZobTable.hs ]]
then
  cd src
  runghc genZobTable.hs
  cd ..
fi
cabal install --global --root-cmd=sudo --enable-documentation -pO2
echo running
goldbug "$@"
