#!/bin/bash
set -v
./.stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/AppleContactsParse/AppleContactsParse
ls -l Arubala.json
jq . Arubala.json >Arubala.pretty.json

