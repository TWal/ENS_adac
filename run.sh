#!/bin/bash
asm=`dirname $1`/`basename $1 .adb`.s
./dist/build/adac/adac --O1 $1
gcc $asm -o /tmp/toto
/tmp/toto
