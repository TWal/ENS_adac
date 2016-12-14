#!/bin/bash
./dist/build/adac/adac --full $1 > /tmp/toto.s
gcc /tmp/toto.s -o /tmp/toto
/tmp/toto
