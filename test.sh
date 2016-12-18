#!/usr/bin/env bash

shopt -s nullglob

# script de test pour le projet de compilation

score=0
score_comp=0
score_out=0
score_test=0
max=0

for f in mytests/*.adb; do
    echo -n "."
    asm=/tmp/toto.s
    rm -f $asm
    expected=mytests/`basename $f .adb`.out
    max=`expr $max + 1`;
    if ./dist/build/adac/adac --full $f > $asm; then
        rm -f /tmp/out
        score_comp=`expr $score_comp + 1`;
        if gcc $asm -o /tmp/toto && /tmp/toto > /tmp/out; then
            score_out=`expr $score_out + 1`;
            if cmp --quiet /tmp/out $expected; then
                score_test=`expr $score_test + 1`;
            else
                echo
                echo "ECHEC : mauvaise sortie pour $f :"
                cat /tmp/out
            fi
        else
            echo
            echo "ECHEC du code produit pour $f"
        fi
    else
        echo
        echo "ECHEC de la compilation sur $f (devrait réussir)"
    fi
done
echo

echo
percent=`expr 100 \* $score / $max`;

echo "Compilation:";
percent=`expr 100 \* $score_comp / $max`;
echo "Compilation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Code produit : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Comportement du code : $score_test/$max : $percent%"


