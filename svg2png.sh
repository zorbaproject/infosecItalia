#!/bin/bash
for i in ./svg/*.svg; do
filename=${i##*/}
inkscape -z -w 2000  "./svg/$filename" -e "./png/${filename%.*}.png"
done
