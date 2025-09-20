#!/bin/bash

for f in *.svg; do
    name=${f%.svg}
    rsvg-convert -f pdf -o "$name.pdf" "$name.svg"
    #inkscape "$name.svg" --export-type=pdf --export-filename="$name.pdf"
done