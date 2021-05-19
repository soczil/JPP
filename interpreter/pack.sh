#!/bin/bash

mkdir Karol_Soczewica
cp -r bad good Lattepp.cf Interpreter.hs Typechecker.hs Main.hs Makefile README.md Karol_Soczewica
find ./Karol_Soczewica -name ".DS_Store" -print -delete
zip -r Karol_Soczewica.zip Karol_Soczewica
rm -r -f Karol_Soczewica
