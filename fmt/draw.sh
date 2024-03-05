#!/bin/bash

gfortran main.f draw.f fun.f
./a.out
rm -rf a.out
cat out.txt

