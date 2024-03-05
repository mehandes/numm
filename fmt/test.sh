#!/bin/bash

gfortran test.f fun.f
./a.out
rm -rf a.out

