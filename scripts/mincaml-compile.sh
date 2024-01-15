#!/bin/sh

pwd

./ocaml/mincamlc $1 -o ./ARM/prog.s

arm-linux-gnueabi-as ./ARM/prog.s ./ARM/libmincaml.S -o prog.o

arm-linux-gnueabi-ld prog.o -o prog.arm

qemu-arm prog.arm

echo "\n"

rm prog.o prog.arm ./ARM/prog.s