#!/bin/sh

pwd
export QEMU_LD_PREFIX=/usr/arm-linux-gnueabi/

./ocaml/mincamlc $1 -o ./ARM/prog.s

arm-linux-gnueabi-gcc ./ARM/prog.s ./ARM/libmincaml.S -o prog.arm -lm
qemu-arm prog.arm

echo "\n"

rm prog.arm ./ARM/prog.s