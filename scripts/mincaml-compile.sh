#!/bin/sh
cd "$(dirname "$0")"/.. || exit 1

export QEMU_LD_PREFIX=/usr/arm-linux-gnueabi/

# create the assembler file .s
./ocaml/mincamlc $1 -o ./ARM/prog.s

# compile the assembler file into an arm file with libC
arm-linux-gnueabi-gcc ./ARM/prog.s ./ARM/libmincaml.S -o prog.arm -mfpu=fpv5-d16 -lm

# execute the arm file with qemu-arm
qemu-arm prog.arm

echo "\n"

# clean the generated arm and assembler files
rm prog.arm ./ARM/prog.s