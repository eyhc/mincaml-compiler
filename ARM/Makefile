export PATH := /opt/gnu/arm/bin:$(PATH)

CC=arm-linux-gnueabi-gcc

ASM=$(wildcard *.s)
PRG=$(subst .s,.arm,${ASM})

export QEMU_LD_PREFIX=/usr/arm-linux-gnueabi/

all: ${PRG}

echo:
	echo ${ASM}
	echo ${PRG}

%.arm : %.s libmincaml.S
	${CC} -o $@ $^ -lm

test: ${PRG}
	@for p in ${PRG} ; do \
	  echo "*** Launching $$p"; \
	  qemu-arm ./$$p ; \
	  echo "================" ; \
	done

