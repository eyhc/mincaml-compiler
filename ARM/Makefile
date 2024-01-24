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
	${CC} -o $@ $^ -mfpu=fpv5-d16 -lm

test: ${PRG}
	@for p in ${PRG} ; do \
	  echo "*** Launching $$p"; \
	  qemu-arm ./$$p ; \
	  echo "================" ; \
	done

