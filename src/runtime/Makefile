# Makefile for ML Lite runtime
#
# CMSC 22600 --- Compilers for Computer Languages
# Autumn 2021
# University of Chicago
#
# COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
# All rights reserved.
#

SHELL		= /bin/sh
CC		= gcc -std=gnu99 -m64
CPPFLAGS	=
CFLAGS		= -g -O2

C_SRCS		= main.c basis.c gc.c statepoints.c
OBJS		= $(patsubst %.c,%.o,$(C_SRCS))

mll-rt.o:	$(OBJS)
	$(LD) -r -o mll-rt.o $(OBJS)

main.o:		main.c mllite.h
basis.o:	basis.c mllite.h
gc.o:		statepoints.c mllite.h statepoints.h
statepoints.o:	statepoints.c statepoints.h

.PHONY:		clean
clean:
		rm -f *.o
