UNU = ../../bin/unu
CC = clang
#CC = gcc
CFLAGS = -Wall -DTERMIOS

#EXT = .exe
#CFLAGS = -Wall -WINDOWS

all: s o l c

s:
	$(UNU) TerminalIO.md > TerminalIO.c
	$(UNU) TerminalIO-Headers.md > TerminalIO.h
	$(UNU) Listener.md > listener.c
	$(UNU) Editor.md > editor.c
	$(UNU) Extend.md > extend.c
	$(UNU) Bridge.md > bridge.c
	cp ../../source/nga.c .
	cp ../../source/nga.h .

o:
	$(CC) $(CFLAGS) -c nga.c -o nga.o
	$(CC) $(CFLAGS) -c TerminalIO.c -o TerminalIO.o
	$(CC) $(CFLAGS) -c listener.c -o listener.o

l:
	$(CC) $(CFLAGS) nga.o TerminalIO.o listener.o -o listener$(EXT)

c:
	rm *.c *.h *.o
