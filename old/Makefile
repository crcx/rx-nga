CC = clang
#CC = gcc
CFLAGS = -Wall

# Uncomment for Windows
#EXT = .exe

all: clean tools sources compile link core image

clean:
	rm -f bin/*.o

tools:
	cd source && $(CC) $(CFLAGS) unu.c -o ../bin/unu$(EXT)
	cd source && $(CC) $(CFLAGS) nga.c -DSTANDALONE -o ../bin/nga$(EXT)
#	cd source && $(CC) $(CFLAGS) -DVERBOSE ngita.c -o ../bin/ngita$(EXT)
	cd source && $(CC) $(CFLAGS) naje.c -DDEBUG -DALLOW_FORWARD_REFS -DENABLE_MAP -o ../bin/naje$(EXT)

sources:
#	./bin/unu Bridge.c.md > source/bridge.c
#	./bin/unu Extend.md > source/extend.c
#	./bin/unu Listener.md > source/listener.c
	./bin/unu RetroForth.md > retro.forth

compile:
	cd source && $(CC) $(CFLAGS) -c nga.c -o nga.o
#	cd source && $(CC) $(CFLAGS) -c listener.c -o listener.o
	cd source && $(CC) $(CFLAGS) -c extend.c -o extend.o
	cd source && $(CC) $(CFLAGS) -c embedimage.c -o embedimage.o
	mv source/*.o bin

link:
#	cd bin && $(CC) nga.o listener.o -o listener$(EXT)
	cd bin && $(CC) nga.o extend.o -o extend$(EXT)
	cd bin && $(CC) embedimage.o -o embedimage$(EXT)

core:
	./bin/unu Rx.md > rx.naje
	./bin/naje rx.naje > rx.log

image:
	./bin/extend retro.forth

#editor: editorbin editorimage editorclean

#editorbin:
#	./bin/unu future/EditorForth.md > editor.forth
#	./bin/unu future/Editor.md > editor.c
#	$(CC) editor.c -o editor.o -c
#	$(CC) editor.o bin/nga.o -o bin/editor$(EXT)

#editorimage:
#	cp ngaImage _1
#	./bin/extend editor.forth
#	cp ngaImage ngaImage+editor
#	mv _1 ngaImage

#editorclean:
#	rm editor.forth editor.c editor.o
