# Ngita-Rx: an interface to Rx

*Rx* (*retro experimental*) is a minimal Forth implementation for the Nga virtual machine. Like Nga this is intended to be used within a larger supporting framework adding I/O and other desired functionality.

This is Ngita-Rx, an interface layer for the Ngita interface layer.

## General Notes on the Source

Rx is developed using a literate tool called *unu*. This allows extraction of fenced code blocks into a separate file for later compilation. Developing in a literate approach is beneficial as it makes it easier for me to keep documentation current and lets me approach the code in a more structured manner.

The code is written in *nuance*. Nuance is a small preprocessor that converts a simple Forth-style language into assembly. I chose to do it this way to reduce the time needed to build and maintain this. Generally the syntax of Nuance is similar to the Rx language, though it adds some useful things like support for forward references.

Nuance generates assembly language. This is built with *naje*, the standard Nga assembler.

The entire process of using *unu*, *nuance*, and *naje* to build an image for Nga takes around 0.1s on my Linode.

## In the Beginning..

## Nga Instruction Set

````
:bye   "-"      _end ;
````

## Dictionary

The dictionary is a simple linked list, with the following format.

| field | holds                                       |
| ----- | ------------------------------------------- |
| link  | link to the previous entry, 0 if last entry |
| xt    | link to start of the function               |
| class | link to the class handler function          |
| name  | zero terminated string                      |

The initial dictionary is constructed at the end of this file. It'll take a form like this:

    :0000  `0    |+     |.word '+'
    :0001  |0000 |-     |.word '-'

````
:run-once
  ^patch
:patch
  &_nop &run-once #1 + store
  &ngita-notfound &err:notfound #1 + store
  &Dictionary fetch &0900 store
  &0908 &Dictionary store
  ;

:ngita-notfound $? putc ;
````

````
:startup
  'rx-2016.09'

:okmsg
  'ok'

:ok &okmsg puts space ;

:main
  run-once
  &startup puts cr cr 
:main_loop
  compiling? [ ok ] -if getToken
  &TIB interpret
  compiling? [ cr ] -if
  ^main_loop
````

## Dictionary

The dictionary is a linked list.


````
:0900 |9999 |putc          |.word  'putc'
:0901 |0900 |putn          |.word  'putn'
:0902 |0901 |puts          |.word  'puts'
:0903 |0902 |cls           |.word  'cls'
:0904 |0903 |getc          |.word  'getc'
:0905 |0904 |getn          |.word  'getn'
:0906 |0905 |gets          |.word  'gets'
:0907 |0906 |save          |.word  'save'
:0908 |0907 |bye           |.word  'bye'
````

## Ngura I/O

Rx does not attempt to become a complete Forth. As with Nga, I/O is generally left undefined. For testing purposes it is helpful though, so the following wraps the reference *Ngura* I/O instructions into functions.

````
:putc `100 ;
:putn `101 ;
:puts `102 ;
:putsc `103 ;
:cls `104 ;
:getc `110 ;
:getn #32 `111 ;
:gets `112 ;
:fs.open `118 ;
:fs.close `119 ;
:fs.read `120 ;
:fs.write `121 ;
:fs.tell `122 ;
:fs.seek `123 ;
:fs.size `124 ;
:fs.delete `125 ;
:save `130 ;
````

With this, we can build in some interactivity around a terminal I/O model.

````
:cr #10 putc ;
:space #32 putc ;
:getToken #32 #2048 &TIB gets ;
:TIB `4096
````
