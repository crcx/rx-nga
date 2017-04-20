# Retro Block Editor

This is one part of the Retro Block Editor. See *Editor.md* for the other portion.

The implementation is split into two parts: an *interface layer*, written in C, and the editor logic, which is written in Retro. The code here contains the editor logic including key handling.

This has been influenced by Sam Falvo II's VIBE. Key handlers are words in the dictionary. The interface layer will lookup names and call the words to handle each keystroke. Naming conventions for the key handlers are as follows:

| name   | used for             |
| ------ | -------------------- |
| ed:c_? | keys in command mode |
| ed:i_? | keys in insert mode  |

Replace the *?* with the ASCII symbol for the key you are handling.

## Variables

There are a few global variables. These will hold the few pieces of state that we need.

````
#326144 'BLOCKS const
#383 'LASTBLOCK const
:ed:Current `0 ;
:ed:Row `0 ;
:ed:Col `0 ;
:ed:Mode `0 ;
````

## Constraints

The cursor position (determined by **ed:Row** and **ed:Col**) needs to be within the block coordinates. Blocks are 8 rows of 64 characters. Additionally, the current block (**ed:Current**) needs to be in the range of valid blocks so we can avoid memory corruption. This word is used to keep these values within the necessary boundaries. Any words that modify these bits of state should call this to keep things orderly.

````
:ed:wrap (-)
  &ed:Col fetch #63 gt? [  #0 &ed:Col store &ed:Row v:inc ] if
  &ed:Col fetch  #0 lt? [ #63 &ed:Col store &ed:Row v:dec ] if
  &ed:Row fetch  #7 gt? [  #0 &ed:Row store  #0 &ed:Col store ] if
  &ed:Row fetch  #0 lt? [  #7 &ed:Row store #63 &ed:Col store ] if ;

:ed:constrain
  ed:wrap
  &ed:Row #0 #7 v:limit
  &ed:Col #0 #63 v:limit
  &ed:Current #0 LASTBLOCK n:dec v:limit ;
````

## Commands

The editor has two modes: command and insertion. These words switch between them.

````
:ed:command-mode #0 &ed:Mode store ;
:ed:insert-mode  #1 &ed:Mode store ;
:ed:quit-mode    #2 &ed:Mode store ;
````

**ed:Current** contains the current block. These commands allow switching between blocks.

````
:ed:next-block   &ed:Current v:inc ed:constrain ;
:ed:prior-block  &ed:Current v:dec ed:constrain ;
````

The cursor (insertion point) is determined by the **ed:Row** and **ed:Col** variables. The next group of commands move the cursor.

````
:ed:cursor-up    &ed:Row v:dec ed:constrain ;
:ed:cursor-down  &ed:Row v:inc ed:constrain ;
:ed:cursor-left  &ed:Col v:dec ed:constrain ;
:ed:cursor-right &ed:Col v:inc ed:constrain ;
````

## Input

````
:ed:index (-a)
  &ed:Current fetch #512 *
  &ed:Row fetch #64 * +
  &ed:Col fetch + ;

:ed:control
   #8 [ ed:cursor-left ] case
  #10 [ #0 &ed:Col store &ed:Row v:inc ] case
  #13 [ #0 &ed:Col store &ed:Row v:inc ] case
 #127 [ ed:cursor-left ] case
 drop ;

:ed:insert-char
  dup chr:visible?
  [ ed:index BLOCKS + store ed:cursor-right ]
  [ ed:control ed:constrain ] choose ;
````

## Block Evaluation

The strategy here:

* Copy a token (ending with a space or end of block) to a buffer
* Interpret the buffer
* Repeat until the end of the block

````
{{
  :TIB #1471 ;
  :ed:End `0 ;
  :ed:Count `0 ;

  :getc
    BLOCKS
    &ed:Current fetch #512 * +
    &ed:End fetch +
    fetch
    &ed:End v:inc ;

  :append   (c-)  &ed:Count fetch TIB + store &ed:Count v:inc ;
  :-empty? (s-sf) dup str:length n:positive? ;
  :-end?    (-f)  &ed:End fetch #512 lt? ;
---reveal---
  :ed:token
    #0 &ed:Count store
    [ getc dup append #32 -eq? -end? and ] while
    #0 TIB &ed:Count fetch + n:dec store TIB ;
  :ed:evaluate-block
    #0 &ed:End store
    [ ed:token -empty? [ interpret ] [ drop ] choose -end? ] while ;
}}
````

## Key Handlers

### Command Mode

Command mode has a number of handlers since it's where most interactions take place.

| Key | Usage                           |
| --- | ------------------------------- |
| [   | switch to prior block           |
| ]   | switch to next block            |
| h   | move cursor left                |
| j   | move cusor down                 |
| k   | move cursor up                  |
| l   | move cursor right               |
| H   | move cursor to leftmost column  |
| J   | move cursor to last row         |
| K   | move cursor to top row          |
| L   | move cursor to rightmost column |
| q   | exit the editor                 |
| e   | evaluate the block              |
| \   | switch to insert mode           |

````
:ed:c_[ (-) ed:prior-block ;
:ed:c_] (-) ed:next-block ;
:ed:c_h (-) ed:cursor-left ;
:ed:c_j (-) ed:cursor-down ;
:ed:c_k (-) ed:cursor-up ;
:ed:c_l (-) ed:cursor-right ;
:ed:c_\ (-) ed:insert-mode ;
:ed:c_q (-) ed:quit-mode ;
:ed:c_e (-) ed:evaluate-block ed:constrain ;
:ed:c_H (-) #0 &ed:Col store ;
:ed:c_J (-) #7 &ed:Row store ;
:ed:c_K (-) #0 &ed:Row store ;
:ed:c_L (-) #63 &ed:Col store ;
````

````
:ed:c_# (n-)
  $# ed:insert-char
  n:to-string
  dup str:length [ fetch-next ed:insert-char ] times drop
  ed:cursor-right ;
:ed:c_$ (c-) $$ ed:insert-char ed:insert-char ed:cursor-right ;
:ed:c_' (s-)
  $' ed:insert-char
  dup str:length [ fetch-next ed:insert-char ] times drop
  ed:cursor-right ;
````

### Insertion Mode

This is intentionally kept to a minimal list as I don't want to restrict what can be input. Just one command, **\**, which returns to command mode.

| Key | Usage                  |
| --- | ---------------------- |
| \   | switch to insert mode  |

````
:ed:i_\ ed:command-mode ;
````


````
(------------------------------)
:nl chr:LF putc ;
:puts     (a-) [ repeat fetch-next 0; putc again ] call drop ;
:puts:n  (an-) [ fetch-next putc ] times drop ;
:putn     (n-) n:to-string puts #32 putc ;

:Count `0 ;
:line# &Count fetch dup #10 lt? [ #32 putc ] if putn &Count v:inc ;
(------------------------------)
:row (a-a)
 #32 [ fetch-next putc ] times ;
:[block]
 #0 &Count store
 &ed:Current fetch #512 * BLOCKS +
 #16 [ $| putc row nl ] times drop ;
:v nl --- [block] --- ;

(------------------------------)
:ed:line (n-a)
  #32 *
  &ed:Current fetch #512 *
  BLOCKS + + ;

:ia (slc-) swap ed:line + [ dup str:length ] dip swap copy v ;
:i  (sl-)  #0 ia ;
:ed:BLANK '                                ' ;
:el (l-)   ed:BLANK swap ed:line #32 copy v ;
:eb #0 #16 [ [ el ] sip n:inc ] times drop v ;
:ed:CB '                                ' ;
:cl (n-) ed:line ed:CB #32 copy v ;
:pl (n-) ed:CB swap ed:line #32 copy v ;
````


## The End


## Legalities

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

    Copyright (c) 2008 - 2016, Charles Childers
    Copyright (c) 2012 - 2013, Michal J Wallace
    Copyright (c) 2009 - 2011, Luke Parrish
    Copyright (c) 2009 - 2010, JGL
    Copyright (c) 2010 - 2011, Marc Simpson
    Copyright (c) 2011 - 2012, Oleksandr Kozachuk
    Copyright (c) 2010,        Jay Skeer
    Copyright (c) 2010,        Greg Copeland
    Copyright (c) 2011,        Aleksej Saushev
    Copyright (c) 2011,        Foucist
    Copyright (c) 2011,        Erturk Kocalar
    Copyright (c) 2011,        Kenneth Keating
    Copyright (c) 2011,        Ashley Feniello
    Copyright (c) 2011,        Peter Salvi
    Copyright (c) 2011,        Christian Kellermann
    Copyright (c) 2011,        Jorge Acereda
    Copyright (c) 2011,        Remy Moueza
    Copyright (c) 2012,        John M Harrison
    Copyright (c) 2012,        Todd Thomas

