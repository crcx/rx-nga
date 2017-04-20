# RETRO 12

Hello, and welcome to RETRO!

## The Interface

The user interface is kept simple.

You have a personal workspace, shown on the left. This contains your code and documentation. It's saved automatically as you work.

To the right is the Output area. This will show the results of running the code in your workspace.

Above Output is the toolbar. There are buttons for making and reloading a snapshot of the workspace, clearing the Output, and running code (Go). There's also a Glossary button that'll provide a short overview of the basic functions understood by RETRO.

The language is fully reset after each run, though output remains until manually cleared.

## The Workspace

This is the workspace. As mentioned earlier, it's where your code and documentation live. RETRO extracts code from within fenced blocks (starting and ending with four backticks: ````). Anything outside a fenced block is ignored when running code. Apart from this you can use any formatting you like for the documentation and commentary. I like Markdown, but you can feel free to use any approach you like.

## The Language

RETRO is a dialect of Forth. But it has many differences from traditional Forth dialects. This section will provide a short overview of the language. (It's assumed that you have some familiarity with Forth or similar languages)

RETRO code consists of tokens, separated by whitespace. Each token can have a single character prefix which tells RETRO how to deal with it. (Tokens without a prefix are assumed to be words which RETRO should run).

As an example, to add two numbers:

    #1 #2 +

The first token is #1. The # prefix tells RETRO to process the token as a number. #2 is also a number, and + is treated a word.

A bit of terminology:

- Words are named items (mostly functions)
- Word names are stored in a Dictionary

RETRO has several prefixes in the core language:

| Prefix | Used For                |
| ------ | ----------------------- |
| &amp;  | Pointers                |
| '      | Strings                 |
| #      | Numbers                 |
| $      | ASCII Characters        |
| :      | Starting a definition   |
| (      | Comments                |

Tokens may not contain spaces. Comments and strings will typically be one token in length:

    (these (are (separate (comments
    'these 'are 'separate 'strings

    (this_is_a_single_comment
    'this_is_a_single_string

Word definitions take a slightly different form from most Forths:

    :word  (stack-usage)  ... ;

RETRO has quotations (anonymous, first-class functions) which are used with combinators (words operating on quotations) for both stack and control flow.

Example:

    #10 [ 'hello puts ] times

    #2 mod [ 'even puts ] if
    #2 mod [ 'odd puts ] -if

    #2 mod [ 'even puts ]
           [ 'odd puts ] choose

    #1 #2 [ #3 + ] dip
    (stack_will_be:_#4_#2)

Words are loosely grouped into namespaces. These can be seen via a short prefix string, separated from the actual name by a colon. E.g.,

    'hello_world str:length
    &FileID fetch file:length

The initial namespaces are:

| Namespace | Related to             |
| --------- | ---------------------- |
| chr       | Characters             |
| compiler  | Compiler words         |
| d         | Dictionary             |
| err       | Error handlers         |
| file      | File I/O               |
| n         | Numbers                |
| str       | Strings                |
| v         | Variables              |

## Naming Conventions

RETRO has some naming conventions used in the core language. You are free to ignore most of these, but this should help as you look through the Glossary for hints about the language.

Mandatory:

- names may not contain spaces
- names may not start with a prefix character

Convention:

- word names corresponding to
  functions are lowercase
- names for constants are UPPERCASE
- names for variables are TitleCase
- use of a dash (-) to separate parts
  of a compound name are preferred
  for words and constants
- names may be prefixed by a namespace
  indicator to help associate them
  with related words
- an initial dash implies *not*:

    -if
    -zero?

## End of Overview

--------------------------------------

So with that introductory stuff out of the way, here's where you can begin exploring the language and building a personal environment for your own use. I'll include just a bit from the start of mine to provide a starting point:

--------------------------------------

## 2016.11-crc

Some output formatting stuff.

````
:sp chr:SPACE putc ;
:---- #30 [ $- putc ] times nl ;
````
--------------------------------------

**words** is a quick thing that displays the names of each word in the Dictionary.

````
:words (-)
  &Dictionary
  repeat
    fetch 0; dup d:name puts nl
  again ;
````

Looking at this, there's a decent possibility that a combinator for iterating over each element in a linked list (like the Dictionary) could be useful. One approach:

````
:ll:for-each (aq-?)
  [ repeat
    [ fetch ] dip
    over 0; drop
    dup-pair [ [ call ] dip ] dip
    again
  ] call
  drop-pair ;
````

With this, **words** could become much simpler:

    :words (-)
      &Dictionary
      [ d:name puts nl ] ll:for-each ;

--------------------------------------

# Files

The basic words are:

| word          | does               |
| ------------- | ------------------ |
| file:open     | open a file        |
| file:close    | close a file       |
| file:read     | read a byte        |
| file:write    | write a byte       |
| file:position | get current index  |
| file:seek     | set index          |
| file:length   | get size of file   |
| file:delete   | delete file        |

Example:

    'F var
    'test.txt $W file:open &F store
    'Hello_World
    [ &F fetch file:write ] str:for-each
    &F fetch file:close

    'test.txt file:delete

Modes:

| symbol | used for  |
| ------ | --------- |
| R      | read      |
| W      | write     |
| A      | append    |

Modes are character values, not strings.

----

A **for-each** for iterating over all of the files.

````
{{
  'Action var
---reveal---
  :file:for-each (q-)
    &Action store
    #1 file:count-files
    [ [ file:name-for-index
        &Action fetch call
      ] sip n:inc
    ] times drop ;
}}
````

With that **files:for-each**, a directory listing can be done easily:

````
:file:list-all (-)
  [ puts nl ] file:for-each ;
````

A word to find the size of a file (in bytes)

'filename file:size-of-named

````
:file:size-of-named (s-n)
  $R file:open dup file:length swap
  file:close ;
````

This one writes a string to a file.

    'Hello,_World! 'hello.txt file:spew

````
:file:spew (ss-)
  $W file:open swap
  [ over file:write ] str:for-each
  file:close ;
````

Make a copy of a file.

    'source 'dest file:copy

````
:file:copy (ss-)
  swap $R file:open (src)
  swap $W file:open (dest)
  over file:length
  [ over file:read
    over file:write
  ] times file:close file:close ;
````

--------------------------------------
