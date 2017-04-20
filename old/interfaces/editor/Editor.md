    ____   ____ ______ ____    ___
    || \\ ||    | || | || \\  // \\
    ||_// ||==    ||   ||_// ((   ))
    || \\ ||___   ||   || \\  \\_//
    a personal, minimalistic forth

# Block Editor

This is one part of the Retro Block Editor. See the **ed:** namespace in *RetroForth.md* for the other portion.

The implementation is split into two parts: an *interface layer*, written in C, and the editor logic, which is written in Retro. The code here contains the interface layer.

## Implementation Overview

Blocks are 512 cells in length. They are displayed as 8 rows with 64 cells per row.

## Standard Headers

````
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
````

## Rx &amp; Nga

````
#include "source/bridge.c"
````

## Configuration

These values need to match the ones in *RetroForth.md*.

````
#define ED_BUFFER 326144
#define ED_BLOCKS 384
````

## Terminal

This interface assumes a VT100-style terminal emulation. The interface is wrapped into a few simple functions. The default is to use *termios* and *ioctl*, though this limits portability. It'd be better to use *curses* instead, but that's a little more complex. Maybe at a later time...

````
#ifdef _WIN32
#include "termios.h"
int	tcgetattr(int _fildes, struct termios *_termios_p) {return 0;};
int	tcsetattr(int _fildes, int _optional_actions, const struct termios *_termios_p) {return 0;};
#include "ioctl.h"
#else
#include <termios.h>
#include <sys/ioctl.h>
#endif

struct termios new_termios, old_termios;
````

Setup and restoration of the terminal environment. To work properly we need:

* non-buffered input
* no keyboard echo

The **term_setup()** turns these on while **term_cleanup()** resets to the prior state.

````
void term_setup() {
  tcgetattr(0, &old_termios);
  new_termios = old_termios;
  new_termios.c_iflag &= ~(BRKINT+ISTRIP+IXON+IXOFF);
  new_termios.c_iflag |= (IGNBRK+IGNPAR);
  new_termios.c_lflag &= ~(ICANON+ISIG+IEXTEN+ECHO);
  new_termios.c_cc[VMIN] = 1;
  new_termios.c_cc[VTIME] = 0;
  tcsetattr(0, TCSANOW, &new_termios);
}

void term_cleanup() {
  tcsetattr(0, TCSANOW, &old_termios);
}
````

````
void term_clear() {
  printf("\033[2J\033[1;1H");
}

void term_move_cursor(int x, int y) {
  printf("\033[%d;%dH", y, x);
}
````

## Editor State

The editor state is handled by the Retro portion of the code. The interface layer does need to be aware of it though so this defines a few global variables and a function for keeping them in sync with the underlying code.

````
CELL Current;
CELL Column;
CELL Row;
CELL Mode;

void update_state() {
  update_rx();
  Current = memory[d_xt_for("ed:Current", Dictionary)];
  Column = memory[d_xt_for("ed:Col", Dictionary)];
  Row = memory[d_xt_for("ed:Row", Dictionary)];
  Mode = memory[d_xt_for("ed:Mode", Dictionary)];
}
````

## Display a Block

These functions are used to display a block.

````
void sep() {
  for (int i = 0; i < 8; i++)
    printf("--------");
  printf("\n");
}

void row(int block, int n) {
  int start = (block * 512) + (n * 64);
  for (int i = 0; i < 64; i++)
    printf("%c", (char) (memory[ED_BUFFER + start + i] & 0xFF));
  printf("\n");
}

void stats() {
  printf("Free: %d | Heap: %d | ", 326140 - Heap, Heap);
  printf("%d : %d : %d | %c\n", Current, Row, Column, (Mode ? 'I' : 'C'));
}

void block_display(int n) {
  for (int line = 0; line < 8; line++)
    row(n, line);
  sep();
  stats();
}
````

## Unsorted

````
void red_enter(int ch) {
  stack_push(ch);
  evaluate("ed:insert-char");
}

void display_stack() {
  for (CELL i = 1; i <= sp; i++)
    (i == sp) ? printf("< %d >", data[i]) : printf("%d ", data[i]);
  printf("\n");
}
````

````
void save() {
  FILE *fp;
  memory[d_xt_for("ed:Mode", Dictionary)] = 0;
  if ((fp = fopen("ngaImage+editor", "wb")) == NULL) {
    printf("Unable to save the ngaImage!\n");
    exit(2);
  }
  fwrite(&memory, sizeof(CELL), IMAGE_SIZE, fp);
  fclose(fp);
}
````

## Block Storage

The editor keeps copies of the blocks in the block buffer. It also saves/reads them from mass storage (a file named *retro.blocks* in this implementation) for long term persistance and easier updating when switching to a new image.

````
void write_BUFFER() {
  FILE *fp;
  if ((fp = fopen("retro.blocks", "wb")) != NULL) {
    CELL slot;
    for(int i = ED_BUFFER; i < IMAGE_SIZE; i++) {
      slot = memory[i];
      fwrite(&slot, sizeof(CELL), 1, fp);
    }
    fclose(fp);
  }
}
````

**read_blocks()** copies the block data into the Retro block buffer.

````
void read_blocks() {
  FILE *fp;
  if ((fp = fopen("retro.blocks", "rb")) != NULL) {
    CELL slot;
    for (int i = ED_BUFFER; i < IMAGE_SIZE; i++) {
      fread(&slot, sizeof(CELL), 1, fp);
      memory[i] = slot;
    }
    fclose(fp);
  }
}


void initialize_rx() {
  ngaPrepare();
  ngaLoadImage("ngaImage+editor");
  read_blocks();
  update_state();
}

int main() {
  initialize_rx();
  term_setup();
  int ch;
  char c[] = "ed:c_?";
  char i[] = "ed:i_?";
  while (1) {
    update_state();
    term_clear();
    block_display(Current);
    display_stack();
    term_move_cursor(Column + 1, Row + 1);
    ch = getchar();
    if (Mode == 0) {
      c[5] = ch;
      CELL dt = d_lookup(Dictionary, c);
      if (dt != 0) execute(memory[d_xt(dt)]);
    } else if (Mode == 1) {
      i[5] = ch;
      CELL dt = d_lookup(Dictionary, i);
      (dt != 0) ? execute(memory[d_xt(dt)]) : red_enter(ch);
    }
    update_state();
    if (Mode == 2) {
      term_move_cursor(1, 15);
      term_cleanup();
      save();
      write_BUFFER();
      exit(0);
    }
  }
  return 0;
}
````
