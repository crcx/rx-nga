# Terminal I/O for Listener

This is a source module intended to help abstract the terminal implementation details away from the main Listener source.

````
#include "TerminalIO.h"
````


````
#ifdef TERMIOS
struct termios new_termios, old_termios;

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

void term_clear() {
  printf("\033[2J\033[1;1H");
}

void term_move_cursor(int x, int y) {
  printf("\033[%d;%dH", y, x);
}
#endif
````

````
#ifdef _WIN32
HANDLE hStdIn;
DWORD fdwMode, fdwOldMode;

void term_setup() {
  hStdIn = GetStdHandle(STD_INPUT_HANDLE);
  GetConsoleMode(hStdIn, &fdwOldMode);
  SetConsoleTitle("RETRO 12 - Windows");
  fdwMode = fdwOldMode ^ ENABLE_LINE_INPUT;
  SetConsoleMode(hStdIn, fdwMode);
}

void term_cleanup() {
  SetConsoleMode(hStdIn, fdwMode);
}

void term_clear() {
}

void term_move_cursor(int x, int y) {
}
#endif
````
