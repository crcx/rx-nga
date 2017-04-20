# Terminal I/O for Listener

Headers for TerminalIO.c

````
#ifdef TERMIOS
#include <stdio.h>
#include <termios.h>
#include <sys/ioctl.h>
#endif

#ifdef _WIN32
#include <windows.h>
#endif

void term_setup();
void term_cleanup();
void term_clear();
void term_move_cursor(int x, int y);
