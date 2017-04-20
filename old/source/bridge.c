/* c-rx.c, copyright (c) 2016 charles childers */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "nga.h"
CELL Dictionary, Heap, Compiler;
CELL notfound;
#define TIB 1471
CELL stack_pop() {
  sp--;
  return data[sp + 1];
}
void stack_push(CELL value) {
  sp++;
  data[sp] = value;
}
void string_inject(char *str, int buffer) {
  int m = strlen(str);
  int i = 0;
  while (m > 0) {
    memory[buffer + i] = (CELL)str[i];
    memory[buffer + i + 1] = 0;
    m--; i++;
  }
}
char string_data[8192];
char *string_extract(int at)
{
  CELL starting = at;
  CELL i = 0;
  while(memory[starting] && i < 8192)
    string_data[i++] = (char)memory[starting++];
  string_data[i] = 0;
  return (char *)string_data;
}
#define D_OFFSET_LINK  0
#define D_OFFSET_XT    1
#define D_OFFSET_CLASS 2
#define D_OFFSET_NAME  3
int d_link(CELL dt) {
  return dt + D_OFFSET_LINK;
}
int d_xt(CELL dt) {
  return dt + D_OFFSET_XT;
}
int d_class(CELL dt) {
  return dt + D_OFFSET_CLASS;
}
int d_name(CELL dt) {
  return dt + D_OFFSET_NAME;
}
int d_count_entries(CELL Dictionary) {
  CELL count = 0;
  CELL i = Dictionary;
  while (memory[i] != 0) {
    count++;
    i = memory[i];
  }
  return count;
}
int d_lookup(CELL Dictionary, char *name) {
  CELL dt = 0;
  CELL i = Dictionary;
  char *dname;
  while (memory[i] != 0 && i != 0) {
    dname = string_extract(d_name(i));
    if (strcmp(dname, name) == 0) {
      dt = i;
      i = 0;
    } else {
      i = memory[i];
    }
  }
  return dt;
}
CELL d_xt_for(char *Name, CELL Dictionary) {
  return memory[d_xt(d_lookup(Dictionary, Name))];
}
CELL d_class_for(char *Name, CELL Dictionary) {
  return memory[d_class(d_lookup(Dictionary, Name))];
}
void execute(int cell) {
  CELL opcode;
  rp = 1;
  ip = cell;
  while (ip < IMAGE_SIZE) {
    if (ip == notfound) {
      printf("%s ?\n", string_extract(TIB));
    }
    opcode = memory[ip];
    if (ngaValidatePackedOpcodes(opcode) != 0) {
      ngaProcessPackedOpcodes(opcode);
    } else if (opcode >= 0 && opcode < 27) {
      ngaProcessOpcode(opcode);
    } else {
      switch (opcode) {
        case 1000: printf("%c", data[sp]); sp--; break;
        case 1001: stack_push(getc(stdin)); break;
        default:   printf("Invalid instruction!\n");
                   printf("At %d, opcode %d\n", ip, opcode);
                   exit(1);
      }
    }
    ip++;
    if (rp == 0)
      ip = IMAGE_SIZE;
  }
}
void update_rx() {
  Dictionary = memory[2];
  Heap = memory[3];
  Compiler = d_xt_for("Compiler", Dictionary);
  notfound = d_xt_for("err:notfound", Dictionary);
}
void evaluate(char *s) {
  if (strlen(s) == 0)
    return;
  update_rx();
  CELL interpret = d_xt_for("interpret", Dictionary);
  string_inject(s, TIB);
  stack_push(TIB);
  execute(interpret);
}
void read_token(FILE *file, char *token_buffer) {
  int ch = getc(file);
  int count = 0;
  while ((ch != '\n') && (ch != ' ') && (ch != EOF))
  {
    token_buffer[count++] = ch;
    ch = getc(file);
  }
  token_buffer[count] = '\0';
}
