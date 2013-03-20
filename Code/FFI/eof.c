#include <stdio.h>

int eof_stdin() {
  return feof(stdin);
}

int flush_stdout() {
  return fflush(stdout);
}
