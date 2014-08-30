#include <stdio.h>

int main() {
  int maxlength = 61;
  char* destline = malloc(maxlength);
  char* sourceline = malloc(maxlength);
  getline(&destline, &maxlength, stdin);
  getline(&sourceline, &maxlength, stdin);
  printf("read is source %s and dest is %s", sourceline, destline);
}
