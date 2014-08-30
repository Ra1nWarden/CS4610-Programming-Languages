#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  int maxlength = 60;
  int initialsize = 5;
  char** readList = (char**) malloc(sizeof(char**) * initialsize);
  int index = 0;
  while(1) {
    char* line = (char*) malloc(maxlength+1);
    int status = getline(&line, &maxlength, stdin);
    if(status == -1)
      break;
    if(index == initialsize) {
      printf("size of the array is now %d", sizeof(readList));
      readList = realloc(readList, (sizeof(char**) * initialsize * 2));
      initialsize *= 2;
    }
    readList[index] = (char*) malloc(maxlength+1);
    strncpy(readList[index], line, maxlength+1);
    index++;
  }
  for(int i = 0; i < index; i ++)
    printf("The string at index %d is %s", i, readList[i]);
}
