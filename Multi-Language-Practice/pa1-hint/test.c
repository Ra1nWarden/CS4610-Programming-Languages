#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct sourceList sourceList;

struct sourceList {
  char** name;
  sourceList* next;
};

typedef struct node {
  char** name;
  sourceList* sources;
} node;

int main() {
  node* myGraph;
  while(true) {
    size_t maxLength = 61;
    char* line = (char*) malloc(maxLength);
    getline(&line, &maxLength, stdin);
    if(line == EOF)
      break;
    printf("%s\n", line);
  }
}
