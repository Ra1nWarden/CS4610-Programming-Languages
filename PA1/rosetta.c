#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct connection {
  char* source;
  char* dest;
} connection;

typedef struct node {
  int inDegree;
  int eliminated;
  char* value;
} node;

node** updateNode(node** nodes, char* dest, char* source, int* arrayLength) {
  int found = 0;
  int i;
  for(i = 0; i < *arrayLength; i++) {
    if(strcmp(nodes[i]->value, dest) == 0) {
      nodes[i]->inDegree++;
      found = 1;
      break;
    }
  }
  if(!found) {
    (*arrayLength)++;
    nodes = realloc(nodes, sizeof(node*) * (*arrayLength));
    nodes[*arrayLength-1] = malloc(sizeof(node));
    nodes[*arrayLength-1]->inDegree = 1;
    nodes[*arrayLength-1]->eliminated = 0;
    nodes[*arrayLength-1]->value = malloc(61);
    strcpy(nodes[*arrayLength-1]->value, dest);
  }
  found = 0;
  for(i = 0; i < *arrayLength; i++) {
    if(strcmp(nodes[i]->value, source) == 0) {
      found = 1;
      break;
    }
  }
  if(!found) {
    (*arrayLength)++;
    nodes = realloc(nodes, sizeof(node*) * (*arrayLength));
    nodes[*arrayLength-1] = malloc(sizeof(node));
    nodes[*arrayLength-1]->inDegree = 0;
    nodes[*arrayLength-1]->eliminated = 0;
    nodes[*arrayLength-1]->value = malloc(61);
    strcpy(nodes[*arrayLength-1]->value, source);
    nodes[*arrayLength - 1]->inDegree = 0;
  }
  return nodes;
}

int eliminateZero(node** nodes, int nodeCount, connection** connections, int connectCount, char** result) {
  int found = 0;
  int removeIndex = 0;
  char* resultString = malloc(61);
  int i;
  for(i = 0; i < nodeCount; i++) {
    if(nodes[i]->eliminated == 0 && nodes[i]->inDegree == 0) {
      if(!found) {
	strcpy(resultString, nodes[i]->value);
	found = 1;
	removeIndex = i;
      }
      else {
	if(strcmp(nodes[i]->value, resultString) < 0) {
	  strcpy(resultString, nodes[i]->value);
	  removeIndex = i;
	}
      }
    }
  }
  if(!found)
    return 0;
  strcpy(*result,resultString);
  nodes[removeIndex]->eliminated = 1;
  for(i = 0; i < connectCount; i++) {
    if(strcmp(connections[i]->source, resultString) == 0) {
      int j;
      for(j = 0; j < nodeCount; j++) {
	if(strcmp(connections[i]->dest, nodes[j]->value) == 0) {
	  nodes[j]->inDegree--;
	  break;
	}
      }
    }
  }
  return 1;
}

int main() {
  int maxlength = 60 + 1;
  int conArraySize = 2;
  int nodeCount = 0;
  node** nodes = malloc(sizeof(node*));
  connection** connections =  malloc(sizeof(connection*) * conArraySize);
  int runningIndex = 0;
  while(1) {
    char* destline = malloc(maxlength);
    int status = getline(&destline, &maxlength, stdin);
    if(status == -1)
      break;
    char* sourceline =  malloc(maxlength);
    getline(&sourceline, &maxlength, stdin);
    if(runningIndex == conArraySize) {
      conArraySize *= 2;
      connections = realloc(connections, sizeof(connection*) * conArraySize);
    }
    connections[runningIndex] = malloc(sizeof(connection));
    connections[runningIndex]->source = malloc(maxlength);
    connections[runningIndex]->dest = malloc(maxlength);
    strcpy(connections[runningIndex]->source, sourceline);
    strcpy(connections[runningIndex]->dest, destline);
    nodes = updateNode(nodes, destline, sourceline, &nodeCount);
    runningIndex++;
  }
  int connectionCount = runningIndex;
  char** sortedList = malloc(sizeof(char*) * nodeCount);
  int cycle = 0;
  int sortedIndex = 0;
  while(sortedIndex != nodeCount) {
    char* word = malloc(maxlength);
    int flag = eliminateZero(nodes, nodeCount, connections, connectionCount, &word);
    if(!flag) {
      cycle = 1;
      break;
    }
    sortedList[sortedIndex] = malloc(maxlength);
    strcpy(sortedList[sortedIndex], word);
    sortedIndex++;
  }
  if(cycle)
    printf("cycle\n");
  else{
    int i;
    for(i = 0; i < nodeCount; i++)
      printf("%s", sortedList[i]);
  }
}
