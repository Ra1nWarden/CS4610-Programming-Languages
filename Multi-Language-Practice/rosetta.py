import sys

def readGraph(lines):
    myGraph = {}
    for index in list(xrange(0, len(lines), 2)):
        dest = lines[index]
        source = lines[index+1]
        if dest not in myGraph:
            myGraph[dest] = []
        myGraph[dest].append(source)
        if source not in myGraph:
            myGraph[source] = []
    return myGraph

def removeZero(myGraph):
    candidateList = []
    for each_key in myGraph:
        inDegree = len(myGraph[each_key])
        if inDegree == 0:
            candidateList.append(each_key)
    if len(candidateList) == 0:
        return [False]
    candidateList.sort()
    toBeRemoved = candidateList[0]
    newGraph = dict()
    for each_key in myGraph:
        if each_key == toBeRemoved:
            continue
        else:
            newSourceList = list()
            for each_source in myGraph[each_key]:
                if each_source == toBeRemoved:
                    continue
                newSourceList.append(each_source)
            newGraph[each_key] = newSourceList
    return [newGraph, toBeRemoved]

lines = sys.__stdin__.readlines()
currentGraph = readGraph(lines)
result = []
success = True
nodes = len(currentGraph)
while len(result) != nodes:
    returnedResult = removeZero(currentGraph)
    if len(returnedResult) == 1:
        success = False
        break
    else:
        currentGraph = returnedResult[0]
        result.append(returnedResult[1])
if success:
    for each_word in result:
        print each_word ,
else:
    print 'cycle'
