lines = []
while input = gets do
  lines << input
end

myGraph = Hash.new
lines.each_index do |i|
  if i % 2 == 0
    dest = lines[i]
    source = lines[i+1]
    if myGraph[dest].nil?
      inIndex = [source]
      myGraph[dest] = inIndex
    else
      updated = myGraph[dest]
      updated << source
      myGraph[dest] = updated
    end
    if myGraph[source].nil?
      myGraph[source] = []
    end
  end
end

sortedList = []
totalTasks = myGraph.size
while sortedList.size < totalTasks do
  candidateList = []
  myGraph.each_key do |dest|
    inSource = myGraph[dest]
    if inSource.empty?
      candidateList << dest
    end
  end
  break if candidateList.empty?
  candidateList.sort!
  removeNode = candidateList[0]
  sortedList << removeNode
  newGraph = Hash.new
  myGraph.each_key do |dest|
    next if dest == removeNode
    inSource = myGraph[dest]
    inSource.delete(removeNode)
    newGraph[dest] = inSource
  end
  myGraph = newGraph
end

if sortedList.size < totalTasks
  puts 'cycle'
else
  sortedList.each do |x|
    puts x
  end
end
