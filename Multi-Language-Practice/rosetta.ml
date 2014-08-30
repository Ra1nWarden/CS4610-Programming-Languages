module MyUsers = Map.Make(String)

let add_to_dict myDict key value = 
  if MyUsers.mem key myDict then
    let mapped_value = MyUsers.find key myDict in
    let mapped_value = mapped_value @ [value] in
    MyUsers.add key mapped_value myDict
  else
    MyUsers.add key [value] myDict

let add_key_to_dict myDict key = 
  if MyUsers.mem key myDict then
    myDict
  else
    MyUsers.add key [] myDict

let update_map_with myDictRef node key value = 
  if List.mem node value then
    let newlist = List.filter (fun a -> not(a = node)) value in
    myDictRef := MyUsers.add key newlist !myDictRef
  else
    myDictRef := MyUsers.add key value !myDictRef

let remove_node myDict node =
  let resultDict = ref MyUsers.empty in
  let filterMap = update_map_with resultDict node in
  MyUsers.iter filterMap myDict;
  resultDict := MyUsers.remove node !resultDict;
  !resultDict

let updateString stringRef key value =
  if (List.length value = 0) then
    if !stringRef = "" then
      stringRef := key
    else if (!stringRef > key) then
      stringRef := key

let findZero myDict =
  let resultString = ref "" in
  let findString = updateString resultString in
  MyUsers.iter findString myDict;
  !resultString;;

let myGraph = ref MyUsers.empty in
try
  while true do
    let dest = read_line () in
    let source = read_line () in
    myGraph := add_to_dict !myGraph dest source;
    myGraph := add_key_to_dict !myGraph source;
  done;
with _ -> begin
  let nodeCount = MyUsers.cardinal !myGraph in
  let sortedList = ref [] in
  try
    while (List.length !sortedList) != nodeCount do
      let eliminateString = findZero !myGraph in
      if eliminateString = "" then raise Not_found;
      sortedList := !sortedList @ [eliminateString];
      myGraph := remove_node !myGraph eliminateString;
    done;
    List.iter print_endline !sortedList;
  with
      Not_found -> Printf.printf "cycle\n"
end
