let () =
  let str =
    try
      read_line ()
    with End_of_file ->
      ""
  in
  print_string str
