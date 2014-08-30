class Main inherits IO {
  main() : Object { 
    f(998)
  } ;

  f(x : Int) : Object{{
    if(x = 0) then (new Main) else f(x - 1) fi;
}};
} ; 
