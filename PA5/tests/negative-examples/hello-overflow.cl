class Main inherits IO {
  main() : Object { 
    f(999)
  } ;

  f(x : Int) : Object{{
    if(x = 0) then 0 else f(x - 1) fi;
}};
} ; 
