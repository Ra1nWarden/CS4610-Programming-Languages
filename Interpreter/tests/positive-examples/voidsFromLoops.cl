class Main inherits IO {
  a : Object;

  main() : Object { {
    a <- while false loop 5 pool;
    if isvoid(a) then out_string("void") else out_string("something") fi;

    a <- while isvoid(a) loop a<-new Object pool;
    if isvoid(a) then out_string("void") else out_string("something") fi;
  }} ;
} ; 
