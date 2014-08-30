class Main inherits IO {
a:Main;
b:Main;

  main() : Object { {
    if a=b then out_string("yo") else out_string("mama") fi;
    a<-new Main;
    if a=b then out_string("yo") else out_string("mama") fi;
    b<-new Main;
    if a=b then out_string("yo") else out_string("mama") fi;
    a<-b;
        if a=b then out_string("yo") else out_string("mama") fi;
  } };
} ; 
