class Main inherits IO {
  main() : Object { {
    if new A = new A then out_string("yo") else out_string("mama") fi;

let t :A <- new A, y:A <-t.copy() in if t=y  then out_string("yo") else out_string("mama") fi;}
  } ;
} ; 

class A{
x:Int;

set(a:Int):Int{
x<-a
};

};

