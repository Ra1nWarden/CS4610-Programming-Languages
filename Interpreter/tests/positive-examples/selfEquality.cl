class Main inherits IO {
  a:A;
  b:Main;
   c:A<- new A;
   d:A<- new A;
   main() : Object { {
    if(a=b) then out_string("yolo") else out_string("woooloooloo") fi;

    if(c=d) then out_string("yolo") else out_string("woooloooloo") fi;

    if((new A)=(new A)) then out_string("yolo") else out_string("woooloooloo") fi;

    if(c=c.get()) then out_string("yolo") else out_string("woooloooloo") fi;

    d<-c.get();

    if(c=d) then out_string("yolo") else out_string("woooloooloo") fi;
  } };
} ; 

class A{
   x:Int;
   get() : SELF_TYPE{
   self
};
};
