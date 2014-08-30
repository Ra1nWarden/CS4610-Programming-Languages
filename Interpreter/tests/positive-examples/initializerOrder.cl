class Main inherits IO {

a:Int<-b;
b:Int<-5;
c:Int<-{a<-4;a;};
d:Int<-a;

  main() : Object {{
    out_int(a); 
    out_string("\n") ;

    out_int(b); 
    out_string("\n") ;

    out_int(c); 
    out_string("\n") ;

    out_int(d); 
    out_string("\n") ;
  } };
} ; 
