class Main inherits IO {
x:String;

  main() : Object { {
    out_string(x) ;
out_int(x.length());
out_string("\n");
x.concat("hey");
out_string(x) ;
out_int(x.length());
out_string("\n");
x.concat(x);
out_string(x) ;
out_int(x.length());
out_string("\n");
x.concat("whichSide?");
out_string(x) ;
out_int(x.length());
out_string("\n");

out_string(in_string());
out_string("\n");
out_string(in_string());
out_string("\n");
x<-in_string();
out_string(x);
out_string("\n");
x<-in_string();
out_string(x);
out_string("\n");


x<-in_string();
out_int(x.length());
out_string(x);
out_string("\n");

x<-in_string();
out_int(x.length());
out_string(x);
out_string("\n");

x<-in_string();
out_int(x.length());
out_string(x);
out_string("\n");
  } };
} ; 
