class Main inherits IO {
  x : Int <-5;
  y : Int <- 1234;
  main() : Object { {
	{x<-52;new Main;}.printInts(x,y,x,y);
	{x<-52;new Main;}.printInts(x<-213,x+y,{y<-315;x;},x<-76);
}
  } ;

  printInts(a:Int, b:Int, c:Int, d:Int):Object{{
out_int(a);
out_int(b);
out_int(c);
out_int(d);
}};
} ; 
