class Main inherits IO {
   a: A <- new A;
   b: A <- a;  

  main() : Object { {
  a.print();
b.print();
a.set(213);
  a.print();
b.print();
	

  }} ;
} ; 


class A inherits IO{
 x : Int <- 5;
 set(y:Int):Int{
   x<-y
 };

print(): Object{
out_int(x)
};
};
