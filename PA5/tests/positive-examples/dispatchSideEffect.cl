class Main inherits IO {
	x : Int <-3;
	y : Int <-6;
  main() : Object { 
   {
{x<-2;self;}.foo(y<-12);
out_int(x);
out_int(y);
{x<-4;self;}@Main.foo(y<-135);
out_int(x);
out_int(y);
}
  } ;

foo(y : Int):Object{
5
};
} ; 
