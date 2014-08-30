class Main inherits IO {
  x : A <- new B;

  main() : Object { 
	x.foo()
  } ;
} ; 

class A inherits IO{
  foo():Object{
	out_string("A")
  };
};

class B inherits A{
  foo():Object{
	out_string("B")
  };
};
