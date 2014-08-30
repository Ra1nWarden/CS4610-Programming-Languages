class Main inherits IO {
  x : A <- new B;

  main() : Object {{
	x.foo();
	(new B).make().foo();
	x.make().foo();
	(new A).make().foo();
  }} ;
} ; 

class A inherits IO{
  foo():Object{
	out_string("A")
  };

  make():SELF_TYPE{
     new SELF_TYPE
  };
};

class B inherits A{
  foo():Object{
	out_string("B")
  };
};
