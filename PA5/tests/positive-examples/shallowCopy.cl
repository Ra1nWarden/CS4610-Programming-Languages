class Main inherits IO {
  b : B <- new B;
  main() : Object {
    let b2 : B <- b.init() in
	if b.get() = b2.get() then
		out_string(" potato") else out_string("cheese") fi
  } ;
} ;

class A{};

class B inherits A{
  a :A;

  init() : SELF_TYPE{
	   a<-self.copy()
  };

  get() : A{
   a
  };
};
