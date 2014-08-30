class BoolWrap {
  b : Bool <- true;
  toggle() : Bool {
    b <- not b
  };
  returnBool() : Bool {
    b
  };
};

class Number {
  a : Int <- 3;
  w : BoolWrap <- new BoolWrap;
  getA() : Int {
      a
  };
  setA(b : Int) : Int {
      a <- b
  };
  toggleW() : Bool {
    w.toggle()
  };
  getW() : Bool {
    w.returnBool()
  };
};

class Main inherits IO {
	x : Number <- new Number;
  main() : Object {
      let y : Number <- x.copy() in {
        x.setA(10);
        out_int(y.getA());
        out_string("\n");
        y.setA(20);
        out_int(y.getA());
        out_string("\n");
        if y.getW() then out_string("true\n") else out_string("false\n") fi;
        x.toggleW();
        if y.getW() then out_string("true\n") else out_string("false\n") fi;
        y.toggleW();
        if y.getW() then out_string("true\n") else out_string("false\n") fi;
      }
 };
};
