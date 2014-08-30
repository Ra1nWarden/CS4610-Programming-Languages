class Number {
  a : Int <- 3;
  getA() : Int {
     a
  };
  setA(x : Int) : SELF_TYPE {
    { a <- x;
      self;
    }
  };
};

class Wrap {
  a : Number <- new Number;
  setNum(x : Number) : Number {
    a <- x
  };
  getNum() : Int {
    a.getA()
  };
};

class Main inherits IO {
  main() : Object {
    let a : Wrap <- new Wrap, b : Wrap <- a.copy() in
      {
        a.setNum((new Number).setA(100));
        out_int(b.getNum());
        out_int(a.getNum());
      }
  };
};
