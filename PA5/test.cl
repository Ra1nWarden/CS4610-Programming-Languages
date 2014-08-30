class Fraction {
  num : Int;
  den : Int;
  setNum(n : Int) : Int {
    num <- n
  };
  print() : Object {
    {
      (new IO).out_int(num);
      (new IO).out_string("\n");
      (new IO).out_int(den);
      (new IO).out_string("\n");
    }
  };
  init(n : Int, d : Int) : SELF_TYPE {
    {
      num <- n;
      den <- d;
      self;
    }
  };
};

class Main inherits IO {
  g : Fraction <- new Fraction;
  print_main(f : Fraction) : Object {
    {
      f.setNum(5);
      f.print();
      g.print();
      g.setNum(10);
      g.print();
    }
  };
  main() : Object {
    let a : Fraction <- (new Fraction).init(2, 3) in
      print_main(a)
  };
};
