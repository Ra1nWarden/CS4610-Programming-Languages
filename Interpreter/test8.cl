class A {
  init() : SELF_TYPE {
    self
  };
};
class B inherits A {

};

class Main inherits IO {
  a : A <- (new B)@A.init();
  main() : Object {
    case a of x : A => out_int(10);
              y : B => out_int(20);
    esac
  };
};
