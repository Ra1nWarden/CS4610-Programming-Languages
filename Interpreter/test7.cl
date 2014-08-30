class A {};
class B {};

class Main inherits IO {
  main() : Object {
    let a : A <- new A, b : B <- new B, c : A <- a in
      {
        if a <= b then out_string("small") else out_string("large") fi;
        if b <= a then out_string("small") else out_string("large") fi;
        if a <= c then out_string("small") else out_string("large") fi;
        if c <= a then out_string("small") else out_string("large") fi;
        if b <= c then out_string("small") else out_string("large") fi;
        if c <= b then out_string("small") else out_string("large") fi;
      }
  };
};
