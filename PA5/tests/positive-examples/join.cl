class Main inherits IO {
  main() : Object { {
    case (if true then new A else new B fi) of
    a : A => out_string("aaa");
    b : B => out_string("bbb");
    c : Object => out_string("obj");
  esac;

    case (if true then "" else 425 fi) of
    a : String => out_string("aaa");
    b : Int => out_string("bbb");
    c : Object => out_string("obj");
esac;

    case (if false then new A else new B fi) of
    a : A => out_string("aaa");
    b : B => out_string("bbb");
    c : Object => out_string("obj");
  esac;

    case (if false then "" else 425 fi) of
    a : String => out_string("aaa");
    b : Int => out_string("bbb");
    c : Object => out_string("obj");
esac;
  } };
} ; 


class A{


};

class B inherits A{

};
