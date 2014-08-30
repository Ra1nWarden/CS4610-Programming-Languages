class Main inherits IO {
  main() : Object { 
    out_string("Hello, world.\n") 
  } ;
} ; 


class A inherits Main {
    s() : SELF_TYPE {
        self
    };
};


class B inherits A {
    s() : SELF_TYPE {
        self
    };
};

class SELF_TYPE_A inherits Main {
    s() : SELF_TYPE {
        self
    };
};


class SELF_TYPE_B inherits A {
    s() : SELF_TYPE {
        self
    };
};

