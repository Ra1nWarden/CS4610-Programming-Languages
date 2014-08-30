class B {
    a() : Int {
	1
    };
};

class A inherits B {
    b() : Int {
	2
    };
};


class Main {
    main() : Int {
	let x : A <- new B in { x.b(); }
    };
};
