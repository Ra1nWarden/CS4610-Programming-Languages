class A {};
class B {};

class Main inherits IO {
	main() : Object {
		let a : A <- new A, b : B <- new B, c : A <- a in
			{
				if a <= b then out_string("small\n") else out_string("large\n") fi;
				if b <= a then out_string("small\n") else out_string("large\n") fi;
				if a <= c then out_string("small\n") else out_string("large\n") fi;
				if c <= a then out_string("small\n") else out_string("large\n") fi;
				if b <= c then out_string("small\n") else out_string("large\n") fi;
				if c <= b then out_string("small\n") else out_string("large\n") fi;
			}
	};
};
