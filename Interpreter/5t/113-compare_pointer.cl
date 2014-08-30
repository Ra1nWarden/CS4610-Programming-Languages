class Main inherits IO {
    main() : SELF_TYPE {
	let c : Complex <- new Complex in
	        if c = c
	        then out_string("passed\n")
	        else out_string("failed\n")
	        fi
    };
};

class Complex {

};
