class Main inherits IO {
	x : Int <- 4;

  main() : Object { {
    let y :Int<- {x<- 2;} in y;
out_int(x);
}
  } ;
} ; 
