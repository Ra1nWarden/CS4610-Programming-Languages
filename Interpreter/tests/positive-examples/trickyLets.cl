class Main inherits IO {
g : Int<-666;
x : Int <- 134;

  main() : Object {{
    let x :Int <- 4, x:Int<- 5 in 
	    out_int(x);

let x:Int <- 4, y : Int<- {x<-12;5;} in 
    {
    out_int(y);
    out_int(x);
	};


let x:Int <- 4, y:Int<- {let x:Int<-12 in 5;} in 
    {
    out_int(y);
    out_int(x);
    out_int(g);
	};


let g:Int <- 4, y:Int<- {let x:Int<-12 in 5;} in 
    {
    out_int(y);
    out_int(x);
    out_int(g);
	};

let y:Int <- 412, x:Int<- y in 
    {
    out_int(y);
    out_int(x);
    out_int(g);
	};

  }} ;
} ; 
