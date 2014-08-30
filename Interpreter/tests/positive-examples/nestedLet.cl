class Main inherits IO {
x : String<- "potato";
  main() : Object { 
    let x:Int <- 5 in let x:Int<-6 in {let x:Int<-7 in 5;out_int(x);}
  } ;
} ; 
