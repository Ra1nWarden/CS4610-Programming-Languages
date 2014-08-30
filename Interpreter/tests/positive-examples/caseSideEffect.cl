class Main inherits IO {
    a : String <- "salad";
    x : Int <- 4;
    main() : Object {
 {   
    case "cheese" of
      a : Object => out_string("yo");
      b : String => {x<-555 ; out_string(b);};
    esac;

out_int(x);
}  } ;
} ; 
