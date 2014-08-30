class Main inherits IO {
    a : String <- "salad";
    main() : Object {
    
    case "cheese" of
      a : Object => out_string("yo");
      b : String => {out_string(b);};
    esac
  } ;
} ; 
