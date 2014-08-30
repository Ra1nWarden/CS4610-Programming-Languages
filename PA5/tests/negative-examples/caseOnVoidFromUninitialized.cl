class Main inherits IO {
  x : Main;

  main() : Object { {
    out_string("Hello, world.\n") ;
    case x of
	yo : Object => out_string("blarg");
    esac;
  } };
} ; 
