class Main inherits IO {
  x : IO <- new IO;

  main() : Object { {
    out_string("Hello, world.\n") ;
    case x of
	yo : Main => out_string("blarg");
    esac;
  } };
} ; 


