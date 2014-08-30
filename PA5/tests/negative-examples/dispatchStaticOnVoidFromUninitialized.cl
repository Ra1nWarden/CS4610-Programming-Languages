class Main inherits IO {
  x : Main;

  main() : Object { {
    out_string("Hello, world.\n") ;
    x@Main.main();
  }} ;
} ; 
