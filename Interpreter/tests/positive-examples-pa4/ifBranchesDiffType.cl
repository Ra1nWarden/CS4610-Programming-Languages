class Main inherits IO {
  main() : Object { 
    out_string("Hello, world.\n") 
  } ;
} ; 

class A inherits IO {
  main() : Object {{
        if 0 = 1 then
            4
        else
            ""
        fi;

        while 0 < 1 loop
                    {
                        self.abort();
                    }
                pool;
     
  }} ;
} ; 