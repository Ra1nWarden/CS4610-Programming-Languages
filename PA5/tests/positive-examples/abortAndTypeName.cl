class Main inherits IO {
x: A<- new B;
  main() : Object { {
    out_string("Hello, world.\n") ;
out_string(self.type_name());
out_string((new A).type_name());
out_string((new B).type_name());
out_string(x.type_name());

self.abort();
out_string((new A).type_name());
  }} ;
} ; 

class A {

};

class B inherits A{};
