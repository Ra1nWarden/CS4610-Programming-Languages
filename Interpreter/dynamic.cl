class NewMain inherits Main {


};

class Main inherits IO {
   call(a : Int) : Object {
     if a = 1 then (new Object) else call(a-1) fi
   };
   main() : Object {
     (new NewMain)@Main.call(999)
   };
};
