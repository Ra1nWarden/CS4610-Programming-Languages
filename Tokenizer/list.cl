(*
 * jfioefjieojfoie
 * fjoiefjeoifjoeij
 *)

class List {

   isNil() : Bool { true };

   head()  : Int { { abort(); 0; } };

   tail()  : List { { abort(); self; } };

   cons(i : Int) : List {
      (new Cons).init(i, self)
   };

};

class Cons inherits List {

   car : Int;	-- The element in this list cell

   cdr : List;	-- The rest of the list

   isNil() : Bool { false };

   head()  : Int { car };

   tail()  : List { cdr };

   init(i : Int, rest : List) : List {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };

};


class Main inherits IO {

   mylist : List;

   print_list(l : List) : Object {
      if l.isNil() then out_string("\n")
                   else {
			   out_int(l.head());
			   out_string(" ");
			   print_list(l.tail());
		        }
      fi
   };

   main() : Object {
      {
	 mylist <- new List.cons(1).cons(2).cons(3).cons(4).cons(5);
	 while (not mylist.isNil()) loop
	    {
	       print_list(mylist);
	       mylist <- mylist.tail();
	    }
	 pool;
      }
   };

};


-- jioejfoiejfoei
