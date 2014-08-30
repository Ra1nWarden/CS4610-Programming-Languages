class Main inherits IO {
main() : Object{
(new A).main()
};
} ; 

class A inherits IO{
  main() : Object { {
    if isvoid(a) then
	out_string("void")
    else
	out_string("not")
    fi;

    if isvoid(b) then
	out_string("void")
    else
	out_string("not")
    fi;

    if isvoid(c) then
	out_string("void")
    else
	out_string("not")
    fi;

    out_int(d);
    out_int(e);

    if f then out_string("true") else out_string("not") fi;
    if g then out_string("true") else out_string("not") fi;
    if h then out_string("true") else out_string("not") fi;

    out_string(i);
    out_string(j);
    out_string(k);

    if isvoid(l) then
	out_string("void")
    else
	out_string("not")
    fi;

    if isvoid(m) then
	out_string("void")
    else
	out_string("not")
    fi;

  } };


  a : A;
  b : B;
  c : B <- new B;
  d : Int;
  e : Int <- 55;
  f : Bool;
  g : Bool <- true;
  h : Bool <- false;
  i : String;
  j : String <- "";
  k : String <- "potato";
  l : Object;
  m : Object <- new Object;
};

class B {


};
