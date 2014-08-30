class Main inherits IO {
  a : Int <- ~2147483647;
  main() : Object {
    {
      out_int(~ (a - 1));
      out_string("\n");
      out_int(a * 3);
      out_string("\n");
      out_int(a - 1);
      out_string("\n");
      out_int(a / 10 - a * a + a);
    }
  };
};
