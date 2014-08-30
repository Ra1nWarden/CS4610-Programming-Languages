class Main inherits IO {
  main() : Object { {
    case "" of
       x : Object => out_string("yo");
       y : String => out_string("mama");
    esac;

    case 5 of
       x : Object => out_string("yo");
       y : String => out_string("mama");
       z : Int => out_string("so flat");
    esac;

    case false of
       x : Object => out_string("yo");
       y : String => out_string("mama");
       z : Int => out_string("so flat");
       a : Bool => out_string("adsfasdfsaf");
    esac;


    case true of
       x : Object => out_string("yo");
       y : String => out_string("mama");
       z : Int => out_string("so flat");
       a : Bool => out_string("adsfasdfsaf");
    esac;

  } };
} ; 
