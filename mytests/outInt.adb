with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    procedure Blu(y: in out Integer) is begin
        y := y*y;
        print_int__(y);
    end;
    procedure Blo(z: Integer) is
        w: Integer;
    begin
        w := z;
        Blu(w);
    end;
    function Bla(x: in out Integer) return Integer is begin
        x := x+1;
        print_int__(x);
        Blu(x);
        print_int__(x);
        Blo(x);
        return x;
    end;
    toto : Integer;
begin
    toto := 0;
    print_int__(Bla(toto));
    print_int__(Bla(toto));
    print_int__(Bla(toto));
end;

