with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    type R is record
        A, B: Integer;
    end record;
    function Bla(x: in out R) return Integer is
        tmp: Integer;
    begin
        tmp := x.A;
        x.A := x.B;
        x.B := x.B + tmp;
        return x.B;
    end;
    toto : R;
begin
    toto.A := 0;
    toto.B := 1;
    while toto.A < 500 loop
        print_int__(Bla(toto));
    end loop;
end;

