with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    procedure Bla(x, y: Integer) is
        foo : Integer := x+y;
        bar : Integer := x-y;
    begin
        print_int__(foo);
        print_int__(bar);
    end;
begin
    bla(42, 57);
    bla(1337, -50);
end;

