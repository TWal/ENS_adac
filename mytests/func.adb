with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    procedure Bla(foo : Integer; bar : Integer) is
    begin
        print_int__(foo);
        print_int__(bar);
        print_int__(foo+bar);
    end;
begin
    Bla(0, 1);
    Bla(42, 1337);
end;
