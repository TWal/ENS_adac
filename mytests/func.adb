with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    procedure Bla(foo : Integer; bar : Integer) is
    begin
        print_int(foo);
        print_int(bar);
        print_int(foo+bar);
    end;
begin
    Bla(0, 1);
    Bla(42, 1337);
end;
