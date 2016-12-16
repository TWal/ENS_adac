with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    function Bla(x, y: Integer) return Integer is begin
        return x+y;
    end;
begin
    print_int__(bla(42, 57));
    print_int__(bla(1337, -50));
end;

