with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    function Bla(x, y: Integer) return Integer is
        foo : Integer;
        tmp : Integer;
        function Blu(z, w : Integer) return Integer is
            bar : Integer;
        begin
            bar := x+y;
            foo := foo + bar;
            return foo;
        end;
    begin
        foo := 0;
        print_int__(x+3);
        print_int__(y-3);
        tmp := Blu(x+3, y-3);
        return foo+tmp;
    end;
begin
    print_int__(bla(42, 57)); --198
    print_int__(bla(1337, -50)); --2574
end;

