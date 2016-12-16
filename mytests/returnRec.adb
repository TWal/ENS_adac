with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type R is record
        foo, bar : Integer;
    end record;
    function Blu(x, y : Integer) return R is
        res : R;
    begin
        res.foo := x+3;
        res.bar := y-3;
        return res;
    end;
    a : R;
begin
    a := Blu(42, 1337);
    print_int__(a.foo);
    print_int__(a.bar);
end;
