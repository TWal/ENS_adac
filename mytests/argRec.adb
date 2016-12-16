with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type R is record
        foo, bar : Integer;
    end record;
    procedure Blu(c : R) is
    begin
        print_int__(c.foo);
        print_int__(c.bar);
    end;
    a : R;
    b : R; -- needed to check if the records are copied on the stack
begin
    a.foo := 42;
    a.bar := 1337;
    Blu(a);
end;

