with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type R is record
        foo, bar : Integer;
    end record;
    a : R;
    b : R;
begin
    a.foo := 42;
    a.bar := 1337;
    b := a;
    print_int__(b.foo);
    print_int__(b.bar);
    b.foo := 57;
    b.bar := 17;
    print_int__(a.foo);
    print_int__(a.bar);
    print_int__(b.foo);
    print_int__(b.bar);
end;
