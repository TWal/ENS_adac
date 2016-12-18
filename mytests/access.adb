with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type R is record
        foo, bar : Integer;
    end record;
    type R2 is record
        acc : access R;
    end record;
    type A is access R2;
    procedure Blu(z : A) is
        w : Integer;
    begin
        w := z.acc.foo;
        z.acc.foo := z.acc.bar;
        z.acc.bar := w;
    end;
    x : A;
begin
    x := new R2;
    x.acc := new R;
    x.acc.foo := 42;
    x.acc.bar := 1337;
    Blu(x);
    print_int__(x.acc.foo);
    print_int__(x.acc.bar);
end;

