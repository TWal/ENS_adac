with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type A is access Integer;
    type B is access A;
    procedure Blu(z : B) is
    begin
        z.all.all := 42;
    end;
    x : B;
begin
    x := new A;
    x.all := new Integer;
    x.all.all := 0;
    Blu(x);
    print_int__(x.all.all);
    free__(x.all);
    free__(x);
end;

