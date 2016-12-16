with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type R is record
        foo, bar, baz: Integer;
    end record;
    type R2 is record
        foo, bar : R;
    end record;
    a : R2;
begin
    a.foo.foo := 0;
    print_int__(a.foo.foo);
    a.foo.bar := 1;
    print_int__(a.foo.bar);
    a.foo.baz := 2;
    print_int__(a.foo.baz);
    a.bar.foo := 3;
    print_int__(a.bar.foo);
    a.bar.bar := 4;
    print_int__(a.bar.bar);
    a.bar.baz := 5;
    print_int__(a.bar.baz);
    put('a');
    new_line;
    new_line;
    new_line;
end;

