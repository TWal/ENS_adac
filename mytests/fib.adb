with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    a : Integer;
    b : Integer;
    tmp : Integer;
begin
    a := 0;
    b := 1;
    while a < 1000 loop
        print_int(a);
        tmp := a+b;
        a := b;
        b := tmp;
    end loop;
end;

