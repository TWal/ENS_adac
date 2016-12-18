with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    x : access Integer := null;
begin
    if false and then x.all = 0
    then
        print_int__(0);
    else
        print_int__(42);
    end if;
end;

