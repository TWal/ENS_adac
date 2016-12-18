with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    function f(x : Integer) return Boolean is
    begin
        return f(x+1);
    end;
begin
    if true or else f(0)
    then
        print_int__(42);
    else
        print_int__(0);
    end if;
end;

