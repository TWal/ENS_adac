with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    type A is access Integer;
    function Blu return A is
    begin
        return null;
    end;
    x : A := Blu;
begin
    print_int__(0);
end;

