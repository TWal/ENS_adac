with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    function RetTrue(x : Integer) return Boolean is
    begin
        return True;
    end;
    function RetFalse(x : Integer) return Boolean is
    begin
        return False;
    end;
    function RetA(x : Integer) return Character is
    begin
        return 'A';
    end;
begin
    if RetTrue(42) then
        Put('a'); New_Line;
    else
        Put('b'); New_Line;
    end if;
    if RetFalse(42) then
        Put('c'); New_Line;
    else
        Put('d'); New_Line;
    end if;
    Put(RetA(42)); New_Line;
end;

