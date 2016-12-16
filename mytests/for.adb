with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    A : Integer;
    procedure Bla(foo : Integer) is
        bar : Integer;
    begin
        bar := 65;
        for I in 1 .. 10 loop
            Put(character'val(bar+I));
            New_Line;
            for I in reverse 1 .. 10 loop
                Put(character'val(foo+I));
                New_Line;
            end loop;
        end loop;
    end;
begin
    for I in 1 .. 10 loop
        Put('A');
        New_Line;
    end loop;
    Bla(97);
end;
