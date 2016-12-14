with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    A : Integer;
    procedure Bla(foo : Integer) is
        bar : Integer;
    begin
        Put(character'val(foo+a));
        for I in 1 .. 10 loop
            Put(character'val(bar+I));
            for I in 1 .. 10 loop
                Put(character'val(97+I));
            end loop;
        end loop;
    end;
begin
    for I in 1 .. 10 loop
        Put('A');
    end loop;
end;
