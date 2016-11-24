with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function Oups(N: Integer) return Integer is
      L : Integer := Oups(N);
   begin
      return 0;
   end;
begin
   New_Line;
end;

