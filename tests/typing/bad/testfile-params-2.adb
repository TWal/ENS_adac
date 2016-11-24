with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P(A: integer; A: character) is begin New_Line; end;
begin
   P(0,'a');
end;
