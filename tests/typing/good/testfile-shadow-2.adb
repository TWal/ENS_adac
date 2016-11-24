with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   X: Integer;
   procedure P is
      procedure X is begin New_Line; end;
   begin
      X;
   end;
begin
   P;
end;
