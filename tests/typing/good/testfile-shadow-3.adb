with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   X: Integer;
   procedure P is
      type X is record A: Integer; end record;
      V: X;
   begin
      New_Line;
   end;
begin
   P;
end;

