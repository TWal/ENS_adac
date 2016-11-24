with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   X: Integer;
   procedure P is
      X: Character;
   begin
      Put(X);
   end;
begin
   P;
end;
