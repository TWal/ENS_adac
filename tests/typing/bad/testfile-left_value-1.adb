with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type R is record A, B: Character; end record;
   X: R;
   function F return R is
   begin
      return X;
   end;
begin
   F.A := '1';
end Test;
