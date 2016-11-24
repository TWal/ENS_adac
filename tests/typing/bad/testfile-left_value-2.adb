with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type R is record A, B: Character; end record;
   X: R;
   function F return R is
   begin
      return X;
   end;
   procedure Update(C: in out Character) is
   begin
      C := '0';
   end;
begin
   update(F.A);
end Test;
