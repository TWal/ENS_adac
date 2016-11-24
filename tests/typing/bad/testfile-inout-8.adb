with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type R is record A: Integer; end record;
   procedure P(X: in R) is begin X.A := 0; end;
   v: R;
begin
   P(v);
end;
