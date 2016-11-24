with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type R is record A: Integer; end record;
   procedure F(X: in out Integer) is begin X := 0; end;
   procedure G(X: in R) is begin F(X.A); end; X: R;
begin
   G(X);
end Test;

