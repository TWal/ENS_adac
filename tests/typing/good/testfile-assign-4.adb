with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type R is record A: Integer; end record;
   type T is access R;
   procedure P(X: in T) is begin X.A := 0; end;
   v: T := new R;
begin
   P(v);
end;

