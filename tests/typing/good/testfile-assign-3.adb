with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P(X: in out Integer) is begin new_line; end;
   type r is record a: integer; end record;
   function f return access r is begin return new r; end;
begin
   P(f.a);
end;
