with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   procedure P(X: in out Integer) is begin new_line; end;
begin
   P(42);
end;
