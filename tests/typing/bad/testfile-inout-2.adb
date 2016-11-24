with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P(X: in Integer) is begin X := 0; end;
begin
   P(42);
end;
