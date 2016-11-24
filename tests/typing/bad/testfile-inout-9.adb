with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure F(X: in out Integer) is begin X := 0; end;
   procedure G(X: in Integer) is begin F(X); end;
   X: Integer;
begin
   G(X);
end Test;
