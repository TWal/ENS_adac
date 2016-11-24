with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
  procedure P is begin x := 0; end Q;
begin P(0); end;
