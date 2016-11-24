with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
  procedure P(x:integer;) is begin x := 0; end;
begin P(0); end;
