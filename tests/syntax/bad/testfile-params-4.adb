with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
  procedure P(:integer) is begin x := 0; end;
begin P(0); end;
