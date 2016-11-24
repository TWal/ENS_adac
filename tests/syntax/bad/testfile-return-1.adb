with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
  function F return character is begin return 'a' end F;
begin Put(F); end;

