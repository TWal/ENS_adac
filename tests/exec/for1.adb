with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
begin
   for I in 1 .. 10 loop Put('A'); end loop; New_Line;
   for I in 10 .. 1 loop Put('B'); end loop; New_Line;
   for I in reverse 1 .. 10 loop Put('C'); end loop; New_Line;
   for I in reverse 10 .. 1 loop Put('D'); end loop; New_Line;
end;

--  Local Variables:
--  compile-command: "gnatmake for1.adb && ./for1"
--  End:
