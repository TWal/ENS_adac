with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   function F(X: Integer) return Integer is
   begin
      Put('F');
      return X;
   end;
begin
   for I in F(1) .. F(10) loop Put('A'); end loop; New_Line;
   for I in F(10) .. F(1) loop Put('B'); end loop; New_Line;
   for I in reverse F(1) .. F(10) loop Put('C'); end loop; New_Line;
   for I in reverse F(10) .. F(1) loop Put('D'); end loop; New_Line;
end;

--  Local Variables:
--  compile-command: "gnatmake for2.adb && ./for2"
--  End:
