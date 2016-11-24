with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type R is record Foo: access R; end record;
begin
   New_Line;
end;
