with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type R;
   type P is access R;
   type R is record Foo: P; end record;
begin
   New_Line;
end;
