with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type R is record Foo: Character; end record;
   type B is R;
   x : B;
begin
   Put(x.Foo);
end;
