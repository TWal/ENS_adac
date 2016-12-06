with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type R;
   type B is new C;
   type R is record Foo: Character; end record;
   x : B;
begin
   Put(x.Foo);
end;
