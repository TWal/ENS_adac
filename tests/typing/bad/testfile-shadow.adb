with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type X is record A: Integer; end record;
   procedure P is
      type T is X;
      type X is record A: Character; end record;
      V: T;
   begin
       Put(V.A);
   end;
begin
   P;
end;

