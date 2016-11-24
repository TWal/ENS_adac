with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type R is record A: Integer; end record;
   X : access R;
begin
   X := null;
end;

