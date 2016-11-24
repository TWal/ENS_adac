with Ada.Text_IO; use Ada.Text_IO;

procedure Oups is
   type R is record A: Integer; end record;
   V : access R := null;
begin
   if V.A = 1 then New_Line; end if;
end;

