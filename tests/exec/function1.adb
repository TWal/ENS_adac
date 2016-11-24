with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   function F return character is
   begin return 'a';
   end F;
   function G(C: Character) return character is
   begin return c;
   end G;
begin
   Put(F); New_Line;
   Put(G('b')); New_Line;
   Put(G(F)); New_Line;
end;
