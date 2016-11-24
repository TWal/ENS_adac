with Ada.Text_IO; use Ada.Text_IO;

procedure Pascal is

   type Node;
   type List is access Node;
   type Node is record
      Value: Integer;
      Next: List;
   end record;

   function Get(L: List; I: Integer) return Integer is
   begin
      if I = 0 then return L.Value; end if;
      return Get(L.next, I - 1);
   end;
   procedure Set(L: List; I, V: Integer) is
   begin
      if I = 0 then
         L.Value := v;
      else
         Set(L.next, I - 1, V);
      end if;
   end;

   function Create(N: Integer) return List is
      L: List;
   begin
      if N = 0 then return null; end if;
      L := new Node;
      L.Value := 0;
      L.Next := Create(N - 1);
      return L;
   end;

   N: Integer := 42;
   R: List := Create(N + 1);

   procedure print_row(I: Integer) is
   begin
      for J in 0..I loop
         if Get(R, J) /= 0 then
            Put('*');
         else
            Put('.');
         end if;
      end loop;
      New_Line;
   end Print_Row;

   procedure compute_row(I: Integer) is
   begin
      for J in reverse 1..I loop
         Set(R, J, (Get(R, J) + Get(R, J-1)) rem 7);
      end loop;
      Set(R, 0, 1);
   end Compute_Row;

begin
   for i in 0 .. N-1 loop
      Set(R, I, 0);
      compute_row(i);
      print_row(i);
   end loop;
end Pascal;

--  Local Variables:
--  compile-command: "gnatmake pascal.adb && ./pascal"
--  End:
