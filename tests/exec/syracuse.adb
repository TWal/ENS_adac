with Ada.Text_IO; use Ada.Text_IO;

procedure Syracuse is

   procedure PrintInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N > 9 then PrintInt(N / 10); end if;
      Put(Character'Val(48 + C));
   end;

   procedure syracuse(max : integer) is
      i : integer;
      procedure length is
         v,j : integer;
         procedure step is
         begin
            v := v+1; if j = 2*(j/2) then j := j/2; else j := 3*j+1; end if;
         end;
      begin
         v := 0;
         j := i;
         while j /= 1 loop step; end loop;
         printint(v); New_Line;
      end;
   begin
      i := 1;
      while i <= max loop length; i := i+1; end loop;
   end;

begin
   syracuse(10);
end;
