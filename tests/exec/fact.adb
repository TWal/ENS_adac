with Ada.Text_IO; use Ada.Text_IO;

procedure Fact is

   procedure PrintInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N > 9 then PrintInt(N / 10); end if;
      Put(Character'Val(48 + C));
   end;

   f : integer;

   procedure fact(n : integer) is
   begin
      if n <= 1 then f := 1; else fact(n-1); f := n * F; end if;
   end;

begin fact(10); printint(f); New_Line; End;
