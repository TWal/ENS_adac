with Ada.Text_IO; use Ada.Text_IO;

-- les N reines

procedure NQueens is
   procedure PrintInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N > 9 then PrintInt(N / 10); end if;
      Put(Character'Val(48 + C));
   end;
   -- des ensembles représentés par des entiers
   Empty: Integer := 0;
   function Mem(X, S: Integer) return Boolean is
   begin
      -- X est une puissance de 2
      return (S / X) rem 2 = 1;
   end;
   -- décompte récursif
   function Q(N: Integer) return Integer is
      Full : Integer;
      function T(A, B, C: Integer) return Integer is
         F,D : Integer;
      begin
         if A = Empty then return 1; end if;
         F := 0;
         D := 1;
         for I in 0 .. N-1 loop
            if Mem(D, A) and then not Mem(D, B) and then not Mem(D, C) then
               F := F + T(A-D, (B+D) * 2, (C+D) / 2);
            end if;
            D := 2 * D;
         end loop;
         return F;
      end;
   begin
      Full := 0;
      for I in 0 .. N-1 loop Full := 2 * Full + 1; end loop;
      return T(Full, Empty, Empty);
   end;
begin
   for N in 1 .. 10 loop
      Printint(N); Put(' ');
      Printint(Q(N)); New_Line;
   end loop;
end;

--  Local Variables:
--  compile-command: "gnatmake queens.adb && ./queens"
--  End:

