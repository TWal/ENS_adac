with Ada.Text_IO; use Ada.Text_IO;

procedure Power is

   procedure PrintInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N > 9 then PrintInt(N / 10); end if;
      Put(Character'Val(48 + C));
   end;
   procedure PrintSInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N >= 0 then Printint(N); return; end if;
      Put('-');
      if N < -9 then Printint(- N / 10); end if;
      Put(Character'Val(48 - C));
   end;

   function Power(X, N: Integer) return Integer is
      P: Integer;
   begin
      if N = 0 then return 1; end if;
      P := Power(X, N / 2);
      P := P * P;
      if N rem 2 = 1 then P := X * P; end if;
      return P;
   end;

begin
   PrintSInt(Power(2, 0)); New_Line;
   PrintSInt(Power(2, 8)); New_Line;
   PrintsInt(Power(2, 30)); New_Line;
   PrintsInt(Power(2, 31)); New_Line;
end;

--  Local Variables:
--  compile-command: "gnatmake power.adb && ./power"
--  End:

