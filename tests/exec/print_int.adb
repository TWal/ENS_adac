with Ada.Text_IO; use Ada.Text_IO;

procedure Print_int is

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

begin
   PrintsInt(-2147483648);
   New_Line;
   for N in -17 .. 42 loop
      Printsint(N);
      New_Line;
   end loop;
   PrintsInt(2147483647);
   New_Line;
end;

--  Local Variables:
--  compile-command: "gnatmake print_int.adb && ./print_int"
--  End:

