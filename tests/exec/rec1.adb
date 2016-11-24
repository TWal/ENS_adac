with Ada.Text_IO; use Ada.Text_IO;
procedure Test_a is
   procedure R(N: Integer) is
   begin
      if N = 0 then return; end if;
      Put('a');
      R(N - 1);
   end;
begin
   R(42);
   New_Line;
end TEST_a;

