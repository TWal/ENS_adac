with Ada.Text_IO; use Ada.Text_IO;

procedure Record3 is

   type R is record A, B: Character; end record;
   type S is record C: Character; D: R; E: Character; end record;

   procedure PrintR(x: R) is
   begin
      Put('[');
      Put(x.A);
      Put(',');
      Put(x.B);
      Put(']');
   end;
   procedure PrintS(x: S) is
   begin
      Put('[');
      Put(x.C);
      Put(',');
      PrintR(x.D);
      Put(',');
      Put(x.E);
      Put(']');

   end;

   V: R;
   X,Y: S;

begin
   V.A := '1';
   V.B := '2';
   X.C := '0';
   X.D := V;
   X.E := '3';
   PrintS(X); New_Line;
   X.D.A := 'l';
   PrintS(X); New_Line;
   Y := X;
   PrintS(Y); New_Line;
end Record3;

--  Local Variables:
--  compile-command: "gnatmake record3.adb && ./record3"
--  End:
