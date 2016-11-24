with Ada.Text_IO; use Ada.Text_IO;

procedure Record4 is

   type R is record A, B: Character; end record;
   type S is record C: Character; D: R; E: Character; end record;

   V, W: R;
   X, Y: S;

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

begin
   V.A := '1';
   V.B := '2';
   X.C := '0';
   X.D := V;
   X.E := '3';

   W.A := 'b';
   W.B := 'c';
   Y.C := 'a';
   Y.D := W;
   Y.E := 'd';

   PrintS(X); New_Line;
   PrintS(Y); New_Line;

   if X = Y then Put('*'); else Put('.'); end if;
   if X.D = W then Put('*'); else Put('.'); end if;
   if X.D = Y.D then Put('*'); else Put('.'); end if;
   if X.D.A = Y.D.A then Put('*'); else Put('.'); end if;
   X.D := Y.D;
   if X.D = W then Put('*'); else Put('.'); end if;
   if X.D = Y.D then Put('*'); else Put('.'); end if;
   if X.D.A = Y.D.A then Put('*'); else Put('.'); end if;
   X := Y;
   if X = Y then Put('*'); else Put('.'); end if;
   if X.D = W then Put('*'); else Put('.'); end if;
   if X.D = Y.D then Put('*'); else Put('.'); end if;
   if X.D.A = Y.D.A then Put('*'); else Put('.'); end if;
   New_Line;
end Record4;

--  Local Variables:
--  compile-command: "gnatmake record4.adb && ./record4"
--  End:
