with Ada.Text_IO; use Ada.Text_IO;

procedure Test is

   type R is record A, B: Integer; end record;

   procedure P1(X, Y: in out R) is
   begin
      X.A := 0;
      X.B := 1;
      Y.A := 2;
      Y.B := 3;
      if X = Y then Put('a'); end if;
      X := Y;
      if X = Y then Put('d'); end if;
   end;

   type T is access R;
   procedure P2(X, Y: in out T) is
   begin
      X.A := 0;
      X.B := 1;
      Y.A := 2;
      Y.B := 3;
      if X = Y then Put('a'); end if;
      X := Y;
      if X = Y then Put('d'); end if;
   end;

   X,Y: R;
   P,Q: T;

begin
   X.A := 0;
   X.B := 1;
   Y.A := 2;
   Y.B := 3;
   if X = Y then Put('a'); end if;
   X := Y;
   if X = Y then Put('b'); end if;

   X.A := 0;
   X.B := 1;
   Y.A := 2;
   Y.B := 3;
   if X = Y then Put('a'); end if;
   P1(X, Y);
   if X = Y then Put('e'); end if;

   P := new R;
   Q := new R;
   P.A := 0;
   P.B := 1;
   Q.A := 0;
   Q.B := 1;
   if P = Q then Put('a'); end if;

   P2(P, Q);
   Q.B := 42;
   if P = Q then Put('f'); end if;

   New_line;

end;

--  Local Variables:
--  compile-command: "gnatmake record1.adb && ./record1"
--  End:
