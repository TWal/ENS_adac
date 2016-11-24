with Ada.Text_IO; use Ada.Text_IO;

procedure Mandelbrot is

   -- Ada 2012
   --  function Add(x, y: in Integer) return Integer is (X+Y);
   --  function Sub(x, y: in Integer) return Integer is (X-Y);
   --  function Mul(x, y: in Integer) return Integer is ((X * Y + 8192 / 2) / 8192);
   --  function Div(x, y: in Integer) return Integer is ((X * 8192 + y / 2) / Y);
   --  function OfInt(x: in Integer) return Integer is (X * 8192);

   function Add(x, y: in Integer) return Integer is
   begin
      return X+Y;
   end Add;
   function Sub(x, y: in Integer) return Integer is
   begin
      return X-Y;
   end Sub;
   function Mul(x, y: in Integer) return Integer is
      T : Integer := X * Y;
   begin
      return (t + 8192 / 2) / 8192;
   end Mul;
   function Div(x, y: in Integer) return Integer is
      T : Integer := X * 8192;
   begin
      return (t + y / 2) / y;
   end Div;
   function OfInt(x: in Integer) return Integer is
   begin
      return X * 8192;
   end OfInt;

   function iter(N, A, B, Xn, Yn: in Integer) return Boolean is
      xn2, Yn2 : Integer;
   begin
      if n = 100 then return true; end if;
      xn2 := mul(xn, xn);
      yn2 := mul(yn, yn);
      if add(xn2, yn2) > ofint(4) then return false; end if;
      return iter(n+1, a, b, add(sub(xn2, yn2), a),
                  add(mul(ofint(2), mul(xn, yn)), b));
   end;
   function inside(X, Y: in Integer) return Boolean is
   begin
      return iter(0, x, y, ofint(0), ofint(0));
   end;
   procedure Run(Steps: in Integer) is
      xmin, xmax, deltax, ymin, ymax, deltay, X, Y: Integer;
   begin
      xmin := ofint(-2);
      xmax := ofint(1);
      deltax := div(sub(xmax, xmin), ofint(2 * steps));
      ymin := ofint(-1);
      ymax := ofint(1);
      deltay := div(sub(ymax, ymin), ofint(steps));
      for i in 0 .. Steps-1 loop
         y := add(ymin, mul(ofint(i), deltay));
         for j in 0 .. 2 * Steps - 1 loop
            x := add(xmin, mul(ofint(j), deltax));
            if inside(x, y) then
               put('0');
            else
               put('1');
            end if;
         end loop;
         New_Line;
      end loop;
   end Run;

begin
   Run(30);
end Mandelbrot;

--  Local Variables:
--  compile-command: "gnatmake mandelbrot.adb && ./mandelbrot"
--  End:
