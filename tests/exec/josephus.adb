with Ada.Text_IO; use Ada.Text_IO;

-- le problème de Josephus, avec des listes doublement chaînées circulaires

procedure josephus is
   type Node;
   type List is access Node;
   type Node is record
      Value: Integer;
      Prev, Next: List;
   end record;
   function Singleton(V: Integer) return List is
      L: List := new Node;
   begin
      L.Value := V;
      L.Prev := L;
      L.Next := L;
      return L;
   end;
   procedure InsertAfter(L: List; V: Integer) is
      E: List := Singleton(V);
   begin
      E.Next := L.Next;
      L.Next := E;
      E.Next.Prev := E;
      E.Prev := L;
   end;
   procedure Remove(L: List) is
   begin
      L.Prev.Next := L.Next;
      L.Next.Prev := L.Prev;
   end;

   procedure PrintInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N > 9 then PrintInt(N / 10); end if;
      Put(Character'Val(48 + C));
   end;

   -- affiche la liste l, supposée bien formée (et non null)
   procedure Print(L: in List) is
      P: List;
   begin
      PrintInt(L.Value);
      P := L.Next;
      while P /= L loop
         Put(',');
         PrintInt(P.Value);
         P := P.Next;
      end loop;
      New_Line;
   end;

   -- construit la liste 1,2,...,n
   -- l'élément renvoyé est celui contenant 1
   function circle(N: Integer) return List is
      L: List := Singleton(1);
   begin
      for I in reverse 2 .. N loop
         InsertAfter(L, I);
      end loop;
      return L;
   end;

   function Josephus(N, P: Integer) return Integer is
      C: List;
   begin
      C := Circle(N);
      Print(C);
      while C /= C.Next loop
         for I in 1 .. P-1 loop
            C := C.Next;
         end loop;
         Remove(C);
         C := C.Next;
      end loop;
      return C.Value;
   end;

begin
   printint(josephus(7, 5)); -- 6
   New_Line;
   printint(josephus(5, 5)); -- 2
   New_Line;
   printint(josephus(5, 17)); -- 4
   New_Line;
   printint(josephus(13, 2)); -- 11
   New_Line;
end;

--  Local Variables:
--  compile-command: "gnatmake josephus.adb && ./josephus"
--  End:
