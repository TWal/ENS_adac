with Ada.Text_IO; use Ada.Text_IO;

-- arbres binaires de recherche

procedure BST is
   type Node;
   type BST is access Node;
   type Node is record
      Value: Integer;
      Left, Right: BST;
   end record;
   -- insertion utilisant le passage par référence
   procedure Add(X: Integer; B: in out BST) is
   begin
      if B = null then B := new Node; B.Value := X; return; end if;
      if X < B.Value then
         Add(X, B.Left);
      elsif X > B.Value then
         Add(X, B.Right);
      end if;
   end;
   function Mem(X: Integer; B: BST) return boolean is
   begin
      if B = null then return False; end if;
      if X < B.Value then
         return Mem(X, B.Left);
      elsif X > B.Value then
         return Mem(X, B.Right);
      else
         return True;
      end if;
   end;
   procedure PrintInt(N: Integer) is
      C: Integer := N rem 10;
   begin
      if N > 9 then PrintInt(N / 10); end if;
      Put(Character'Val(48 + C));
   end;
   procedure PrintBool(b: boolean) is
   begin
      if B then Put('T'); else Put('F'); end if;
   end;
   procedure Print(B: in BST) is
   begin
      if B = null then return; end if;
      Put('(');
      Print(B.Left);
      -- Put(',');
      PrintInt(B.Value);
      -- Put(',');
      Print(B.Right);
      Put(')');
   end;
   B: BST;
begin
   B := null;
   Print(B); New_Line;
   Add(5, B);
   Print(B); New_Line;
   Add(2, B);
   Print(B); New_Line;
   Add(7, B);
   Print(B); New_Line;
   for I in 1 .. 10 loop
      PrintBool(Mem(I, B));
      Add(I, B);
   end loop;
   New_Line;
   Print(B); New_Line;
end;

--  Local Variables:
--  compile-command: "gnatmake bst.adb && ./bst"
--  End:
