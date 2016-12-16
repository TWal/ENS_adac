with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
    sum: Integer;
    procedure Bla(x, y: Integer) is
        foo : Integer;
        bar : Integer;
        procedure Blu(z, w : Integer) is
        begin
            foo := z + w;
            bar := z - w;
            sum := sum + foo;
            sum := sum + bar;
        end;
    begin
        Blu(x, y);
        print_int__(foo);
        print_int__(bar);
    end;
begin
    sum := 0;
    print_int__(sum);
    bla(42, 57);
    print_int__(sum);
    bla(1337, -50);
    print_int__(sum);
end;


