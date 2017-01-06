with Ada.Text_IO; use Ada.Text_IO;

procedure Pi is
    base : integer := 32768;
    procedure mul is
    begin
        print_int__(base);
    end;
    procedure pi is
    begin
        print_int__(base);
        mul;
        print_int__(base);
    end;
begin
    pi;
end Pi;
