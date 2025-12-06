procedure Example is
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, To_String (File_Name);
   while not Ada.Text_IO.End_Of_File (File) loop
      null;
   end loop;
end Example;
