package body Test is

   A  : array (1 .. 6) of Float;
   AA : array (Integer range 0 .. 2, Integer range 0 .. 3) of Float;

begin

   for I in A'Range loop
      --      ^^^^^ font-lock-property-use-face
      A (I) := 0.0;
   end loop;

   for I in AA'Range (1) loop
      --       ^^^^^ font-lock-property-use-face
      --             ^ ^ nil
      --              ^ font-lock-number-face
      for J in AA'Range (2) loop
         --       ^^^^^ font-lock-property-use-face
         --             ^ ^ nil
         --              ^ font-lock-number-face
         AA (I, J) := 0.0;
      end loop;
   end loop;

end Test;
