package body Test is

   A1 : Address := System'To_Address (16#0000_0000#);
   --              ^ nil
   --                     ^ (font-lock-function-call-face font-lock-property-use-face)

   A2 : Address := System.To_Address (16#0000_0000#);
   --              ^ nil
   --                     ^ font-lock-function-call-face

   I1 : Integer := Integer'Min (5, 6);
   --              ^ nil
   --                      ^ (font-lock-function-call-face font-lock-property-use-face)

   I2 : Integer := Standard.Integer'Min (5, 6);
   --              ^        ^ nil
   --                               ^ (font-lock-function-call-face font-lock-property-use-face)

   I3 : Integer := Integer (5);
   --              ^ font-lock-function-call-face

   I4 : Integer := Standard.Integer (5);
   --              ^ nil
   --                       ^ font-lock-function-call-face

   X1 : Integer := Foo.all (5);
   --              ^ nil
   --                  ^ font-lock-keyword-face

   X2 : Integer := Foo.ALL (5);
   --              ^ nil
   --                  ^ font-lock-keyword-face

   X3 : Integer := XYZ.Foo.all (5);
   --              ^   ^ nil
   --                      ^ font-lock-keyword-face

   X4 : Integer := XYZ.Foo.ALL (5);
   --              ^   ^ nil
   --                      ^ font-lock-keyword-face

   X5 : Integer := XYZ.Foo.Ball (5);
   --              ^   ^ nil
   --                      ^ font-lock-function-call-face

   X6 : Integer := XYZ.Foo.BALL (5);
   --              ^   ^ nil
   --                      ^ font-lock-function-call-face

   X7 : Integer := Foo'Reduce ("+", 0);
   --              ^ nil
   --                  ^ (font-lock-function-call-face font-lock-property-use-face)

   X8 : Integer := XYZ.Foo'Reduce ("+", 0);
   --              ^   ^ nil
   --                      ^ (font-lock-function-call-face font-lock-property-use-face)

   X9 : Integer := [for J in 1 .. 10 => J]'Reduce ("*", 1);
   --                                      ^ (font-lock-function-call-face font-lock-property-use-face)

   X10 : Integer := Integer'Mod (5);
   --               ^ nil
   --                       ^ (font-lock-function-call-face font-lock-property-use-face)

   X11 : Integer := "+" (1, 2);
   --               ^ font-lock-function-call-face

   X12 : Integer := XYZ."+" (1, 2);
   --               ^ nil
   --                   ^ font-lock-function-call-face

end Test;
