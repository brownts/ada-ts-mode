package body Test is
begin

   Foo;
   -- <- font-lock-function-call-face

   Foo (5);
   -- <- font-lock-function-call-face

   XYZ.Foo;
   -- <- nil
   --  ^ font-lock-function-call-face

   XYZ.Foo (5);
   -- <- nil
   --  ^ font-lock-function-call-face

   Foo.all;
   -- <- nil
   --  ^ font-lock-keyword-face

   Foo.all (5);
   -- <- nil
   --  ^ font-lock-keyword-face

   Foo.ALL;
   -- <- nil
   --  ^ font-lock-keyword-face

   Foo.ALL (5);
   -- <- nil
   --  ^ font-lock-keyword-face

   XYZ.Foo.all;
   -- <-^ nil
   --      ^ font-lock-keyword-face

   XYZ.Foo.all (5);
   -- <-^ nil
   --      ^ font-lock-keyword-face

   XYZ.Foo.ALL;
   -- <-^ nil
   --      ^ font-lock-keyword-face

   XYZ.Foo.ALL (5);
   -- <-^ nil
   --      ^ font-lock-keyword-face

   XYZ.Foo.Ball;
   -- <-^ nil
   --      ^ font-lock-function-call-face

   XYZ.Foo.Ball (5);
   -- <-^ nil
   --      ^ font-lock-function-call-face

   XYZ.Foo.all.Ball;
   -- <-^ nil
   --      ^ font-lock-keyword-face
   --          ^ font-lock-function-call-face

   XYZ.Foo.all.Ball (5);
   -- <-^ nil
   --      ^ font-lock-keyword-face
   --          ^ font-lock-function-call-face

   Foo'Class'Read (Bar);
   -- <- nil
   --  ^ font-lock-property-use-face
   --        ^ (font-lock-function-call-face font-lock-property-use-face)

   XYZ.Foo'Class'Read (Bar);
   -- <-^ nil
   --      ^ font-lock-property-use-face
   --            ^ (font-lock-function-call-face font-lock-property-use-face)

   Foo'Something;
   -- <- nil
   --  ^ (font-lock-function-call-face font-lock-property-use-face)

   XYZ.Foo'Something;
   -- <-^ nil
   --      ^ (font-lock-function-call-face font-lock-property-use-face)

end Test;
