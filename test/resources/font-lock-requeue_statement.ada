package body Test is
   protected body XYZ is
      entry ABC when X > Y is
      begin
         requeue Foo;
         -- <- font-lock-keyword-face
         requeue Foo with abort;
         -- <-       ^    ^ font-lock-keyword-face
      end ABC;
   end XYZ;
end Test;
