package Test is
   function Factorial (N : Natural) return Natural is
     ([parallel (10) for J in 1 .. N => J]'Reduce("*", 1));
   -- ^         ^  ^                     ^       ^      ^ font-lock-bracket-face
   --  ^ font-lock-keyword-face
   --                                     ^ nil
   --                                      ^ (font-lock-function-call-face font-lock-property-use-face)
   --                                             ^ font-lock-function-name-face
   --                                                ^ font-lock-delimiter-face

   type N_Array is array (Integer range <>) of Natural;

   function Sum (X : N_Array) return Natural is
     (X'Reduce("+", 0));
   --  ^ nil
   --   ^ (font-lock-function-call-face font-lock-property-use-face)
   --         ^      ^ font-lock-bracket-face
   --             ^ font-lock-delimiter-face
end Test;
