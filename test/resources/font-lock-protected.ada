package body Test is

   protected type Protected_Type_Declaration is
      --   ^    ^                             ^ font-lock-keyword-face
      --          ^ font-lock-type-face
   end Protected_Type_Declaration;
   -- <- font-lock-keyword-face
   --  ^ font-lock-type-face


   protected Single_Protected_Declaration is
      --   ^                              ^ font-lock-keyword-face
      --     ^ font-lock-variable-name-face
   end Single_Protected_Declaration;
   -- <- font-lock-keyword-face
   --  ^ font-lock-variable-name-face


   protected body Protected_Body is
      --   ^    ^                 ^ font-lock-keyword-face
      --          ^ font-lock-variable-name-face
   end Protected_Body;
   -- <- font-lock-keyword-face
   --  ^ font-lock-variable-name-face

   protected body Protected_Body is separate;
   --      ^    ^                 ^        ^ font-lock-keyword-face
   --             ^ font-lock-variable-name-face

end Test;
