package body Test is

   task type Task_Type_Declaration is new Interface_Type and ABC.Interface_Type with
      -- <-^                        ^ ^                  ^                      ^ font-lock-keyword-face
      --     ^                            ^                      ^ font-lock-type-face
      --                                                     ^ nil
      --                                                        ^ font-lock-delimiter-face
   end Task_Type_Declaration;
   -- <- font-lock-keyword-face
   --  ^ font-lock-type-face


   task Single_Task_Declaration is new Interface_Type and ABC.Interface_Type with
      -- <-                      ^ ^                  ^                      ^ font-lock-keyword-face
      -- ^ font-lock-variable-name-face
      --                               ^                      ^ font-lock-type-face
      --                                                  ^ nil
      --                                                     ^ font-lock-delimiter-face
   end Single_Task_Declaration;
   -- <- font-lock-keyword-face
   --  ^ font-lock-variable-name-face


   task body Task_Body is
      -- <-^            ^ font-lock-keyword-face
      --     ^ font-lock-variable-name-face
   begin
      -- <- font-lock-keyword-face
      null;
   end Task_Body;
   -- <- font-lock-keyword-face
   --  ^ font-lock-variable-name-face

   task body Task_Body is separate;
   -- ^    ^            ^        ^ font-lock-keyword-face
   --        ^ font-lock-variable-name-face

end Test;
