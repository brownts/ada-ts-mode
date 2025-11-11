package Test is
   type Test_Type is new Base_Type and Interface1_Type with private;
   -- <-          ^  ^             ^                   ^    ^ font-lock-keyword-face
   --   ^                ^             ^ font-lock-type-face;
   --                                                              ^ font-lock-delimiter-face

   type Test_Type is new ABC.Base_Type and Interface1_Type with private;
   -- <-          ^  ^                 ^                   ^    ^ font-lock-keyword-face
   --   ^                    ^             ^ font-lock-type-face;
   --                    ^ nil
   --                       ^                                          ^ font-lock-delimiter-face

   type Test_Type is new Base_Type and Interface1_Type and Interface2_Type with private;
   -- <-          ^  ^             ^                   ^                   ^    ^ font-lock-keyword-face
   --   ^                ^             ^                   ^ font-lock-type-face;
   --                                                                                  ^ font-lock-delimiter-face

   type Test_Type is new Base_Type and ABC.Interface1_Type with private;
   -- <-          ^  ^             ^                       ^    ^ font-lock-keyword-face
   --   ^                ^                 ^ font-lock-type-face;
   --                                  ^ nil
   --                                     ^                            ^ font-lock-delimiter-face
end Test;
