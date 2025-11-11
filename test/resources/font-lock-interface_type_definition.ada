package Test is
   type Interface_Type is interface;
   -- <-               ^  ^ font-lock-keyword-face
   --   ^ font-lock-type-face
   --                              ^ font-lock-delimiter-face

   type Interface_Type is limited interface;
   -- <-               ^  ^       ^ font-lock-keyword-face
   --   ^ font-lock-type-face
   --                                      ^ font-lock-delimiter-face

   type Interface_Type is task interface;
   -- <-               ^  ^    ^ font-lock-keyword-face
   --   ^ font-lock-type-face
   --                                   ^ font-lock-delimiter-face

   type Interface_Type is protected interface;
   -- <-               ^  ^         ^ font-lock-keyword-face
   --   ^ font-lock-type-face
   --                                        ^ font-lock-delimiter-face

   type Interface_Type is synchronized interface;
   -- <-               ^  ^            ^ font-lock-keyword-face
   --   ^ font-lock-type-face
   --                                           ^ font-lock-delimiter-face

   type Interface_Type is interface and Interface2_Type;
   -- <-               ^  ^         ^ font-lock-keyword-face
   --   ^                               ^ font-lock-type-face
   --                                                  ^ font-lock-delimiter-face

   type Interface_Type is interface and Interface2_Type and Interface3_Type;
   -- <-               ^  ^         ^                   ^ font-lock-keyword-face
   --   ^                               ^                   ^ font-lock-type-face
   --                                                                      ^ font-lock-delimiter-face

   type Interface_Type is interface and ABC.Interface2_Type;
   -- <-               ^  ^         ^ font-lock-keyword-face
   --   ^                                   ^ font-lock-type-face
   --                                   ^ nil
   --                                      ^               ^ font-lock-delimiter-face
end Test;
