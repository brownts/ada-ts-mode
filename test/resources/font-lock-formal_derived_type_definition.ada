generic
   type Test_Type is abstract new Base_Type;
   -- <-          ^  ^        ^ font-lock-keyword-face
   --   ^                         ^ font-lock-type-face
   --                                      ^ font-lock-delimiter-face

   type Test_Type is abstract new ABC.Base_Type;
   -- <-          ^  ^        ^ font-lock-keyword-face
   --   ^                             ^ font-lock-type-face
   --                             ^ nil
   --                                ^         ^ font-lock-delimiter-face

   type Test_Type is limited new Base_Type;
   -- <-          ^  ^       ^ font-lock-keyword-face
   --   ^                        ^ font-lock-type-face
   --                                     ^ font-lock-delimiter-face

   type Test_Type is synchronized new Base_Type;
   -- <-          ^  ^            ^ font-lock-keyword-face
   --   ^                             ^ font-lock-type-face
   --                                          ^ font-lock-delimiter-face

   type Test_Type is new Base_Type and Interface_Type and ABC.Interface_Type with private;
   -- <-          ^  ^             ^                  ^                      ^    ^ font-lock-keyword-face
   --   ^                ^             ^                      ^ font-lock-type-face
   --                                                     ^ nil
   --                                                        ^                           ^ font-lock-delimiter-face
package Test is

end Test;
