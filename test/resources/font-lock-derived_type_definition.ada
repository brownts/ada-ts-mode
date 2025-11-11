package Test is
   type Derived_Type is new Base_Type;
   -- <-             ^  ^ font-lock-keyword-face
   --   ^                   ^ font-lock-type-face
   --                                ^ font-lock-delimiter-face

   type Derived_Type is new ABC.Base_Type;
   -- <-             ^  ^ font-lock-keyword-face
   --   ^                       ^ font-lock-type-face
   --                       ^ nil
   --                          ^         ^ font-lock-delimiter-face

   type Derived_Type is abstract limited new Base_Type;
   -- <-             ^  ^        ^       ^ font-lock-keyword-face
   --   ^                                    ^ font-lock-type-face
   --                                                 ^ font-lock-delimiter-face

   type Derived_Type is new Base_Type with null record;
   -- <-             ^  ^             ^    ^    ^ font-lock-keyword-face
   --   ^                   ^ font-lock-type-face
   --                                                 ^ font-lock-delimiter-face

   type Derived_Type is new Base_Type and Interface1_Type with null record;
   -- <-             ^  ^             ^                   ^    ^    ^ font-lock-keyword-face
   --   ^                   ^             ^ font-lock-type-face
   --                                                                     ^ font-lock-delimiter-face

   type Derived_Type is new Base_Type and Interface1_Type and Interface2_Type with null record;
   -- <-             ^  ^             ^                   ^                   ^    ^    ^ font-lock-keyword-face
   --   ^                   ^             ^                   ^ font-lock-type-face
   --                                                                                         ^ font-lock-delimiter-face

   type Derived_Type is new Base_Type and ABC.Interface1_Type with null record;
   -- <-             ^  ^             ^                       ^    ^    ^ font-lock-keyword-face
   --   ^                   ^                 ^ font-lock-type-face
   --                                     ^ nil
   --                                        ^                                ^ font-lock-delimiter-face
end Test;
