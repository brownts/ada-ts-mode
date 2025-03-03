package Test is
   for Record_Type use record
   -- <-           ^   ^ font-lock-keyword-face
   --  ^ font-lock-type-face
      at mod 4;
      -- <-^ font-lock-keyword-face
      --     ^ font-lock-number-face
      --      ^ font-lock-delimiter-face
      X at 0 range 0 .. 31;
      -- <- font-lock-property-name-face
      -- ^   ^ font-lock-keyword-face
      --   ^       ^    ^ font-lock-number-face
      --             ^ font-lock-operator-face
      --                  ^ font-lock-delimiter-face
      Y at 4 range 0 .. 31;
   end record Record_Type;
   -- <-^ font-lock-keyword-face
   --         ^ font-lock-type-face
   --                    ^ font-lock-delimiter-face
end Test;
