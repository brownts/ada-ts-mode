package Test is
   type Record_Type is abstract tagged null record;
   -- <-            ^  ^        ^      ^    ^ font-lock-keyword-face
   --   ^ font-lock-type-face
   --                                             ^ font-lock-delimiter-face

   type Record_Type (O : Boolean) is limited record
      -- <-                       ^  ^       ^ font-lock-keyword-face
      --            ^           ^ font-lock-bracket-face
      --               ^ font-lock-delimiter-face
      --             ^ (font-lock-constant-face font-lock-variable-name-face)
      --                 ^ font-lock-type-face
      X : Integer;
      -- <- font-lock-property-name-face
      --^        ^ font-lock-delimiter-face
      Y : Integer;
      case O is
         -- <-^ font-lock-keyword-face
         --^ (font-lock-constant-face font-lock-variable-name-face)
         when True =>
            -- <- font-lock-keyword-face
            --     ^^ font-lock-operator-face
            Z : Integer;
         when others =>
            null;
      end case;
      -- <-^ font-lock-keyword-face
      --      ^ font-lock-delimiter-face
   end record Record_Type;
   -- <-^ font-lock-keyword-face
   --         ^ font-lock-type-face
   --                    ^ font-lock-delimiter-face
end Test;
