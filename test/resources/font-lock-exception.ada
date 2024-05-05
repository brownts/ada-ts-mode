package body Test is

   ---------------------------
   -- exception_declaration --
   ---------------------------

   Foo_Exception : exception;
   -- <- font-lock-type-face
   --            ^          ^ font-lock-delimiter-face
   --              ^ font-lock-keyword-face

   Foo_Exception, Bar_Exception : exception;
   -- <-          ^ font-lock-type-face
   --           ^               ^          ^ font-lock-delimiter-face
   --                             ^ font-lock-keyword-face

   ------------------------------------
   -- exception_renaming_declaration --
   ------------------------------------

   Bar_Exception : exception renames Foo_Exception;
   -- <-                             ^ font-lock-type-face
   --            ^                                ^ font-lock-delimiter-face
   --              ^         ^ font-lock-keyword-face

   Bar_Exception : exception renames Test.Foo_Exception;
   -- <-                                  ^ font-lock-type-face
   --            ^                       ^             ^ font-lock-delimiter-face
   --              ^         ^ font-lock-keyword-face
   --                                ^ nil

   ----------------------
   -- raise_expression --
   ----------------------

   function Raise_Error return Integer is
     (raise Foo_Exception);
   -- ^ (font-lock-operator-face font-lock-keyword-face)
   --       ^ font-lock-type-face

   function Raise_Error return Integer is
     (raise Foo_Exception with "message");
   -- ^ (font-lock-operator-face font-lock-keyword-face)
   --       ^ font-lock-type-face
   --                     ^ font-lock-keyword-face
   --                          ^ font-lock-string-face

   function Raise_Error return Integer is
     (raise Test.Foo_Exception);
   -- ^ (font-lock-operator-face font-lock-keyword-face)
   --       ^ nil
   --           ^ font-lock-delimiter-face
   --            ^ font-lock-type-face

   function Raise_Error return Integer is
     (raise Test.Foo_Exception with "message");
   -- ^ (font-lock-operator-face font-lock-keyword-face)
   --       ^ nil
   --           ^ font-lock-delimiter-face
   --            ^ font-lock-type-face
   --                          ^ font-lock-keyword-face
   --                               ^ font-lock-string-face

   ---------------------
   -- raise_statement --
   ---------------------

   procedure Raise_Error is
      X : Integer;
   begin
      X := 5 / 0;
   exception
      when others =>
         Put_Line ("exception handler");
         raise;
      -- ^ (font-lock-operator-face font-lock-keyword-face)
      --      ^ font-lock-delimiter-face
   end ABC;

   procedure Raise_Error is
   begin
      raise Foo_Exception;
      -- ^ (font-lock-operator-face font-lock-keyword-face)
      --    ^ font-lock-type-face
      --                 ^ font-lock-delimiter-face
   end ABC;

   procedure Raise_Error is
   begin
      raise Foo_Exception with "message";
      -- ^ (font-lock-operator-face font-lock-keyword-face)
      --    ^ font-lock-type-face
      --                  ^ font-lock-keyword-face
      --                       ^ font-lock-string-face
      --                                ^ font-lock-delimiter-face
   end ABC;

   procedure Raise_Error is
   begin
      raise Test.Foo_Exception;
      -- ^ (font-lock-operator-face font-lock-keyword-face)
      --    ^ nil
      --        ^             ^ font-lock-delimiter-face
      --         ^ font-lock-type-face
   end ABC;

   procedure Raise_Error is
   begin
      raise Test.Foo_Exception with "message";
      -- ^ (font-lock-operator-face font-lock-keyword-face)
      --    ^ nil
      --        ^                            ^ font-lock-delimiter-face
      --         ^ font-lock-type-face
      --                       ^ font-lock-keyword-face
      --                            ^ font-lock-string-face
   end ABC;

begin

   Raise_Error;

exception

   -----------------------
   -- exception_handler --
   -----------------------

   when Foo_Exception =>
      -- <- font-lock-keyword-face
      --^ font-lock-type-face
      --              ^ font-lock-operator-face
      null;

   when X : Foo_Exception =>
      -- <- font-lock-keyword-face
      --^ (font-lock-constant-face font-lock-variable-name-face)
      --  ^ font-lock-delimiter-face
      --    ^ font-lock-type-face
      --                  ^ font-lock-operator-face
      null;

   when X : Test.Foo_Exception =>
      -- <- font-lock-keyword-face
      --^ (font-lock-constant-face font-lock-variable-name-face)
      --  ^     ^ font-lock-delimiter-face
      --    ^ nil
      --         ^ font-lock-type-face
      --                       ^ font-lock-operator-face
      null;

   when X : Foo_Exception | Bar_Exception =>
      -- <- font-lock-keyword-face
      --^ (font-lock-constant-face font-lock-variable-name-face)
      --  ^ font-lock-delimiter-face
      --    ^               ^ font-lock-type-face
      --                  ^               ^ font-lock-operator-face
      null;

   when X : Test.Foo_Exception | Test.Bar_Exception =>
      -- <- font-lock-keyword-face
      --^ (font-lock-constant-face font-lock-variable-name-face)
      --  ^     ^                    ^ font-lock-delimiter-face
      --    ^                    ^ nil
      --         ^                    ^ font-lock-type-face
      --                       ^                    ^ font-lock-operator-face
      null;

   when others =>
      -- <-^ font-lock-keyword-face
      --       ^ font-lock-operator-face
      null;

   when Foo_Exception | others =>
      -- <-             ^ font-lock-keyword-face
      -- ^ font-lock-type-face
      --              ^        ^ font-lock-operator-face
      null;

   when X : Foo_Exception | others =>
      -- <-                 ^ font-lock-keyword-face
      --  ^ font-lock-delimiter-face
      --    ^ font-lock-type-face
      --                  ^        ^ font-lock-operator-face
      --^ (font-lock-constant-face font-lock-variable-name-face)
      null;

end Test;
