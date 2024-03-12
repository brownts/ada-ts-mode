-------------------------------------
-- expression_function_declaration --
-------------------------------------

function "+" (Left, Right : Integer) return Integer is (Left + Right);
--       ^ font-lock-function-name-face

----------------------------
-- subprogram_declaration --
----------------------------

function "+" (Left, Right : Integer) return Integer;
--       ^ font-lock-function-name-face

function ABC."+" (Left, Right : Integer) return Integer;
--       ^ nil
--           ^ font-lock-function-name-face

package Test is
   function "+" (Left, Right : Integer) return Integer;
   --       ^ font-lock-function-name-face
end Test;

---------------------
-- subprogram_body --
---------------------

function "+" (Left, Right : Integer) return Integer is
   --    ^ font-lock-function-name-face
begin
   return "+" (Left, Right);
   --     ^ font-lock-function-call-face
end "+";
--  ^ font-lock-function-name-face

function ABC."+" (Left, Right : Integer) return Integer is
   --    ^ nil
   --        ^ font-lock-function-name-face
begin
   return XYZ."+" (Left, Right);
   --     ^ nil
   --         ^ font-lock-function-call-face
end ABC."+";
--  ^ nil
--      ^ font-lock-function-name-face

package body Test is
   function "+" (Left, Right : Integer) return Integer is
   --        ^ font-lock-function-name-face
   begin
      return XYZ."+" (Left, Right);
      --     ^ nil
      --         ^ font-lock-function-call-face
   end "+";
   --  ^ font-lock-function-name-face
end Test;

--------------------------
-- subprogram_body_stub --
--------------------------

package body Test is
   function "+" (Left, Right : Integer) return Integer is separate;
   --       ^ font-lock-function-name-face
end Test;

-----------------------------
-- subunit subprogram_body --
-----------------------------

separate (Test)
function "+" (Left, Right : Integer) return Integer is
   --    ^ font-lock-function-name-face
begin
   return "+" (Left, Right);
   --     ^ font-lock-function-call-face
end "+";
--  ^ font-lock-function-name-face

-------------------------------------
-- subprogram_renaming_declaration --
-------------------------------------

function "+" (Left, Right : Integer) return Integer renames "+";
--       ^                                                  ^ font-lock-function-name-face

function ABC."+" (Left, Right : Integer) return Integer renames XYZ."+";
--       ^                                                      ^ nil
--           ^                                                       ^ font-lock-function-name-face

------------------------------------
-- generic_subprogram_declaration --
-------------------------------------

generic
function "+" (Left, Right : Integer) return Integer;
--       ^ font-lock-function-name-face

generic
function ABC."+" (Left, Right : Integer) return Integer;
--       ^ nil
--           ^ font-lock-function-name-face

---------------------------
-- generic_instantiation --
---------------------------

function "+" is new "+";
--       ^          ^ font-lock-function-name-face

function ABC."+" is new XYZ."+";
--       ^              ^ nil
--           ^              ^ font-lock-function-name-face

----------------------------------
-- generic_renaming_declaration --
----------------------------------

generic function "+" renames "+";
--               ^           ^ font-lock-function-name-face

generic function ABC."+" renames XYZ."+";
--               ^               ^ nil
--                   ^               ^ font-lock-function-name-face

-----------------------------------
-- formal_subprogram_declaration --
-----------------------------------

generic
   with function "+" (Left, Right : Integer) return Integer;
   --            ^ font-lock-function-name-face
   with function "+" (Left, Right : Integer) return Integer is abstract;
   --            ^ font-lock-function-name-face
   with function "+" (Left, Right : Integer) return Integer is "+";
   --            ^                                             ^ font-lock-function-name-face
   with function "+" (Left, Right : Integer) return Integer is ABC."+";
   --                                                          ^ nil
   --            ^                                                 ^ font-lock-function-name-face
package Test is
end Test;
