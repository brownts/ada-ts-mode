package body Test is

  package My_Test is new My_Test_Template;
  -- <-           ^  ^ font-lock-keyword-face
  --      ^              ^ nil

  package My_Test is new My_Test_Template (Foo, Bar);
  -- <-           ^  ^ font-lock-keyword-face
  --      ^              ^ nil

  package My_Test is new XYZ.My_Test_Template;
  -- <-           ^  ^ font-lock-keyword-face
  --      ^              ^   ^ nil

  package My_Test is new XYZ.My_Test_Template (Foo, Bar);
  -- <-           ^  ^ font-lock-keyword-face
  --      ^              ^   ^ nil

  procedure My_Test is new My_Test_Template;
  -- <-             ^  ^ font-lock-keyword-face
  --        ^              ^ font-lock-function-name-face

  procedure My_Test is new My_Test_Template (Foo, Bar);
  -- <-             ^  ^ font-lock-keyword-face
  --        ^              ^ font-lock-function-name-face

  procedure My_Test is new XYZ.My_Test_Template;
  -- <-             ^  ^ font-lock-keyword-face
  --                       ^ nil
  --        ^                  ^ font-lock-function-name-face

  procedure My_Test is new XYZ.My_Test_Template (Foo, Bar);
  -- <-             ^  ^ font-lock-keyword-face
  --                       ^ nil
  --        ^                  ^ font-lock-function-name-face

  function My_Test is new My_Test_Template;
  -- <-            ^  ^ font-lock-keyword-face
  --       ^              ^ font-lock-function-name-face

  function My_Test is new "=";
  -- <-            ^  ^ font-lock-keyword-face
  --       ^              ^ font-lock-function-name-face

  function My_Test is new My_Test_Template (Foo, Bar);
  -- <-            ^  ^ font-lock-keyword-face
  --       ^              ^ font-lock-function-name-face

  function My_Test is new "=" (Foo, Bar);
  -- <-            ^  ^ font-lock-keyword-face
  --       ^              ^ font-lock-function-name-face

  function My_Test is new XYZ.My_Test_Template;
  -- <-            ^  ^ font-lock-keyword-face
  --                      ^ nil
  --       ^                  ^ font-lock-function-name-face

  function My_Test is new XYZ."=";
  -- <-            ^  ^ font-lock-keyword-face
  --                      ^ nil
  --       ^                  ^ font-lock-function-name-face

  function My_Test is new XYZ.My_Test_Template (Foo, Bar);
  -- <-            ^  ^ font-lock-keyword-face
  --                      ^ nil
  --       ^                  ^ font-lock-function-name-face

  function My_Test is new XYZ."=" (Foo, Bar);
  -- <-            ^  ^ font-lock-keyword-face
  --                      ^ nil
  --       ^                  ^ font-lock-function-name-face

end Test;
