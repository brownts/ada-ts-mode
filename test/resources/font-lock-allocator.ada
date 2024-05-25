procedure P is
begin

   ------------------------
   -- subtype_indication --
   ------------------------

   -- subtype_mark: identifier
   A := new T;
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face

   -- subtype_mark: selected_component
   A := new XYZ.T;
   --   ^^^ font-lock-keyword-face
   --       ^^^ nil
   --          ^ font-lock-delimiter-face
   --           ^ font-lock-type-face

   -- subtype_mark: attribute_reference (identifier)
   A := new T'Base;
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^ nil
   --         ^^^^ font-lock-property-use-face

   -- subtype_mark: attribute_reference (selected_component)
   A := new XYZ.T'Base;
   --   ^^^ font-lock-keyword-face
   --       ^^^  ^ nil
   --          ^ font-lock-delimiter-face
   --           ^ font-lock-type-face
   --             ^^^^ font-lock-property-use-face

   -- null_exclusion
   A := new not null T;
   --   ^^^ ^^^ ^^^^ font-lock-keyword-face
   --                ^ font-lock-type-face

   -- constraint: index_constraint
   A := new T (1 .. 10, 1 .. 20);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --         ^                ^ font-lock-bracket-face
   --          ^    ^^  ^    ^^ font-lock-number-face
   --            ^^       ^^ font-lock-operator-face
   --                 ^ font-lock-delimiter-face

   -- constraint: discriminant_constraint (numeric_literal)
   A := new T (100);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --         ^   ^ font-lock-bracket-face
   --          ^^^ font-lock-number-face

   -- constraint: discriminant_constraint (discriminant_association)
   A := new T (F => 100);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --         ^        ^ font-lock-bracket-face
   --          ^ font-lock-property-name-face
   --            ^^ font-lock-operator-face
   --               ^^^ font-lock-number-face

   -- constraint: discriminant_constraint (identifier)
   A := new T (F);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --         ^ ^ font-lock-bracket-face

   --------------------------
   -- qualified_expression --
   --------------------------

   -- subtype_mark: identifier
   -- constraint: positional_array_aggregate
   A := new T'(0, 255, 0);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^ nil
   --         ^         ^ font-lock-bracket-face
   --          ^  ^^^  ^ font-lock-number-face
   --           ^    ^ font-lock-delimiter-face

   -- subtype_mark: selected_component
   -- constraint: positional_array_aggregate
   A := new XYZ.T'(0, 255, 0);
   --   ^^^ font-lock-keyword-face
   --       ^^^  ^ nil
   --          ^    ^    ^ font-lock-delimiter-face
   --           ^ font-lock-type-face
   --             ^         ^ font-lock-bracket-face
   --              ^  ^^^  ^ font-lock-number-face

   -- aggregate: record_aggregate
   A := new T'(F => 1);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^ nil
   --         ^      ^ font-lock-bracket-face
   --          ^ font-lock-property-name-face
   --            ^^ font-lock-operator-face
   --               ^ font-lock-number-face

   -- aggregate: named_array_aggregate
   A := new T'(1 .. 10 => (1 .. 20 => 0.0));
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^ nil
   --         ^           ^              ^^ font-lock-bracket-face
   --          ^    ^^     ^    ^^    ^^^ font-lock-number-face
   --            ^^    ^^    ^^    ^^ font-lock-operator-face

   -- expression: numeric_literal
   A := new T'(55);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^ nil
   --         ^  ^ font-lock-bracket-face
   --          ^^ font-lock-number-face

   -- expression: identifier
   A := new T'(F);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^ nil
   --         ^ ^ font-lock-bracket-face

   -- subtype_mark: attribute_reference (identifier)
   A := new T'Base'(5);
   --   ^^^ font-lock-keyword-face
   --       ^ font-lock-type-face
   --        ^    ^ nil
   --         ^^^^ font-lock-property-use-face
   --              ^ ^ font-lock-bracket-face
   --               ^ font-lock-number-face

   -- subtype_mark: attribute_reference (selected_component)
   A := new XYZ.T'Base'(5);
   --   ^^^ font-lock-keyword-face
   --       ^^^  ^    ^ nil
   --          ^ font-lock-delimiter-face
   --           ^ font-lock-type-face
   --             ^^^^ font-lock-property-use-face
   --                  ^ ^ font-lock-bracket-face
   --                   ^ font-lock-number-face

   ---------------------------
   -- subpool_specification --
   ---------------------------

   -- subtype_indication
   A := new (pkg.pool) T;
   --   ^^^ font-lock-keyword-face
   --       ^        ^ font-lock-bracket-face
   --                  ^ font-lock-type-face


   -- quantified_expression
   A := new (pkg.pool) T'(55);
   --   ^^^ font-lock-keyword-face
   --       ^        ^   ^  ^ font-lock-bracket-face
   --                  ^ font-lock-type-face
   --                   ^ nil
   --                     ^^ font-lock-number-face

end P;
