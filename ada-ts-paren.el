;;; ada-ts-paren.el --- Parenthesis Highlight support in Ada files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Troy Brown

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ada-ts-common)
(require 'paren)
(require 'treesit)

(ada-ts-mode--declare-treesit-functions)

;;;; `show-paren-mode' support

(defmacro ada-ts-paren--not-after-type-p (type)
  "Check if a node of type TYPE does not precede delimiter."
  `(lambda (n)
     (let* ((prev-n (ada-ts-mode--prev-node n))
            (prev-t (treesit-node-type prev-n)))
       (or (not prev-n)
           (not (member prev-t ,(or (and (consp type) type)
                                    `(quote ,(ensure-list type)))))))))

(defmacro ada-ts-paren--match-second-delimiter-p (s)
  "Check if a node of type S follows matching delimiter.

If S does not follow the matching delimiter, check if there is nothing
in the buffer after the trailing delimiter or if there is nothing
remaining on the line after the delimiter.  These are fallbacks to allow
matching with incomplete syntax."
  `(lambda (n)
     (let* ((next-n (ada-ts-mode--next-node n))
            (next-t (treesit-node-type next-n)))
       (or (null next-n)
           (string-equal next-t ,s)
           (save-excursion
             (goto-char (treesit-node-end n))
             (looking-at-p ,(rx (* whitespace) eol)))))))

(defun ada-ts-paren--match-block_statement-end-p (n)
  "Check if contents after N align with a block statement."
  (let* ((next-n (ada-ts-mode--next-node n))
         (next-t (treesit-node-type next-n)))
    (or (not next-n)
        (string-equal next-t ";")
        (and (string-equal next-t "identifier")
             (when-let* ((parent-n (treesit-node-parent n))
                         (parent-t (treesit-node-type parent-n))
                         ((string-equal parent-t "block_statement"))
                         (first-child-n (treesit-node-child parent-n 0))
                         (first-child-t (treesit-node-type first-child-n))
                         ((string-equal first-child-t "loop_label"))
                         (label-n (treesit-node-child-by-field-name first-child-n "statement_identifier")))
               (string-equal-ignore-case (treesit-node-text label-n 'no-property)
                                         (treesit-node-text next-n 'no-property))))
        (save-excursion
          (goto-char (treesit-node-end n))
          (looking-at-p (rx (* whitespace) eol))))))

(defun ada-ts-paren--match-loop_statement-end-p (n)
  "Check if contents after N align with a loop statement."
  (let* ((next-n (ada-ts-mode--next-node n))
         (next-t (treesit-node-type next-n))
         (next-next-n (and next-n (ada-ts-mode--next-node next-n)))
         (next-next-t (treesit-node-type next-next-n)))
    (or (not next-n)
        (and (string-equal next-t "loop")
             (or (not next-next-n)
                 (string-equal next-next-t ";")
                 (and (string-equal next-next-t "identifier")
                      (when-let* ((parent-n (treesit-node-parent n))
                                  (parent-t (treesit-node-type parent-n))
                                  ((string-equal parent-t "loop_statement"))
                                  (first-child-n (treesit-node-child parent-n 0))
                                  (first-child-t (treesit-node-type first-child-n))
                                  ((string-equal first-child-t "loop_label"))
                                  (label-n (treesit-node-child-by-field-name first-child-n "statement_identifier")))
                        (string-equal-ignore-case (treesit-node-text label-n 'no-property)
                                                  (treesit-node-text next-next-n 'no-property))))
                 (save-excursion
                   (goto-char (treesit-node-end next-n))
                   (looking-at-p (rx (* whitespace) eol)))))
        (save-excursion
          (goto-char (treesit-node-end n))
          (looking-at-p (rx (* whitespace) eol))))))

(defun ada-ts-paren--match-subprogram_body-end-p (n)
  "Check if contents after N align with a subprogram body end."
  (let* ((next-n (ada-ts-mode--next-node n))
         (next-t (treesit-node-type next-n)))
    (or (not next-n)
        (string-equal next-t ";")
        (and (member next-t '("identifier" "string_literal" "selected_component"))
             (when-let* ((parent-n (treesit-node-parent n))
                         (parent-t (treesit-node-type parent-n))
                         ((string-equal parent-t "subprogram_body"))
                         (spec-n (car (treesit-filter-child parent-n
                                                            (lambda (n)
                                                              (when-let* ((n-t (treesit-node-type n)))
                                                                (member n-t '("function_specification"
                                                                              "procedure_specification")))))))
                         (name-n (treesit-node-child-by-field-name spec-n "name"))
                         (name-t (treesit-node-type name-n)))
               (and (member name-t '("identifier" "string_literal" "selected_component"))
                    (string-equal-ignore-case
                     (ada-ts-mode--node-to-name next-n)
                     (ada-ts-mode--node-to-name name-n)))))
        (save-excursion
          (goto-char (treesit-node-end n))
          (looking-at-p (rx (* whitespace) eol))))))

(defconst ada-ts-paren--show-paren-info
  `(("accept" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((n-e (treesit-node-end n))
                    (parent-n (treesit-node-parent n)))
          (treesit-filter-child parent-n
                                (lambda (n)
                                  (when-let* ((n-t (treesit-node-type n)))
                                    (and (string-equal n-t "do")
                                         (> (treesit-node-start n) n-e)))))))
     :matching-delimiter-predicate
     ,(lambda (n)
        (let* ((next-n (ada-ts-mode--next-node n))
               (next-t (treesit-node-type next-n)))
          (or (not next-n)
              (string-equal next-t ";")
              (and (string-equal next-t "identifier")
                   (when-let* ((parent-n (treesit-node-parent n))
                               (name-n (ada-ts-mode--next-node (treesit-node-at (treesit-node-start parent-n)))))
                     (string-equal-ignore-case (treesit-node-text next-n 'no-property)
                                               (treesit-node-text name-n 'no-property))))
              (save-excursion
                (goto-char (treesit-node-end n))
                (looking-at (rx (* whitespace) eol)))))))
    ("begin" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((parent-n (treesit-node-parent n))
                    (parent-t (treesit-node-type parent-n))
                    ((string-equal parent-t "block_statement"))
                    (child-n (treesit-node-child parent-n 0))
                    (child-t (treesit-node-type child-n)))
          (when (string-equal child-t "loop_label")
            (setq child-n (ada-ts-mode--next-node child-n)))
          (treesit-node-eq n child-n)))
     :matching-delimiter-predicate ,#'ada-ts-paren--match-block_statement-end-p)
    ("case" :delimiter-type opener :matching-delimiter "end"
     :predicate ,(ada-ts-paren--not-after-type-p '("end" "("))
     :matching-delimiter-predicate ,(ada-ts-paren--match-second-delimiter-p "case"))
    ("declare" :delimiter-type opener :matching-delimiter "end"
     :predicate ,(ada-ts-paren--not-after-type-p "(")
     :matching-delimiter-predicate ,#'ada-ts-paren--match-block_statement-end-p)
    ("entry" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((n-e (treesit-node-end n))
                    (parent-n (treesit-node-parent n)))
          (treesit-filter-child parent-n
                                (lambda (n)
                                  (when-let* ((n-t (treesit-node-type n)))
                                    (and (string-equal n-t "begin")
                                         (> (treesit-node-start n) n-e)))))))
     :matching-delimiter-predicate
     ,(lambda (n)
        (let* ((next-n (ada-ts-mode--next-node n))
               (next-t (treesit-node-type next-n)))
          (or (not next-n)
              (string-equal next-t ";")
              (and (string-equal next-t "identifier")
                   (when-let* ((parent-n (treesit-node-parent n))
                               (name-n (ada-ts-mode--next-node (treesit-node-at (treesit-node-start parent-n)))))
                     (string-equal-ignore-case (treesit-node-text next-n 'no-property)
                                               (treesit-node-text name-n 'no-property))))
              (save-excursion
                (goto-char (treesit-node-end n))
                (looking-at (rx (* whitespace) eol)))))))
    ("for" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((node-s (treesit-node-start n))
                    (parent-n (treesit-node-parent n))
                    (parent-t (treesit-node-type parent-n))
                    (parent-s (treesit-node-start parent-n)))
          (and (string-equal parent-t "iteration_scheme")
               (= node-s parent-s))))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((p-n (treesit-node-parent n)))
          (treesit-node-parent p-n)))
     :matching-delimiter-predicate ,#'ada-ts-paren--match-loop_statement-end-p)
    ("function" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((p-n (treesit-node-parent n))
                    (g-p-n (treesit-node-parent p-n))
                    (g-p-n-t (treesit-node-type g-p-n)))
          (string-equal g-p-n-t "subprogram_body")))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((p-n (treesit-node-parent n))
                    (g-p-n (treesit-node-parent p-n))
                    (g-p-n-t (treesit-node-type g-p-n)))
          (and (string-equal g-p-n-t "subprogram_body")
               g-p-n)))
     :matching-delimiter-predicate ,#'ada-ts-paren--match-subprogram_body-end-p)
    ("if" :delimiter-type opener :matching-delimiter "end"
     :predicate ,(ada-ts-paren--not-after-type-p '("end" "("))
     :matching-delimiter-predicate ,(ada-ts-paren--match-second-delimiter-p "if"))
    ("loop" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (and (funcall (ada-ts-paren--not-after-type-p "end") n)
             (if-let* ((prev-n (ada-ts-mode--prev-node n))
                       (prev-t (treesit-node-type prev-n)))
                 (not (string-equal prev-t "iteration_scheme"))
               t)))
     :matching-delimiter-predicate ,#'ada-ts-paren--match-loop_statement-end-p)
    ("package" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((next-n (ada-ts-mode--next-node n))
                    (next-t (treesit-node-type next-n))
                    (parent-n (treesit-node-parent n))
                    ((member next-t '("identifier" "selected_component" "body")))
                    (is-n
                     (car (treesit-filter-child parent-n
                                                (lambda (n)
                                                  (when-let* ((n-t (treesit-node-type n)))
                                                    (string-equal n-t "is"))))))
                    (next-n (ada-ts-mode--next-node is-n))
                    (next-t (treesit-node-type next-n)))
          (not (member next-t '("separate" "new")))))
     :matching-delimiter-predicate
     ,(lambda (n)
        (let* ((next-n (ada-ts-mode--next-node n))
               (next-t (treesit-node-type next-n)))
          (or (not next-n)
              (string-equal next-t ";")
              (and (member next-t '("identifier" "selected_component"))
                   (when-let* ((parent-n (treesit-node-parent n))
                               (name-n (treesit-node-child-by-field-name parent-n "name"))
                               (endname-n (treesit-node-child-by-field-name parent-n "endname")))
                     (string-equal-ignore-case
                      (ada-ts-mode--node-to-name name-n)
                      (ada-ts-mode--node-to-name endname-n))))
              (save-excursion
                (goto-char (treesit-node-end n))
                (looking-at (rx (* whitespace) eol)))))))
    ("parallel" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (if-let* ((prev-n (ada-ts-mode--prev-node n))
                  (prev-t (treesit-node-type prev-n)))
            (not (string-equal prev-t "["))
          t))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((parent-n (treesit-node-parent n))
                    (parent-t (treesit-node-type parent-n)))
          (if (string-equal parent-t "iteration_scheme")
              (treesit-node-parent parent-n)
            parent-n)))
     :matching-delimiter-predicate
     ,(lambda (n)
        (or (funcall (ada-ts-paren--match-second-delimiter-p "do") n)
            (ada-ts-paren--match-loop_statement-end-p n))))
    ("procedure" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((p-n (treesit-node-parent n))
                    (g-p-n (treesit-node-parent p-n))
                    (g-p-n-t (treesit-node-type g-p-n)))
          (string-equal g-p-n-t "subprogram_body")))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((p-n (treesit-node-parent n))
                    (g-p-n (treesit-node-parent p-n))
                    (g-p-n-t (treesit-node-type g-p-n)))
          (and (string-equal g-p-n-t "subprogram_body")
               g-p-n)))
     :matching-delimiter-predicate ,#'ada-ts-paren--match-subprogram_body-end-p)
    ("protected" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((next-n (ada-ts-mode--next-node n))
                    (next-t (treesit-node-type next-n))
                    (parent-n (treesit-node-parent n)))
          (cond
           ;; single_protected_declaration
           ((string-equal next-t "identifier"))
           ;; protected_body
           ((string-equal next-t "body")
            (treesit-filter-child parent-n
                                  (lambda (n)
                                    (when-let* ((n-t (treesit-node-type n))
                                                (next-n (ada-ts-mode--next-node n))
                                                (next-t (treesit-node-type next-n)))
                                      (and (string-equal n-t "is")
                                           (not (string-equal next-t "separate")))))))
           ;; protected_type_declaration
           ((string-equal next-t "type")))))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((next-n (ada-ts-mode--next-node n))
                    (next-t (treesit-node-type next-n))
                    (parent-n (treesit-node-parent n)))
          (if (member next-t '("identifier" "type"))
              (car (treesit-filter-child parent-n
                                         (lambda (n)
                                           (when-let* ((n-t (treesit-node-type n)))
                                             (string-equal n-t "protected_definition")))))
            parent-n)))
     :matching-delimiter-predicate
     ,(lambda (n)
        (let* ((next-n (ada-ts-mode--next-node n))
               (next-t (treesit-node-type next-n)))
          (or (not next-n)
              (string-equal next-t ";")
              (and (string-equal next-t "identifier")
                   (when-let* ((parent-n (treesit-node-parent n))
                               (parent-t (treesit-node-type parent-n)))
                     (cond ((string-equal parent-t "protected_definition")
                            (when-let* ((g-p-n (treesit-node-parent parent-n))
                                        (name-n (car (treesit-filter-child g-p-n
                                                                           (lambda (n)
                                                                             (when-let* ((n-t (treesit-node-type n)))
                                                                               (string-equal n-t "identifier")))))))
                              (string-equal-ignore-case (treesit-node-text next-n 'no-property)
                                                        (treesit-node-text name-n 'no-property))))
                           ((string-equal parent-t "protected_body")
                            (when-let* ((name-n (car (treesit-filter-child parent-n
                                                                           (lambda (n)
                                                                             (when-let* ((n-t (treesit-node-type n)))
                                                                               (string-equal n-t "identifier")))))))
                              (string-equal-ignore-case (treesit-node-text next-n 'no-property)
                                                        (treesit-node-text name-n 'no-property)))))))
              (save-excursion
                (goto-char (treesit-node-end n))
                (looking-at (rx (* whitespace) eol)))))))
    ("record" :delimiter-type opener :matching-delimiter "end"
     :predicate ,(ada-ts-paren--not-after-type-p '("end" "null"))
     :matching-delimiter-predicate
     ,(lambda (n)
        (let* ((next-n (ada-ts-mode--next-node n))
               (next-t (treesit-node-type next-n))
               (next-next-n (and next-n (ada-ts-mode--next-node next-n)))
               (next-next-t (treesit-node-type next-next-n)))
          (or (not next-n)
              (and (string-equal next-t "record")
                   (or (not next-next-n)
                       (string-equal next-next-t ";")
                       (and (string-equal next-next-t "identifier")
                            (when-let* ((p-n (treesit-node-parent n))
                                        (p-t (treesit-node-type p-n)))
                              (cond ((string-equal p-t "record_representation_clause")
                                     (when-let* ((local-name-n (treesit-node-child-by-field-name p-n "local_name")))
                                       (string-equal-ignore-case (treesit-node-text local-name-n 'no-property)
                                                                 (treesit-node-text next-next-n 'no-property))))
                                    ((string-equal p-t "record_definition")
                                     (when-let* ((g-p-n (treesit-node-parent p-n))
                                                 (g-p-t (treesit-node-type g-p-n)))
                                       (cond ((string-equal g-p-t "record_type_definition")
                                              (when-let* ((g-g-p-n (treesit-node-parent g-p-n))
                                                          (g-g-p-t (treesit-node-type g-g-p-n))
                                                          ((string-equal g-g-p-t "full_type_declaration"))
                                                          (name-n (ada-ts-mode--next-node (treesit-node-at (treesit-node-start g-g-p-n)))))
                                                (string-equal-ignore-case (treesit-node-text name-n 'no-property)
                                                                          (treesit-node-text next-next-n 'no-property))))
                                             ((string-equal g-p-t "record_extension_part")
                                              (when-let* ((g-g-p-n (treesit-node-parent g-p-n))
                                                          (g-g-p-t (treesit-node-type g-g-p-n))
                                                          ((string-equal g-g-p-t "derived_type_definition"))
                                                          (g-g-g-p-n (treesit-node-parent g-g-p-n))
                                                          (g-g-g-p-t (treesit-node-type g-g-g-p-n))
                                                          ((string-equal g-g-g-p-t "full_type_declaration"))
                                                          (name-n (ada-ts-mode--next-node (treesit-node-at (treesit-node-start g-g-g-p-n)))))
                                                (string-equal-ignore-case (treesit-node-text name-n 'no-property)
                                                                          (treesit-node-text next-next-n 'no-property))))))))))
                       (save-excursion
                         (goto-char (treesit-node-end next-n))
                         (looking-at (rx (* whitespace) eol)))))
              (save-excursion
                (goto-char (treesit-node-end n))
                (looking-at (rx (* whitespace) eol)))))))
    ("return" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (and (funcall (ada-ts-paren--not-after-type-p "end") n)
             (when-let* ((node-s (treesit-node-start n))
                         (parent-n (treesit-node-parent n))
                         (parent-t (treesit-node-type parent-n)))
               (treesit-filter-child parent-n
                                     (lambda (n)
                                       (when-let* ((n-t (treesit-node-type n))
                                                   (n-s (treesit-node-start n)))
                                         (and (string-equal n-t "do")
                                              (> n-s node-s))))))))
     :matching-delimiter-predicate ,(ada-ts-paren--match-second-delimiter-p "return"))
    ("select" :delimiter-type opener :matching-delimiter "end"
     :predicate ,(ada-ts-paren--not-after-type-p "end")
     :matching-delimiter-predicate ,(ada-ts-paren--match-second-delimiter-p "select"))
    ("task" :delimiter-type opener :matching-delimiter "end"
     :predicate
     ,(lambda (n)
        (when-let* ((next-n (ada-ts-mode--next-node n))
                    (next-t (treesit-node-type next-n))
                    (parent-n (treesit-node-parent n)))
          (cond
           ;; single_task_declaration
           ((string-equal next-t "identifier"))
           ;; task_body
           ((string-equal next-t "body")
            (treesit-filter-child parent-n
                                  (lambda (n)
                                    (when-let* ((n-t (treesit-node-type n)))
                                      (string-equal n-t "begin")))))
           ;; task_type_declaration
           ((string-equal next-t "type")
            (treesit-filter-child parent-n
                                  (lambda (n)
                                    (when-let* ((n-t (treesit-node-type n)))
                                      (string-equal n-t "is"))))))))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((next-n (ada-ts-mode--next-node n))
                    (next-t (treesit-node-type next-n))
                    (parent-n (treesit-node-parent n)))
          (if (member next-t '("identifier" "type"))
              (car (treesit-filter-child parent-n
                                         (lambda (n)
                                           (when-let* ((n-t (treesit-node-type n)))
                                             (string-equal n-t "task_definition")))))
            parent-n)))
     :matching-delimiter-predicate
     ,(lambda (n)
        (let* ((next-n (ada-ts-mode--next-node n))
               (next-t (treesit-node-type next-n)))
          (or (not next-n)
              (string-equal next-t ";")
              (and (string-equal next-t "identifier")
                   (when-let* ((parent-n (treesit-node-parent n))
                               (parent-t (treesit-node-type parent-n)))
                     (cond ((string-equal parent-t "task_definition")
                            (when-let* ((g-p-n (treesit-node-parent parent-n))
                                        (name-n (car (treesit-filter-child g-p-n
                                                                           (lambda (n)
                                                                             (when-let* ((n-t (treesit-node-type n)))
                                                                               (string-equal n-t "identifier")))))))
                              (string-equal-ignore-case (treesit-node-text next-n 'no-property)
                                                        (treesit-node-text name-n 'no-property))))
                           ((string-equal parent-t "task_body")
                            (when-let* ((name-n (car (treesit-filter-child parent-n
                                                                           (lambda (n)
                                                                             (when-let* ((n-t (treesit-node-type n)))
                                                                               (string-equal n-t "identifier")))))))
                              (string-equal-ignore-case (treesit-node-text next-n 'no-property)
                                                        (treesit-node-text name-n 'no-property)))))))
              (save-excursion
                (goto-char (treesit-node-end n))
                (looking-at (rx (* whitespace) eol)))))))
    ("while" :delimiter-type opener :matching-delimiter "end"
     :prediate
     ,(lambda (n)
        (when-let* ((parent-n (treesit-node-parent n))
                    (parent-t (treesit-node-type parent-n)))
          (string-equal parent-t "iteration_scheme")))
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((p-n (treesit-node-parent n)))
          (treesit-node-parent p-n)))
     :matching-delimiter-predicate ,#'ada-ts-paren--match-loop_statement-end-p)
    ("end" :delimiter-type closer
     :matching-delimiter
     ("accept" "case" "begin" "declare" "entry" "for" "function" "if" "loop" "package"
      "parallel" "procedure" "protected" "record" "return" "select" "task" "while")
     :matching-delimiter-parent-function
     ,(lambda (n)
        (when-let* ((parent-n (treesit-node-parent n))
                    (parent-t (treesit-node-type parent-n)))
          (cond ((string-equal parent-t "subprogram_body")
                 (car (treesit-filter-child parent-n
                                            (lambda (n)
                                              (when-let* ((n-t (treesit-node-type n)))
                                                (member n-t '("function_specification"
                                                              "procedure_specification")))))))
                ((member parent-t '("task_definition"
                                    "protected_definition"))
                 (treesit-node-parent parent-n))
                ((car (treesit-filter-child parent-n
                                            (lambda (n)
                                              (when-let* ((n-t (treesit-node-type n)))
                                                (string-equal n-t "iteration_scheme"))))))
                (t parent-n)))))))

(defun ada-ts-paren--show-paren-data-categorize (pos)
  "Return a list suitable for `show-paren-data-function'.

The delimiter must start at, end at, or contain position POS."
  (when-let* ((here-n (ada-ts-mode--node-at pos 'or-ends-at-pos ";"))
              (here-t (treesit-node-type here-n))
              (here-s (treesit-node-start here-n))
              (here-e (treesit-node-end here-n))
              (info (cdr (assoc-string here-t ada-ts-paren--show-paren-info)))
              ((or (not (plist-get info :predicate))
                   (funcall (plist-get info :predicate) here-n)))
              (delimiter-type (plist-get info :delimiter-type))
              (matching-delimiters (ensure-list (plist-get info :matching-delimiter)))
              (parent-node
               (if-let* ((matching-delimiter-parent-function
                          (plist-get info :matching-delimiter-parent-function)))
                   (funcall matching-delimiter-parent-function here-n)
                 (treesit-node-parent here-n))))
    (let* ((matched-delimiters
            (treesit-filter-child
             parent-node
             (lambda (there-n)
               (let* ((there-t (treesit-node-type there-n))
                      (there-s (treesit-node-start there-n)))
                 (and (member there-t matching-delimiters)
                      (cond  ((eq delimiter-type 'opener) (< here-s there-s))
                             ((eq delimiter-type 'closer) (< there-s here-s))
                             (t (error "Unknown delimiter type")))
                      (let* ((there-info (cdr (assoc-string there-t ada-ts-paren--show-paren-info)))
                             (there-predicate (plist-get there-info :predicate))
                             (here-predicate (plist-get there-info :matching-delimiter-predicate)))
                        (and (or (not there-predicate)
                                 (funcall there-predicate there-n))
                             (or (not here-predicate)
                                 (funcall here-predicate here-n)))))))))
           (there-n (cond ((eq delimiter-type 'opener) (car matched-delimiters))
                          ((eq delimiter-type 'closer) (car (reverse matched-delimiters)))
                          (t (error "Unknown delimiter type"))))
           (there-s (treesit-node-start there-n))
           (there-e (treesit-node-end there-n))
           (error-n
            (car (treesit-filter-child
                  parent-node
                  (lambda (n)
                    (let ((n-t (treesit-node-type n)))
                      (or (string-equal n-t "ERROR")
                          (treesit-node-check n 'missing)
                          (treesit-node-check n 'has-error)))))))
           (error-s (treesit-node-start error-n)))
      (when (cond ((not (null show-paren-when-point-in-periphery)))
                  ((not (null show-paren-when-point-inside-paren))
                   (cond ((eq delimiter-type 'opener) (> pos here-s))
                         ((eq delimiter-type 'closer) (< pos here-e))))
                  (t
                   (cond ((eq delimiter-type 'opener) (< pos here-e))
                         ((eq delimiter-type 'closer) (> pos here-s)))))
        (if (and there-n
                 (let* ((there-predicate (plist-get info :matching-delimiter-predicate)))
                   (or (not there-predicate)
                       (funcall there-predicate there-n)))
                 (or (not error-n)
                     ;; As long as the error occurs after the closing
                     ;; delimiter, still try to highlight both
                     ;; delimiters.
                     (and (> error-s there-s)
                          (> error-s here-s))))
            (list here-s here-e there-s there-e)
          (list here-s here-e nil nil 'mismatch))))))

(defun ada-ts-paren--show-paren-data ()
  "A function suitable for `show-paren-data-function'."
  (or (ada-ts-paren--show-paren-data-categorize (point))
      (when show-paren-when-point-in-periphery
        (let* ((current-pos (point))
               (indent-pos (save-excursion
                             (back-to-indentation)
                             (point)))
               (eol-pos (save-excursion
                          (end-of-line)
                          (skip-chars-backward " \t" indent-pos)
                          (point))))
          (let ((show-paren-when-point-inside-paren nil))
            (cond ((<= current-pos indent-pos)
                   (ada-ts-paren--show-paren-data-categorize indent-pos))
                  ((>= current-pos eol-pos)
                   (ada-ts-paren--show-paren-data-categorize eol-pos))))))
      ;; Fall back for parenthesis matching.
      (show-paren--default)))

(defun ada-ts-paren--show-paren-post-setup ()
  "Parenthesis matching setup performed after `treesit-major-mode-setup'."
  (setq-local show-paren-data-function #'ada-ts-paren--show-paren-data))

(add-hook 'ada-ts-mode--after-setup-hook #'ada-ts-paren--show-paren-post-setup)

;;;; `blink-matching-paren' support

(defconst ada-ts-paren--blink-matching-paren-closers '("end" ")"))

(defun ada-ts-paren--blink-matching-paren-data ()
  "Determine matching opener, if one exists.

Return nil if point is not on or immediately after a closer.  Otherwise,
returns value compatible with `show-paren-data-function'."
  (when-let* ((data
               (let ((show-paren-when-point-in-periphery nil)
                     (show-paren-when-point-inside-paren nil))
                 (ada-ts-paren--show-paren-data))))
    (let* ((here-s (nth 0 data))
           (here-e (nth 1 data))
           (there-s (nth 2 data))
           (mismatch (nth 4 data)))
      (and (or (and (not mismatch)
                    (< there-s here-s))
               (and mismatch
                    (let ((closer (buffer-substring-no-properties here-s here-e)))
                      (member-ignore-case
                       closer
                       ada-ts-paren--blink-matching-paren-closers))))
           data))))

(defun ada-ts-paren--maybe-blink-matching-paren ()
  "Blink the matching opener when applicable.

This is expected to be placed on `post-command-hook'."
  (when-let* (((not (null blink-matching-paren)))
              ((not show-paren-mode))
              ((eq this-command 'self-insert-command))
              ((not executing-kbd-macro))
              (data (ada-ts-paren--blink-matching-paren-data)))
    (let* ((opener-s (nth 2 data))
           (opener-e (nth 3 data)))
      (if opener-s
          (cond ((or (eq blink-matching-paren 'jump-offscreen)
                     (pos-visible-in-window-p opener-s))
                 (and blink-matching-paren-on-screen
                      (if (memq blink-matching-paren '(jump jump-offscreen))
                          (save-excursion
                            (goto-char opener-s)
                            (sit-for blink-matching-delay))
                        (unwind-protect
                            (progn
                              (move-overlay blink-matching--overlay
                                            opener-s
                                            opener-e
                                            (current-buffer))
                              (sit-for blink-matching-delay))
                          (delete-overlay blink-matching--overlay)))))
                (t
                 (let* ((line (blink-paren-open-paren-line-string opener-s)))
                   (minibuffer-message "%s%s"
                                       (propertize "Matches " 'face 'shadow)
                                       line))))
        (message "No matching delimiter found")))))

;;;; Parenthesis Setup

(defun ada-ts-paren--setup ()
  "Setup parenthesis support for buffer."
  ;; For `blink-matching-paren', don't trigger off of
  ;; `blink-paren-post-self-insert-function', which by default is in
  ;; the global `post-self-insert-hook', since that only matches
  ;; close-parenthesis or paired delimiter characters based on the
  ;; mode's syntax table, but doesn't handle block (i.e., keyword)
  ;; delimiters.  It's disabled here by setting `blink-paren-function'
  ;; to nil.
  ;;
  ;; Instead, the mode's own hook is added to `post-command-hook' with
  ;; a high depth to ensure it is executed after other functionality
  ;; that should occur first (e.g., electric indentation).  Because
  ;; the blinking delay can be substantial, it's better to perform
  ;; tasks such as indentation first, rather than delay them, as it
  ;; can be jarring to the user to see the buffer change after a
  ;; "long" delay.
  (setq-local blink-paren-function nil)
  (add-hook 'post-command-hook #'ada-ts-paren--maybe-blink-matching-paren 100 'local))

(provide 'ada-ts-paren)

;;; ada-ts-paren.el ends here
