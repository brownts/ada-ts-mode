;;; ada-ts-align.el -- Alignment support in Ada files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Troy Brown

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

(require 'align)
(eval-when-compile (require 'rx))
(require 'treesit)

(declare-function ada-ts-mode--next-leaf-node "ada-ts-indentation" (start))

(defvar ada-ts-align--keyword-separators
  '("begin"
    "declare"
    "else"
    "end"
    "exception"
    "for"
    "function"
    "generic"
    "if"
    "is"
    "procedure"
    "private"
    "record"
    "return"
    "type"
    "when")
  "Keywords that are also alignment separators.")

(defun ada-ts-align ()
  "Align region or section."
  (interactive nil ada-ts-mode)
  (if (region-active-p)
      (progn
        (align (region-beginning) (region-end))
        (deactivate-mark 'force))
    (if-let* ((formal-part-node (ada-ts-align--formal-part-p)))
        (ada-ts-align--formal-part formal-part-node)
      (align-current))))

(defun ada-ts-align--formal-part-p ()
  "Check that point is within a formattable parameter list.

If point is within a formattable parameter list, return formal_part
node, else return nil."
  (when-let*
      ;; Check that point is within a formal_part.
      ((formal-part-node
        (treesit-parent-until
         (treesit-node-on (point) (point))
         (lambda (n)
           (string-equal (treesit-node-type n) "formal_part"))))
       ;; Check that tree doesn't contain syntax errors.
       ((not (treesit-search-subtree formal-part-node "ERROR")))
       ;; Check for at least 2 parameter specifications, otherwise
       ;; alignment doesn't make sense.
       (specifications
        (treesit-node-children formal-part-node "parameter_specification"))
       ((>= (length specifications) 2)))
    formal-part-node))

(defun ada-ts-align--formal-part (node)
  "Format parameter list corresponding to NODE."
  (let ((identifier-field-length 0)
        in-p out-p null-exclusion-p access-p aliased-p constant-p protected-p
        params)
    (message "NODE: %s" node)
    (dolist (param (treesit-node-children node "parameter_specification"))
      (message "PARAM: %s" param)
      (let* ((child-exists-p (lambda (parent node-t)
                               (seq-find
                                (lambda (n)
                                  (string-equal (treesit-node-type n) node-t))
                                (treesit-node-children parent))))
             (identifiers
              (seq-map
               (lambda (n)
                 (treesit-node-text n 'no-property))
               (seq-filter
                (lambda (n)
                  (string-equal (treesit-node-type n) "identifier"))
                (seq-take-while
                 (lambda (n)
                   (not (string-equal (treesit-node-type n) ":")))
                 (treesit-node-children param)))))
             (aliased (funcall child-exists-p param "aliased"))
             (null-exclusion (funcall child-exists-p param "null_exclusion"))
             (non-empty-mode (funcall child-exists-p param "non_empty_mode"))
             (in (and non-empty-mode (funcall child-exists-p non-empty-mode "in")))
             (out (and non-empty-mode (funcall child-exists-p non-empty-mode "out")))
             (subtype-mark (treesit-node-child-by-field-name param "subtype_mark"))
             (access-definition (funcall child-exists-p param "access_definition"))
             (null-exclusion (or null-exclusion
                                 (and access-definition
                                      (funcall child-exists-p access-definition "null_exclusion"))))
             (constant (and access-definition (funcall child-exists-p access-definition "constant")))
             (subtype-mark (or subtype-mark
                               (and access-definition
                                    (treesit-node-child-by-field-name access-definition "subtype_mark"))))
             (protected (and access-definition (funcall child-exists-p access-definition "protected"))))
        ;; TODO: Still need to handle function/procedure formal_part/_parameter_and_result_profile
        (setq aliased-p        (or aliased-p        aliased)
              in-p             (or in-p             in)
              out-p            (or out-p            out)
              null-exclusion-p (or null-exclusion-p null-exclusion)
              access-p         (or access-p         access-definition)
              constant-p       (or constant-p       constant)
              protected-p      (or protected-p      protected))
        (setq identifier-field-length
              (max identifier-field-length
                   (+ (seq-reduce (lambda (s n) (+ s (length n))) identifiers 0) ; "identifier" lengths
                      (* 2 (1- (length identifiers))))))                         ; ", " separators
        (push `( :identifiers ,identifiers
                 :in          ,in
                 :out         ,out
                 :access      ,access-definition
                 :aliased     ,aliased
                 :constant    ,constant
                 :protected   ,protected)
              params)))
    ;; [aliased] [in] [out] | [not null] subtype_mark
    ;; [not null] access    | [constant] subtype_mark
    ;; [not null] access    | [protected] procedure formal_part
    ;; [not null] access    | [protected] function _parameter_and_result_profile

    ;; TODO: use `indent-to' to align whole line to desired column?  or just use tree-sitter indentation to align whole region?
    ;; May want to use the tree-sitter back-end since LSP provided back-end may reformat everything.
    (message "PARAMS: %s" params)
    ))

;; (message "TODO: Handle parameter list: %s" node))

(defun ada-ts-align--contains-separator-p (beg end)
  "Check if section separator is found in range BEG to END."
  (when (markerp beg)
    (setq beg (marker-position beg)))
  (if (and (null beg) (null end))
      t ; use common section
    (or
     ;; keyword separator
     (save-excursion
       (goto-char beg)
       (skip-chars-forward " \t\n" end)
       (when-let* (((< (point) end))
                   (node (treesit-node-at (point)))
                   (node-s (treesit-node-start node))
                   (node-t (treesit-node-type node)))
         (while (and node
                     (< node-s end)
                     (cond ((not (member node-t ada-ts-align--keyword-separators)))
                           ((string-equal node-t "when")
                            (when-let* ((parent (treesit-node-parent node))
                                        (parent-t (treesit-node-type parent)))
                              ;; case expression "when" isn't a separator.
                              (string-equal parent-t "case_expression_alternative")))))
           (setq node (ada-ts-mode--next-leaf-node node)
                 node-s (treesit-node-start node)
                 node-t (treesit-node-type node)))
         (and node
              (< node-s end)
              (member node-t ada-ts-align--keyword-separators))))
     ;; empty line separator
     (save-excursion
       (goto-char beg)
       (search-forward-regexp (rx bol (* whitespace) eol) end 'noerror)))))

(defun ada-ts-align--valid-p ()
  "Determine if alignment location is valid."
  (save-excursion
    (goto-char (match-beginning 1))
    (let ((status (parse-partial-sexp (pos-bol) (point))))
      (and (not (nth 4 status))     ;; Not in comment
           (not (nth 3 status)))))) ;; Not in string

(defun ada-ts-align--setup ()
  "Setup align command for mode."
  (setq-local align-region-separate #'ada-ts-align--contains-separator-p)
  (setq-local align-mode-exclude-rules-list
              `((ada-standalone-comment
                 (regexp . ,(rx bol (group (* whitespace) "--")))
                 (modes  . '(ada-ts-mode)))))
  (setq-local align-mode-rules-list
              `((ada-declaration-assign
                 (regexp . ,(rx (not ":") (group (* whitespace)) ":" (not ":")))
                 (valid  . ada-ts-align--valid-p)
                 (repeat . t)
                 (modes  . '(ada-ts-mode)))
                (ada-associate
                 (regexp . ,(rx (not "=") (group (* whitespace)) "=>"))
                 (valid  . ada-ts-align--valid-p)
                 (modes  . '(ada-ts-mode)))
                (ada-at ; component_clause
                 (regexp . ,(rx (group (+ whitespace)) "at" eow))
                 (valid  . ada-ts-align--valid-p)
                 (modes  . '(ada-ts-mode)))
                (ada-trailing-use
                 (regexp . ,(rx ";" (group (* whitespace)) "use" whitespace))
                 (valid  . ada-ts-align--valid-p)
                 (modes  . '(ada-ts-mode)))
                (ada-trailing-comment
                 (regexp . ,(rx (+? nonl) (group (* whitespace)) "--"))
                 (valid  . ada-ts-align--valid-p)
                 (modes  . '(ada-ts-mode))))))

(provide 'ada-ts-align)

;;; ada-ts-align.el ends here
