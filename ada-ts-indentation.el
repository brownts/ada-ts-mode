;;; ada-ts-indentation.el -- Indentation support in Ada files -*- lexical-binding: t; -*-

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

(require 'ada-ts-mode-lspclient)
(require 'cl-generic)
(require 'treesit)
(eval-when-compile (require 'rx))

(defvar ada-ts-mode--keywords nil) ; definition in ada-ts-mode.el
(declare-function ada-ts-mode--defun-name   "ada-ts-mode" (node))
(declare-function ada-ts-mode--node-to-name "ada-ts-mode" (node))

(defcustom ada-ts-mode-indent-backend 'tree-sitter
  "Backend used for indentation."
  :type '(choice (const :tag "Tree-sitter" tree-sitter)
                 (const :tag "Language Server" lsp))
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))
;;;###autoload(put 'ada-ts-mode-indent-backend 'safe-local-variable #'symbolp)

(defcustom ada-ts-mode-indent-strategy 'aggressive
  "Indentation strategy utilized with tree-sitter backend."
  :type '(choice :tag "Indentation Strategy"
                 (const :tag "Aggressive" aggressive)
                 (const :tag "Line" line))
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-offset 3
  "Indentation of statements."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.7.0"))
;;;###autoload(put 'ada-ts-mode-indent-offset 'safe-local-variable #'integerp)

(defcustom ada-ts-mode-indent-when-offset ada-ts-mode-indent-offset
  "Indentation of `when' relative to `case'."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-broken-offset (- ada-ts-mode-indent-offset 1)
  "Indentation for the continuation of a broken line."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-exp-item-offset 0
  "Indentation for the continuation of an expression."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-subprogram-is-offset (- ada-ts-mode-indent-offset 1)
  "Indentation of \"is\" in a null procedure or expression function."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defcustom ada-ts-mode-indent-record-offset ada-ts-mode-indent-offset
  "Indentation of \"record\" in a type or representation clause."
  :type 'integer
  :group 'ada-ts
  :link '(custom-manual :tag "Indentation" "(ada-ts-mode)Indentation")
  :package-version '(ada-ts-mode . "0.8.0"))

(defun ada-ts-mode--indent-recompute (symbol newval operation where)
  "Recompute indentation variables when SYMBOL is changed.

SYMBOL is expected to be `ada-ts-mode-indent-offset', and
OPERATION is queried to check that it is a `set' operation (as
defined by `add-variable-watcher'), otherwise nothing is updated.
Assuming the global value has not been updated by the user, the
indentation variables are updated using the NEWVAL of SYMBOL and
made buffer-local WHERE indicates a buffer-local modification of
SYMBOL, else the default value is updated instead."
  (when (and (eq symbol 'ada-ts-mode-indent-offset)
             (eq operation 'set))
    (dolist (indent-symbol '(ada-ts-mode-indent-when-offset
                             ada-ts-mode-indent-broken-offset
                             ada-ts-mode-indent-subprogram-is-offset
                             ada-ts-mode-indent-exp-item-offset))
      (let* ((valspec (or (custom-variable-theme-value indent-symbol)
                          (get indent-symbol 'standard-value)))
             (cur-custom-value (eval (car valspec)))
             ;; This routine is invoked before SYMBOL is updated to
             ;; NEWVAL so we need to bind it to the new value so the
             ;; other indentation variables are evaluated using the
             ;; updated value.
             (ada-ts-mode-indent-offset newval)
             (new-custom-value (eval (car valspec))))
        ;; Only update if not globally modified by the user outside of
        ;; the customization system (e.g., via `set-default'), or the
        ;; symbol is already buffer local.
        (when (or (eql cur-custom-value (default-value indent-symbol))
                  (and where (buffer-local-boundp indent-symbol where)))
          (if where
              (with-current-buffer where
                (set (make-local-variable indent-symbol) new-custom-value))
            (set-default indent-symbol new-custom-value)))))))

(add-variable-watcher 'ada-ts-mode-indent-offset #'ada-ts-mode--indent-recompute)

;;; Indentation Error Recovery

;; (defun ada-ts-mode--matching-prev-node (start matches)
;;   "Find a node before START where node type is contained in MATCHES."
;;   (save-excursion
;;     (let ((prev-node start)
;;           prev-node-e prev-node-t)
;;       (while (or (treesit-node-eq prev-node start)
;;                  (and prev-node-t
;;                       (not (member prev-node-t matches))))
;;         (goto-char (treesit-node-start prev-node))
;;         (skip-chars-backward " \t\n" (point-min))
;;         (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
;;               prev-node-e (treesit-node-end prev-node))
;;         (setq prev-node
;;               (treesit-parent-while
;;                prev-node
;;                (lambda (node)
;;                  (and
;;                   (not (string-equal (treesit-node-type node) "ERROR"))
;;                   (= (treesit-node-end node) prev-node-e))))
;;               prev-node-t (treesit-node-type prev-node)))
;;       prev-node)))

(defun ada-ts-mode--prev-node (start &optional include-comments)
  "Find node before START, and possibly INCLUDE-COMMENTS.

START is either a node or a position."
  (let* ((prev-node-s
          (if (treesit-node-p start)
              (treesit-node-start start)
            start))
         (first-pass t)
         prev-node prev-node-e prev-node-t)
    (save-excursion
      (while (or first-pass
                 (and prev-node-t
                      (not include-comments)
                      (string-equal prev-node-t "comment")))
        (setq first-pass nil)
        (goto-char prev-node-s)
        (skip-chars-backward " \t\n" (point-min))
        (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
              prev-node-e (treesit-node-end prev-node))
        (setq prev-node
              (treesit-parent-while
               prev-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-end node) prev-node-e))))
              prev-node-t (treesit-node-type prev-node)
              prev-node-s (treesit-node-start prev-node))))
    prev-node))

(defun ada-ts-mode--next-node (start)
  "Find node after START.

START is either a node or a position."
  (let* ((next-node-e
          (if (treesit-node-p start)
              (treesit-node-end start)
            (1+ start)))
         (first-pass t)
         next-node next-node-s next-node-t)
    (save-excursion
      (while (or first-pass
                 (and next-node-t
                      (string-equal next-node-t "comment")))
        (setq first-pass nil)
        (goto-char next-node-e)
        (skip-chars-forward " \t\n" (point-max))
        (setq next-node (if (eobp) nil (treesit-node-at (point)))
              next-node-s (treesit-node-start next-node))
        (setq next-node
              (treesit-parent-while
               next-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-start node) next-node-s))))
              next-node-t (treesit-node-type next-node)
              next-node-e (treesit-node-end next-node))))
    next-node))

(defun ada-ts-mode--prev-leaf-node (start)
  "Find leaf node before START.

START is either a node or a position."
  (when-let* ((prev-node (ada-ts-mode--prev-node start)))
    (treesit-node-at
     (1- (treesit-node-end prev-node)))))

(defun ada-ts-mode--next-leaf-node (start)
  "Find leaf node after START.

START is either a node or a position."
  (when-let* ((next-node (ada-ts-mode--next-node start)))
    (treesit-node-at (treesit-node-start next-node))))

(defun ada-ts-mode--prev-token (start)
  "Find token before START.

  START is either a node or a position."
  (when-let* ((prev-leaf-node (ada-ts-mode--prev-leaf-node start)))
    (treesit-node-type prev-leaf-node)))

(defun ada-ts-mode--matching-prev-node (start matches)
  "Find a node before START where node type is contained in MATCHES.

  MATCHES is either a list of strings consisting of node types or a
  predicate function which takes a node as its sole parameter and returns
  non nil for a match."
  (let ((prev-node start)
        (predicate (if (functionp matches)
                       matches
                     (lambda (node)
                       (member (treesit-node-type node) matches)))))
    (while (or (treesit-node-eq prev-node start)
               (and prev-node
                    (not (funcall predicate prev-node))))
      (setq prev-node (ada-ts-mode--prev-node prev-node)))
    prev-node))

(defun ada-ts-mode--is-keyword-anchor (node)
  "Find anchor node for \\='is\\=' keyword NODE."
  (when-let* ((anchor-node
               (ada-ts-mode--matching-prev-node
                node
                '("case" "entry"
                  "function" "function_specification"
                  "package"
                  "procedure" "procedure_specification"
                  "protected"
                  "subtype"
                  "task" "type")))
              (anchor-node-t (treesit-node-type anchor-node)))
    (when-let* (((string-equal anchor-node-t "type"))
                (prev-anchor-node (ada-ts-mode--prev-node anchor-node))
                (prev-anchor-node-t (treesit-node-type prev-anchor-node)))
      (when (member prev-anchor-node-t '("task" "protected"))
        (setq anchor-node prev-anchor-node
              anchor-node-t prev-anchor-node-t)))
    ;; Adjust to beginning of line to account for leading keywords
    ;; (such as "overriding" for functions and procedures) except for
    ;; "case", since if it's part of a case expression, it's part of a
    ;; larger expression or statement that we don't want to align
    ;; with.
    (if (string-equal anchor-node-t "case")
        anchor-node
      (save-excursion
        (goto-char (treesit-node-start anchor-node))
        (back-to-indentation)
        (treesit-node-at (point))))))

(defun ada-ts-mode--then-keyword-anchor (node)
  "Find anchor node for \\='then\\=' keyword NODE."
  (when-let* ((prev-node (ada-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (if (string-equal prev-node-t "and")
        (ada-ts-mode--prev-node prev-node)
      (ada-ts-mode--matching-prev-node node '("elsif" "#elsif" "if" "#if" "select")))))

(defun ada-ts-mode--do-keyword-anchor (node)
  "Find anchor node for \\='do\\=' keyword NODE."
  (ada-ts-mode--matching-prev-node node '("accept" "return")))


(defun ada-ts-mode--indent-error-recovery (op)
  "Find indentation in the presence of syntax errors.

  If OP is \\='anchor\\=', determine anchor.  If OP is \\='offset\\=',
  determine offset."
  (lambda (node _parent bol &rest _)
    (when treesit--indent-verbose
      (message "*** ORIG NODE: %s" node))

    (setq node (treesit-node-at bol))
    (when (or (> (treesit-node-start node) bol)
              (<= (treesit-node-end node) bol))
      (setq node nil))

    (let* ((node-t (treesit-node-type node))
           (prev-node (ada-ts-mode--prev-node bol))
           (prev-node-t (treesit-node-type prev-node))
           anchor offset scenario)
      (when treesit--indent-verbose
        (message "*** NODE: %s" node)
        (message "*** BOL: %s" bol)
        (message "*** PREV-NODE: %s" prev-node))
      (unless prev-node
        (setq anchor (point-min)
              offset 0
              scenario "Scenario [No previous node]"))
      ;; Keyword: "is"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "is")))
                  (anchor-node (ada-ts-mode--is-keyword-anchor node)))
        (setq anchor (treesit-node-start anchor-node)
              offset 0
              scenario "Scenario [Keyword: 'is']"))
      ;; Keyword: "begin"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "begin"))))
        (setq scenario "Scenario [Keyword: 'begin']")
        (cond ((string-equal prev-node-t "is")
               (when-let* ((anchor-node (ada-ts-mode--is-keyword-anchor prev-node)))
                 (setq anchor (treesit-node-start anchor-node)
                       offset 0)))
              ((string-equal prev-node-t "begin")
               (setq anchor (treesit-node-start prev-node)
                     offset ada-ts-mode-indent-offset))
              (t
               (when-let* ((prev-node (ada-ts-mode--prev-node node))
                           (prev-node-t (treesit-node-type prev-node)))
                 (if (string-equal prev-node-t "non_empty_declarative_part")
                     (setq anchor (treesit-node-start prev-node)
                           offset (- ada-ts-mode-indent-offset))
                   (setq anchor (treesit-node-start prev-node)
                         offset 0))))))
      ;; Keyword: "limited"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "limited")))
                  (next-node (ada-ts-mode--next-leaf-node node))
                  (next-node-t (treesit-node-type next-node))
                  ((string-equal next-node-t "record"))
                  (anchor-node (ada-ts-mode--matching-prev-node node '("type"))))
        (setq anchor (treesit-node-start anchor-node)
              offset ada-ts-mode-indent-record-offset
              scenario "Scenario [Keyword: 'limited']"))
      ;; Keyword: "record"
      (when-let* (((not anchor))
                  ((and node-t
                        (member node-t '("record" "record_definition"))))
                  ((member prev-node-t '("is" "tagged" "limited" "use" "with")))
                  (anchor-node (ada-ts-mode--matching-prev-node node '("type" "for"))))
        (setq anchor (treesit-node-start anchor-node)
              offset ada-ts-mode-indent-record-offset
              scenario "Scenario [Keyword: 'record']"))
      ;; After Keyword: "record"
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "record"))
                  (prev-prev-node (ada-ts-mode--prev-node prev-node))
                  (prev-prev-node-t (treesit-node-type prev-prev-node))
                  ((member prev-prev-node-t '("is" "tagged" "limited" "use" "with")))
                  (prev-node-s (treesit-node-start prev-node)))
        ;; Anchor to "limited" or "record" keyword if it's at the
        ;; beginning of the line, otherwise anchor to the "type"
        ;; (in type definition) or "for" (in representation
        ;; clause) keyword, which might be on a different line,
        ;; especially if discriminant arguments are present.
        (let* ((bol-node
                (save-excursion
                  (goto-char prev-node-s)
                  (back-to-indentation)
                  (treesit-node-at (point))))
               (bol-node-t (treesit-node-type bol-node)))
          (if (member bol-node-t '("limited" "record"))
              (setq anchor (treesit-node-start bol-node)
                    offset ada-ts-mode-indent-offset
                    scenario "Scenario [After Keyword: 'record']")
            (when-let* ((anchor-node (ada-ts-mode--matching-prev-node prev-node '("for" "type"))))
              (setq anchor (treesit-node-start anchor-node)
                    offset ada-ts-mode-indent-offset
                    scenario "Scenario [After Keyword: 'record']")))))
      ;; After Keyword: "is"
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "is"))
                  (anchor-node (ada-ts-mode--is-keyword-anchor prev-node))
                  (anchor-node-s (treesit-node-start anchor-node))
                  (anchor-node-t (treesit-node-type anchor-node)))
        (setq anchor anchor-node-s
              offset (if (string-equal anchor-node-t "case")
                         ada-ts-mode-indent-when-offset
                       ada-ts-mode-indent-offset)
              scenario "Scenario [After Keyword: 'is']"))
      ;; After "elsif_statement_item"
      (when-let* (((not anchor))
                  ((and node-t
                        (member node-t '("elsif" "elsif_statement_item" "else" "end"))))
                  ((string-equal prev-node-t "elsif_statement_item")))
        (setq anchor (treesit-node-start prev-node)
              offset 0
              scenario "Scenario [After 'elsif_statement_item']"))
      ;; Keywords after "statement"
      (when-let* (((not anchor))
                  ((and node-t
                        (member node-t '("elsif" "elsif_statement_item" "else" "end" "exception"))))
                  ((or (string-equal prev-node-t "handled_sequence_of_statements")
                       (ada-ts-mode--statement-p prev-node))))
        (setq anchor (treesit-node-start prev-node)
              offset (- ada-ts-mode-indent-offset)
              scenario "Scenario [Keywords after 'statement']"))
      ;; Keyword: "then"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "then")))
                  (anchor-node (ada-ts-mode--then-keyword-anchor node)))
        (setq anchor (treesit-node-start anchor-node)
              offset 0
              scenario "Scenario [Keyword: 'then']"))
      ;; Keywords after "then"
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "then"))
                  (anchor-node (ada-ts-mode--then-keyword-anchor prev-node))
                  (anchor-node-t (treesit-node-type anchor-node))
                  ((member anchor-node-t '("elsif" "#elsif" "if" "#if"))))
        (setq anchor (treesit-node-start anchor-node)
              offset ada-ts-mode-indent-offset
              scenario "Scenario [Keywords after 'then']"))
      ;; Keyword: "do"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "do")))
                  (anchor-node (ada-ts-mode--do-keyword-anchor node)))
        (setq anchor (treesit-node-start anchor-node)
              offset 0
              scenario "Scenario [Keyword: 'do']"))
      ;; After Keyword: "do"
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "do"))
                  (anchor-node (ada-ts-mode--do-keyword-anchor prev-node))
                  (anchor-node-s (treesit-node-start anchor-node)))
        (setq anchor anchor-node-s
              offset ada-ts-mode-indent-offset
              scenario "Scenario [After Keyword: 'do']"))
      ;; Keyword: "loop"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "loop")))
                  ((not (string-equal prev-node-t "end"))))
        (setq anchor
              ;; Account for possible loop label
              (save-excursion
                (goto-char (treesit-node-start prev-node))
                (back-to-indentation)
                (point))
              offset (if (string-equal prev-node-t "begin")
                         ada-ts-mode-indent-offset
                       0)
              scenario "Scenario [Keyword: 'loop']"))
      ;; After Keyword: "loop"
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "loop"))
                  (prev-prev-node (ada-ts-mode--prev-node prev-node))
                  (prev-prev-node-t (treesit-node-type prev-prev-node)))
        (setq anchor
              ;; Account for possible loop label
              (save-excursion
                (goto-char
                 (treesit-node-start
                  (if (string-equal prev-prev-node-t "iteration_scheme")
                      prev-prev-node
                    prev-node)))
                (back-to-indentation)
                (point))
              offset ada-ts-mode-indent-offset
              scenario "Scenario [After Keyword: 'loop'"))
      ;; After "loop_label"
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "loop_label")))
        (setq anchor (treesit-node-start prev-node)
              offset 0
              scenario "Scenario [After 'loop_label']"))
      ;; Generic subprogram/package declaration
      (when-let* (((not anchor))
                  ((and node-t
                        (member node-t '("function"  "function_specification"
                                         "package"   "package_declaration"
                                         "procedure" "procedure_specification"))))
                  ((member prev-node-t '("generic" "generic_formal_part"))))
        (setq anchor (treesit-node-start prev-node)
              offset 0
              scenario "Scenario [Generic subprogram/package declaration]"))
      ;; Keyword: "end"
      (when-let* (((not anchor))
                  ((and node-t (string-equal node-t "end"))))
        (setq anchor (treesit-node-start prev-node)
              offset (- ada-ts-mode-indent-offset)
              scenario "Scenario [Keyword: 'end']"))
      ;; After ";", "," and "|"
      (when-let* (((not anchor))
                  ((member prev-node-t '(";" "," "|")))
                  (prev-prev-node (ada-ts-mode--prev-node prev-node)))
        (setq anchor (treesit-node-start prev-prev-node)
              offset 0
              scenario "Scenario [After ';', ',', and '|']"))
      ;; Keywords: "or", "else" (select statement)
      (when-let* (((not anchor))
                  ((and node-t (member node-t '("or" "else"))))
                  (prev-token (ada-ts-mode--prev-token node))
                  ((string-equal prev-token ";"))
                  (anchor-node (ada-ts-mode--matching-prev-node node '("select"))))
        (setq anchor (treesit-node-start anchor-node)
              offset 0
              scenario "Scenario [Keywords: 'or', 'else']"))
      ;; After Keyword: "or" (select statement)
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "or"))
                  (prev-token (ada-ts-mode--prev-token prev-node))
                  ((string-equal prev-token ";")))
        (setq anchor (treesit-node-start prev-node)
              offset ada-ts-mode-indent-offset
              scenario "Scenario [After Keyword: 'or']"))
      ;; After Punctuation: "=>"
      (when-let* (((not anchor))
                  (prev-leaf-node (ada-ts-mode--prev-leaf-node bol))
                  (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                  ((string-equal prev-leaf-node-t "=>"))
                  (anchor-node (ada-ts-mode--matching-prev-node prev-leaf-node '("when" "," "(")))
                  (anchor-node-t (treesit-node-type anchor-node)))
        (if (string-equal anchor-node-t "when")
            (setq anchor (treesit-node-start anchor-node)
                  offset ada-ts-mode-indent-offset
                  scenario "Scenario [After Punctuation: '=>']")
          (when (setq anchor-node (ada-ts-mode--next-node anchor-node))
            (setq anchor (treesit-node-start anchor-node)
                  offset ada-ts-mode-indent-broken-offset
                  scenario "Scenario [After Punctuation: '=>']"))))
      ;; Newline after case_statement_alternative
      (when-let* (((not anchor))
                  ((not node))
                  ((string-equal prev-node-t "case_statement_alternative")))
        (setq anchor (treesit-node-start prev-node)
              offset ada-ts-mode-indent-offset
              scenario "Scenario [Newline after case_statement_alternative]"))
      ;; Newline after handled_sequence_of_statements
      (when-let* (((not anchor))
                  ((not node))
                  ((string-equal prev-node-t "handled_sequence_of_statements"))
                  (last-pos (1- (treesit-node-end prev-node)))
                  (last-node (treesit-node-at last-pos))
                  (statement-node
                   (treesit-parent-until
                    last-node
                    (lambda (node)
                      (ada-ts-mode--statement-p node))
                    'include-node)))
        (setq anchor (treesit-node-start statement-node)
              offset 0
              scenario "Scenario [Newline after handled_sequence_of_statements]"))
      ;; After Punctuation: ":="
      (when-let* (((not anchor))
                  ((string-equal prev-node-t ":="))
                  (prev-anchor-node (ada-ts-mode--matching-prev-node
                                     prev-node
                                     (lambda (node)
                                       (let ((node-t (treesit-node-type node)))
                                         (or (member node-t '("begin" "is" "(" ";"))
                                             (string-equal
                                              (treesit-node-type
                                               (treesit-node-at (1- (treesit-node-end node))))
                                              ";"))))))
                  (anchor-node (ada-ts-mode--next-node prev-anchor-node)))
        (setq anchor (treesit-node-start anchor-node)
              offset ada-ts-mode-indent-broken-offset
              scenario "Scenario [After Punctuation: ':=']"))
      ;; After Keywords: ada-ts-mode-indent-offset
      (when-let* (((not anchor))
                  ((member prev-node-t '("begin" "declare" "else" "exception" "generic" "private" "record" "select"))))
        (setq anchor (save-excursion
                       (goto-char (treesit-node-start prev-node))
                       (back-to-indentation)
                       (point))
              offset ada-ts-mode-indent-offset
              scenario "Scenario [After Keywords: ada-ts-mode-indent-offset]"))
      ;; After elsif_statement_item
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "elsif_statement_item")))
        (setq anchor (treesit-node-start prev-node)
              offset ada-ts-mode-indent-offset
              scenario "Scenario [After elsif_statement_item]"))
      ;; After keywords
      (when-let* (((not anchor))
                  ((member prev-node-t ada-ts-mode--keywords)))
        (setq anchor (save-excursion
                       (goto-char (treesit-node-start prev-node))
                       (back-to-indentation)
                       (point))
              offset (if (and node
                              (member
                               (treesit-node-type (treesit-node-at (treesit-node-start node)))
                               ada-ts-mode--keywords))
                         0 ;; Adjacent keywords (e.g., "overriding function")
                       ada-ts-mode-indent-broken-offset)
              scenario "Scenario [After keywords]"))
      ;; Previous Punctuation: "("
      (when-let* (((not anchor))
                  ((string-equal prev-node-t "(")))
        (setq anchor (treesit-node-start prev-node)
              offset 1
              scenario "Scenario [Previous Punctuation: '(']"))
      ;; Fallback
      (unless anchor
        (setq scenario "Scenario [fallback]")
        (if (string-equal
             (treesit-node-type
              (treesit-node-at (1- (treesit-node-end prev-node))))
             ";")
            (setq anchor (treesit-node-start prev-node)
                  offset 0)
          (setq anchor (treesit-node-start prev-node)
                offset (or offset ada-ts-mode-indent-broken-offset))))
      (cond ((eq op 'anchor)
             (when treesit--indent-verbose
               (message scenario)
               (message "Anchor: %s" anchor))
             anchor)
            ((eq op 'offset)
             (when treesit--indent-verbose
               (message scenario)
               (message "Offset: %s" offset))
             offset)
            (t (error "Unknown op: %s" op))))))

(defun ada-ts-mode--anchor-of-indent-error-recovery ()
  "Determine indentation anchor of error recovery point."
  (ada-ts-mode--indent-error-recovery 'anchor))

(defun ada-ts-mode--offset-of-indent-error-recovery ()
  "Determine indentation offset of error recovery point."
  (ada-ts-mode--indent-error-recovery 'offset))

(defun ada-ts-mode--first-child-matching (parent type)
  "Find first child of PARENT matching TYPE.
Return nil if no child of that type is found."
  (car
   (treesit-filter-child
    parent
    (lambda (n)
      (equal (treesit-node-type n) type)))))

(defun ada-ts-mode--after-first-sibling-p (sibling)
  "Determine if the location of node comes after SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
               (ada-ts-mode--first-child-matching parent sibling)))
        (< (treesit-node-start sibling-node) bol))))

(defun ada-ts-mode--before-first-sibling-p (sibling)
  "Determine if the location of node comes before SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
               (ada-ts-mode--first-child-matching parent sibling)))
        (> (treesit-node-start sibling-node) bol))))

(defun ada-ts-mode--between-siblings-p (first-sibling last-sibling)
  "Deterine if node is between FIRST-SIBLING and LAST-SIBLING."
  (lambda (node parent bol &rest _)
    (let ((after (ada-ts-mode--after-first-sibling-p first-sibling))
          (before (ada-ts-mode--before-first-sibling-p last-sibling)))
      (and (funcall after node parent bol)
           (funcall before node parent bol)))))

(defun ada-ts-mode--anchor-first-sibling-matching (type &rest types)
  "Position of first sibling of node whose type matches TYPE.

If TYPES is provided, then match the first sibling whose type
matches any of the types in TYPE or TYPES."
  (lambda (_n parent &rest _)
    (let ((all-types (cons type types)))
      (treesit-node-start
       (car
        (treesit-filter-child
         parent
         (lambda (n)
           (member (treesit-node-type n) all-types))))))))

(defun ada-ts-mode--anchor-bol-last-child-of-first-sibling-matching (sibling-type child-type)
  "Position of last child of first sibling matching CHILD-TYPE and SIBLING-TYPE.

If BOL is specified, the position is the start of the indentation
of the line rather than the start position of the node."
  (lambda (_n parent &rest _)
    (let* ((sibling-node
            (car
             (treesit-filter-child
              parent
              (lambda (n)
                (equal (treesit-node-type n) sibling-type)))))
           (child-nodes
            (treesit-filter-child
             sibling-node
             (lambda (n)
               (equal (treesit-node-type n) child-type))))
           (last-child-node (car (reverse child-nodes)))
           (last-child-start (treesit-node-start last-child-node)))
      (save-excursion
        (goto-char last-child-start)
        (back-to-indentation)
        (point)))))


(defun ada-ts-mode--sibling-exists-p (sibling)
  "Determine if SIBLING exists."
  (lambda (_node parent _bol &rest _)
    (ada-ts-mode--first-child-matching parent sibling)))

(defun ada-ts-mode--sibling-child-exists-p (sibling child)
  "Determine if SIBLING and CHILD of SIBLING exists."
  (lambda (_node parent _bol &rest _)
    (when-let* ((sibling-node (ada-ts-mode--first-child-matching parent sibling)))
      (ada-ts-mode--first-child-matching sibling-node child))))

(defun ada-ts-mode--next-sibling (node parent bol &rest _)
  "Determine next sibling in PARENT after this NODE or BOL."
  (if node
      (treesit-node-next-sibling node)
    (car
     (treesit-filter-child
      parent
      (lambda (n)
        (> (treesit-node-start n) bol))))))

(defalias 'ada-ts-mode--next-sibling-exists-p
  'ada-ts-mode--next-sibling)

(defun ada-ts-mode--next-sibling-not-matching (type &rest types)
  "Locate next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let ((all-types (cons type types))
          (sibling-node (ada-ts-mode--next-sibling node parent bol)))
      (while (and sibling-node
                  (seq-some (lambda (a-type)
                              (equal (treesit-node-type sibling-node) a-type))
                            all-types))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'ada-ts-mode--next-sibling-not-matching-exists-p
  'ada-ts-mode--next-sibling-not-matching)

(defun ada-ts-mode--anchor-of-next-sibling-not-matching (type &rest types)
  "Determine indentation anchor of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'ada-ts-mode--next-sibling-not-matching all-types) node parent bol)))
      (car (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun ada-ts-mode--offset-of-next-sibling-not-matching (type &rest types)
  "Determine indentation offset of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'ada-ts-mode--next-sibling-not-matching all-types) node parent bol)))
      (cdr (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun ada-ts-mode--prev-sibling (_node parent bol &rest _)
  "Determine previous sibling in PARENT before this NODE or BOL."
  (car
   (reverse
    (treesit-filter-child
     parent
     (lambda (n)
       (and (not (string-equal (treesit-node-type n) "comment"))
            (< (treesit-node-start n) bol)))))))

(defun ada-ts-mode--prev-sibling-matches-p (type)
  "Check if previous sibling matches TYPE."
  (lambda (node parent bol &rest _)
    (if-let* ((prev (ada-ts-mode--prev-sibling node parent bol)))
        (string-equal (treesit-node-type prev) type))))

(defun ada-ts-mode--anchor-prev-sibling-matching (sibling-type)
  "Locate previous sibling matching SIBLING-TYPE."
  (lambda (_node parent bol &rest _)
    (treesit-node-start
     (car
      (last
       (treesit-filter-child
        parent
        (lambda (n)
          (and
           (equal (treesit-node-type n) sibling-type)
           (< (treesit-node-start n) bol)))))))))

(defun ada-ts-mode--node-first-child-matches-p (type)
  "Check if first child of node matches TYPE."
  (lambda (node _parent _bol &rest _)
    (when-let* ((first-child-node (treesit-node-child node 0))
                (first-child-node-t (treesit-node-type first-child-node)))
      (string-equal type first-child-node-t))))

(defun ada-ts-mode--parent-first-child-matches-p (type)
  "Check if first child of parent node matches TYPE."
  (lambda (_node parent _bol &rest _)
    (when-let* ((first-child-node (treesit-node-child parent 0))
                (first-child-node-t (treesit-node-type first-child-node)))
      (string-equal type first-child-node-t))))

(defun ada-ts-mode--anchor-grand-parent-bol ()
  "Locate BOL of grand-parent."
  (lambda (_node parent _bol &rest _)
    (save-excursion
      (goto-char (treesit-node-start (treesit-node-parent parent)))
      (back-to-indentation)
      (point))))

(defvar ada-ts-mode--indent-rules
  `((ada

     ;; Error recover rules.

     ((or (parent-is "ERROR")
          (node-is "ERROR")
          (ada-ts-mode--prev-sibling-matches-p "ERROR")
          no-node)
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))

     ;; top-level
     ((query ([(compilation      _ @node)
               (compilation_unit _ @node)]))
      ;; ((or (parent-is ,(rx bos "compilation" eos))
      ;;      (parent-is ,(rx bos "compilation_unit" eos)))
      column-0 0)
     ;; with_clause / use_clause
     ((query ([(with_clause [(identifier) (selected_component)]
                            [(identifier) (selected_component) ","] @node)
               (use_clause [(identifier) (selected_component)]
                           [(identifier) (selected_component) ","] @node)]))
      ;; ((and (or (parent-is "with_clause")
      ;;           (parent-is "use_clause"))
      ;;       (or (node-is "identifier")
      ;;           (node-is "selected_component")
      ;;           (node-is ","))
      ;;       (or (ada-ts-mode--after-first-sibling-p "identifier")
      ;;           (ada-ts-mode--after-first-sibling-p "selected_component")))
      (ada-ts-mode--anchor-first-sibling-matching "identifier" "selected_component")
      0)

     ;; subunit
     ((query ((subunit [(subprogram_body) (package_body) (task_body) (protected_body)] @node)))
      ;; ((and (parent-is "subunit")
      ;;       (or (node-is ,(rx bos "subprogram_body" eos))
      ;;           (node-is ,(rx bos "package_body" eos))
      ;;           (node-is ,(rx bos "task_body" eos))
      ;;           (node-is ,(rx bos "protected_body" eos))))
      column-0 0)

     ((query ((subunit [(identifier) (selected_component)] @node)))
      ;; ((and (parent-is "subunit")
      ;;       (or (node-is "identifier")
      ;;           (node-is "selected_component")))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)

     ;; aspect_mark_list / aspect_association
     ((query ((aspect_specification) @node))
      ;; ((node-is "aspect_specification")
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ((query ((aspect_mark_list) @node))
      ;; ((node-is "aspect_mark_list")
      parent
      ada-ts-mode-indent-broken-offset)
     ((query ((aspect_mark_list _ @node)))
      ;; ((parent-is "aspect_mark_list")
      parent
      0)
     ((query ((aspect_association _ @node "=>")))
      ;; ((and (parent-is "aspect_association")
      ;;       (ada-ts-mode--before-first-sibling-p "=>"))
      parent
      0)

     ;; expression
     ((query ([(array_delta_aggregate (expression) @node)
               (record_delta_aggregate (expression) @node)]))
      ;; ((and (or (parent-is "array_delta_aggregate")
      ;;           (parent-is "record_delta_aggregate"))
      ;;       (node-is ,(rx bos "expression" eos)))
      parent
      1)
     ((query ((expression_function_declaration (expression) @node)))
      ;; ((and (parent-is "expression_function_declaration")
      ;;       (node-is ,(rx bos "expression" eos)))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)
     ((query ((declare_expression (expression) @node)))
      ;; ((and (parent-is ,(rx bos "declare_expression"))
      ;;      (node-is ,(rx bos "expression" eos)))
      parent
      ada-ts-mode-indent-offset)
     ((query ([(if_expression         "then" (expression) @node)
               (elsif_expression_item "then" (expression) @node)
               (case_expression_alternative  (expression) @node)]))
      ;; ((and (or (parent-is ,(rx bos "if_expression" eos))
      ;;           (parent-is ,(rx bos "elsif_expression_item" eos)))
      ;;       (node-is ,(rx bos "expression" eos))
      ;;       (ada-ts-mode--after-first-sibling-p "then"))
      parent
      ada-ts-mode-indent-offset)
     ;; ((query ((case_expression_alternative (expression) @node)))
     ;;  ;; ((and (parent-is ,(rx bos "case_expression_alternative" eos))
     ;;  ;;       (node-is ,(rx bos "expression" eos)))
     ;;  parent
     ;;  ada-ts-mode-indent-offset)
     ((query ((expression) @node))
      ;; ((node-is ,(rx bos "expression" eos))
      parent
      ada-ts-mode-indent-broken-offset)
     ((query ((expression _ @node)))
      ;; ((parent-is ,(rx bos "expression" eos))
      parent
      ada-ts-mode-indent-exp-item-offset)

     ;; discrete_choice_list
     ((query ((discrete_choice_list _ @node)))
      ;; ((parent-is "discrete_choice_list")
      parent
      0)
     ((query ((discrete_choice_list) @node))
      ;; ((node-is "discrete_choice_list")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; case_statement / case_statement_alternative
     ((query ((case_statement_alternative) @node))
      ;; ((node-is "case_statement_alternative")
      parent
      ada-ts-mode-indent-when-offset)
     ((query ((case_statement_alternative "=>" _ @node)))
      ;; ((and (parent-is "case_statement_alternative")
      ;;       (ada-ts-mode--after-first-sibling-p "=>"))
      parent
      ada-ts-mode-indent-offset)
     ;; ((and (parent-is ,(rx bos "case_statement" eos))
     ;;       no-node
     ;;       (ada-ts-mode--between-siblings-p "case_statement_alternative" "end"))
     ;;  (ada-ts-mode--anchor-prev-sibling-matching "case_statement_alternative")
     ;;  ada-ts-mode-indent-offset)
     ((query ((case_statement "is" (comment) @node "end")))
      ;; ((and (parent-is ,(rx bos "case_statement" eos))
      ;;       (or no-node (node-is "comment"))
      ;;       (ada-ts-mode--between-siblings-p "is" "end"))
      parent
      ada-ts-mode-indent-when-offset)

     ;; case_expression_alternative
     ((query ((case_expression_alternative) @node))
      ;; ((node-is "case_expression_alternative")
      parent
      ada-ts-mode-indent-when-offset)

     ;; if_expression / case_expression / declare_expression / quantified_expression
     ((query ([(if_expression)
               (case_expression)
               (declare_expression)
               (quantified_expression)]
              @node))
      ;; ((or (node-is ,(rx bos "if_expression" eos))
      ;;     (node-is ,(rx bos "case_expression" eos))
      ;;     (node-is ,(rx bos "declare_expression" eos))
      ;;     (node-is ,(rx bos "quantified_expression" eos)))
      (ada-ts-mode--anchor-prev-sibling-matching "(")
      1)

     ;; variant_part / variant_list / component_list / record_definition
     ;; ((and (parent-is "record_definition")
     ;;       (ada-ts-mode--between-siblings-p "record" "end"))
     ;;  parent-bol
     ;;  ada-ts-mode-indent-offset)
     ((query ((component_list _ @node))) parent 0)
     ;; ((parent-is "component_list") parent 0)
     ;; ((and (parent-is "variant_part")
     ;;       no-node
     ;;       (ada-ts-mode--between-siblings-p "variant_list" "end"))
     ;;  (ada-ts-mode--anchor-prev-sibling-matching "variant_list")
     ;;  ada-ts-mode-indent-offset)

     ((query ((variant_part "is" _ @node "end")))
      ;; ((and (parent-is "variant_part")
      ;;        (ada-ts-mode--between-siblings-p "is" "end"))
      parent
      ada-ts-mode-indent-when-offset)
     ((query ((variant_list _ @node))) parent 0)
     ;; ((parent-is "variant_list") parent 0)
     ((query ((variant "=>" _ @node)))
      ;; ((and (parent-is ,(rx bos "variant" eos))
      ;;       (ada-ts-mode--after-first-sibling-p "=>"))
      parent
      ada-ts-mode-indent-offset)

     ;; parameter_specification
     ((query ((_ (parameter_specification) (parameter_specification) @node)))
      ;; ((and (node-is ,(rx bos "parameter_specification" eos))
      ;;       (ada-ts-mode--after-first-sibling-p "parameter_specification"))
      (ada-ts-mode--anchor-first-sibling-matching "parameter_specification")
      0)
     ((query ((parameter_specification) @node))
      ;; ((node-is ,(rx bos "parameter_specification" eos))
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; result_profile
     ((query ((result_profile) @node))
      ;; ((node-is "result_profile")
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ((query ((result_profile _ @node)))
      ;; ((parent-is "result_profile")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; access_definition
     ((query ((access_definition subtype_mark: _ @node)))
      ;; ((and (field-is "subtype_mark")
      ;;       (parent-is "access_definition"))
      parent
      0)

     ;; parameter_association
     ((query ((_ (parameter_association) (parameter_association) @node)))
      ;; ((and (node-is "parameter_association")
      ;;       (ada-ts-mode--after-first-sibling-p "parameter_association"))
      (ada-ts-mode--anchor-first-sibling-matching "parameter_association")
      0)
     ((query ((parameter_association) @node))
      ;; ((node-is "parameter_association")
      parent-bol
      ada-ts-mode-indent-broken-offset)

     ;; named_array_aggregate / array_delta_aggregate
     ((query ([(named_array_aggregate (array_component_association)
                                      [(array_component_association) ","] @node)
               (array_delta_aggregate (array_component_association)
                                      [(array_component_association) ","] @node)]))
      ;; ((and (or (parent-is "named_array_aggregate")
      ;;           (parent-is "array_delta_aggregate"))
      ;;       (or (node-is "array_component_association")
      ;;           (node-is ","))
      ;;       (ada-ts-mode--after-first-sibling-p "array_component_association"))
      (ada-ts-mode--anchor-first-sibling-matching "array_component_association")
      0)
     ((query ((named_array_aggregate (array_component_association) @node)))
      ;; ((and (parent-is "named_array_aggregate")
      ;;       (node-is "array_component_association"))
      parent
      1)
     ((query ((array_delta_aggregate ["with" "delta" (array_component_association)] @node)))
      ;; ((and (parent-is "array_delta_aggregate")
      ;;       (or (node-is "with")
      ;;           (node-is "delta")
      ;;           (node-is "array_component_association")))
      (ada-ts-mode--anchor-prev-sibling-matching "expression")
      ada-ts-mode-indent-broken-offset)

     ;; record_component_association_list
     ((query ((record_component_association_list _ @node)))
      ;; ((parent-is "record_component_association_list")
      parent
      0)

     ;; record_delta_aggregate
     ((query ((record_delta_aggregate ["with" "delta" (record_component_association_list)] @node)))
      ;; ((and (parent-is "record_delta_aggregate")
      ;;       (or (node-is "with")
      ;;           (node-is "delta")
      ;;           (node-is "record_component_association_list")))
      (ada-ts-mode--anchor-prev-sibling-matching "expression")
      ada-ts-mode-indent-broken-offset)

     ;; enumeration_type_definition
     ((query ((enumeration_type_definition
               [(identifier) (character_literal)]
               [(identifier) (character_literal) ","] @node)))
      (ada-ts-mode--anchor-first-sibling-matching "identifier" "character_literal")
      0)
     ((query ((enumeration_type_definition [(identifier) (character_literal)] @node)))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)

     ;; ((and (parent-is "enumeration_type_definition")
     ;;       (or (node-is "identifier")
     ;;           (node-is "character_literal")
     ;;           (node-is ","))
     ;;       (or (ada-ts-mode--after-first-sibling-p "identifier")
     ;;           (ada-ts-mode--after-first-sibling-p "character_literal")))
     ;;  (ada-ts-mode--anchor-first-sibling-matching "identifier" "character_literal")
     ;;  0)
     ;; ((and (parent-is "enumeration_type_definition")
     ;;       (or (node-is "identifier")
     ;;           (node-is "character_literal")))
     ;;  (ada-ts-mode--anchor-first-sibling-matching "(")
     ;;  1)

     ;; pragma_argument_association
     ((query ((_ (pragma_argument_association)
                 (pragma_argument_association) @node)))
      (ada-ts-mode--anchor-first-sibling-matching "pragma_argument_association")
      0)
     ((query ((pragma_argument_association) @node))
      parent
      ada-ts-mode-indent-broken-offset)


     ;; ((and (node-is "pragma_argument_association")
     ;;       (ada-ts-mode--after-first-sibling-p "pragma_argument_association"))
     ;;  (ada-ts-mode--anchor-first-sibling-matching "pragma_argument_association")
     ;;  0)
     ;; ((node-is "pragma_argument_association")
     ;;  parent
     ;;  ada-ts-mode-indent-broken-offset)

     ;; exception_declaration
     ((query ((exception_declaration _ @node)))
      ;; ((parent-is "exception_declaration")
      parent
      0)

     ;; extended_return_object_declaration
     ((query ((extended_return_object_declaration) @node))
      ;; ((node-is "extended_return_object_declaration")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; protected_definition
     ((query ((protected_definition :anchor ["private" "end"]) @node))
      parent
      0)
     ((query ((protected_definition) @node))
      parent
      ada-ts-mode-indent-offset)
     ((and (query ((protected_definition ["private" "end"] @node)))
           (n-p-gp nil nil "ERROR"))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ((protected_definition ["private" "end"] @node)))
      grand-parent
      0)
     ((and (query ((protected_definition "end" (identifier) @node)))
           (n-p-gp nil nil "ERROR"))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ((protected_definition "end" (identifier) @node)))
      grand-parent
      ada-ts-mode-indent-broken-offset)
     ((query ((protected_definition :anchor "private" (_) @node)))
      parent
      ada-ts-mode-indent-offset)
     ((query ((protected_definition (_) @node)))
      parent
      0)

     ;; task_definition
     ((query ((task_definition :anchor ["private" "end"]) @node))
      parent
      0)
     ((query ((task_definition) @node))
      parent
      ada-ts-mode-indent-offset)
     ((and (query ((task_definition ["private" "end"] @node)))
           (n-p-gp nil nil "ERROR"))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ((task_definition ["private" "end"] @node)))
      grand-parent
      0)
     ((and (query ((task_definition "end" (identifier) @node)))
           (n-p-gp nil nil "ERROR"))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ((task_definition "end" (identifier) @node)))
      grand-parent
      ada-ts-mode-indent-broken-offset)
     ((query ((task_definition :anchor "private" (_) @node)))
      parent
      ada-ts-mode-indent-offset)
     ((query ((task_definition (_) @node)))
      parent
      0)

     ;; generic_instantiation
     ((query ((generic_instantiation generic_name: _ @node)))
      ;; ((and (parent-is "generic_instantiation")
      ;;       (field-is "generic_name"))
      parent
      ada-ts-mode-indent-broken-offset)

     ;; discriminant_specification_list / discriminant_specification
     ((query ((_ (discriminant_specification) [(discriminant_specification) ";"] @node)))
      ;; ((and (or (node-is ,(rx bos "discriminant_specification" eos))
      ;;           (node-is ";"))
      ;;       (ada-ts-mode--after-first-sibling-p "discriminant_specification"))
      (ada-ts-mode--anchor-first-sibling-matching "discriminant_specification")
      0)
     ((query ((discriminant_specification_list) @node))
      ;; ((node-is "discriminant_specification_list")
      (ada-ts-mode--anchor-first-sibling-matching "(")
      1)

     ;; null_procedure_declaration / expression_function_declaration / abstract subprogram_declaration
     ((query ([(expression_function_declaration "is" @node)
               (null_procedure_declaration      "is" @node)
               (subprogram_declaration          "is" @node "abstract")]))
      ;; ((and (node-is ,(rx bos "is" eos))
      ;;       (or (parent-is "expression_function_declaration")
      ;;           (parent-is "null_procedure_declaration")))
      parent-bol
      ada-ts-mode-indent-subprogram-is-offset)
     ((query ([(null_procedure_declaration "null" @node)
               (subprogram_declaration "abstract" @node)]))
      ;; ((and (node-is ,(rx bos "null" eos))
      ;;       (parent-is "null_procedure_declaration"))
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ;; keywords / semicolon
     ((and (query ((handled_sequence_of_statements "exception" @node)))
           (n-p-gp nil nil "ERROR"))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ((handled_sequence_of_statements "exception" @node)))
      (ada-ts-mode--anchor-grand-parent-bol)
      0)

     ;; prevent keywords from aligning to parent BOL.
     ((query ([(if_expression ["then" "else" (elsif_expression_item)] @node)
               (case_expression "is" @node)
               (declare_expression "begin" @node)]))
      parent
      0)
     ((query ((declare_expression (_) @node "begin")))
      parent
      ada-ts-mode-indent-offset)
     ((query ((quantifier) @node))
      ;; ((node-is ,(rx bos "quantifier" eos))
      parent
      ada-ts-mode-indent-broken-offset)

     ;; Handle special record type indentation.
     ((query ([(record_definition) (record_type_definition) (record_representation_clause)] @node))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ([(record_definition _ @node)
               (record_representation_clause _ @node)]))
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))
     ;; ((query ((record_type_definition (record_definition :anchor "record")) @node)) parent            ada-ts-mode-indent-record-offset)
     ;; ((query ((record_type_definition) @node))                                      parent            ada-ts-mode-indent-offset)
     ;; ((query ((record_definition :anchor "record") @node))                          grand-parent      ada-ts-mode-indent-record-offset)
     ;; ((query ((record_definition :anchor "null") @node))                            grand-parent      ada-ts-mode-indent-offset)
     ;; ((query ((record_definition (component_list) @node)))                          standalone-parent ada-ts-mode-indent-offset)
     ;; ((query ((record_definition (identifier) @node)))                              standalone-parent ada-ts-mode-indent-broken-offset)
     ;; ((query ((record_definition ["end" "record"] @node)))                          standalone-parent 0)


     ;; ((or (node-is ,(rx bos "record_definition" eos))
     ;;      (node-is ,(rx bos "record_type_definition" eos))
     ;;      (parent-is ,(rx bos "record_definition" eos))
     ;;      (node-is ,(rx bos "record_representation_clause" eos))
     ;;      (parent-is ,(rx bos "record_representation_clause" eos)))
     ;;  (ada-ts-mode--anchor-of-indent-error-recovery)
     ;;  (ada-ts-mode--offset-of-indent-error-recovery))
     ((query ([ ,@ada-ts-mode--keywords ";"
                (elsif_statement_item)
                (aspect_specification)
                (null_exclusion)
                (access_to_object_definition)
                (access_to_subprogram_definition)
                (procedure_specification)
                (function_specification)
                (allocator)]
              @node))
      ;; ((or (node-is ,(eval `(rx bos (or ,@ada-ts-mode--keywords ";") eos)))
      ;;      ;; (node-is "record_type_definition")
      ;;      ;; (node-is "record_definition")
      ;;      (node-is "elsif_statement_item")
      ;;      (node-is "aspect_specification")
      ;;      (node-is "null_exclusion")
      ;;      (node-is "access_to_object_definition")
      ;;      (node-is "access_to_subprogram_definition")
      ;;      (node-is "procedure_specification")
      ;;      (node-is "function_specification")
      ;;      (node-is ,(rx bos "allocator" eos)))
      parent-bol
      0)

     ;; loop_statement / loop_parameter_specification / iterator_specification
     ((query ((loop_statement [(loop_label) (iteration_scheme)] @node)))
      ;; ((and (parent-is "loop_statement")
      ;;       (or (node-is "loop_label")
      ;;           (node-is "iteration_scheme")))
      parent
      0)
     ((query ((loop_parameter_specification) @node))
      ;; ((node-is "loop_parameter_specification")
      parent
      ada-ts-mode-indent-broken-offset)
     ((query ((loop_parameter_specification _ @node)))
      ;; ((parent-is "loop_parameter_specification")
      parent
      0)
     ((query ((iterator_specification) @node))
      ;; ((node-is "iterator_specification")
      parent
      ada-ts-mode-indent-broken-offset)
     ((query ((iterator_specification _ @node)))
      ;; ((parent-is "iterator_specification")
      parent
      0)

     ;; handled_sequence_of_statements / exception_handler / exception_choice_list
     ((query ((exception_choice_list (exception_choice) _ @node)))
      ;; ((and (parent-is "exception_choice_list")
      ;;       (ada-ts-mode--after-first-sibling-p "exception_choice"))
      (ada-ts-mode--anchor-first-sibling-matching "exception_choice")
      0)
     ((query ((exception_handler "=>" _ @node)))
      ;; ((and (parent-is "exception_handler")
      ;;       (ada-ts-mode--after-first-sibling-p "=>"))
      parent
      ada-ts-mode-indent-offset)
     ((query ((exception_handler _ @node)))
      ;; ((parent-is "exception_handler")
      parent
      ada-ts-mode-indent-broken-offset)
     ((query ((_ (exception_handler) (exception_handler) @node)))
      ;; ((and (node-is "exception_handler")
      ;;       (ada-ts-mode--after-first-sibling-p "exception_handler"))
      (ada-ts-mode--anchor-first-sibling-matching "exception_handler")
      0)
     ;; ((and (parent-is "handled_sequence_of_statements")
     ;;       no-node
     ;;       (ada-ts-mode--after-first-sibling-p "exception_handler"))
     ;;  (ada-ts-mode--anchor-prev-sibling-matching "exception_handler")
     ;;  ada-ts-mode-indent-offset)
     ;; ((and (ada-ts-mode--between-siblings-p
     ;;        "handled_sequence_of_statements"
     ;;        "end")
     ;;       (ada-ts-mode--sibling-child-exists-p
     ;;        "handled_sequence_of_statements"
     ;;        "exception_handler")
     ;;       no-node)
     ;;  (ada-ts-mode--anchor-bol-last-child-of-first-sibling-matching
     ;;   "handled_sequence_of_statements"
     ;;   "exception_handler")
     ;;  ada-ts-mode-indent-offset)
     ((query ((handled_sequence_of_statements _ @node)))
      ;; ((parent-is "handled_sequence_of_statements")
      parent-bol
      ;; (ada-ts-mode--anchor-grand-parent-bol)
      ;; ada-ts-mode-indent-offset)
      0)
     ((query ((handled_sequence_of_statements) @node))
      ;; ((node-is "handled_sequence_of_statements")
      parent-bol
      ada-ts-mode-indent-offset)

     ((query ((non_empty_declarative_part _ @node)))
      ;; ((parent-is "non_empty_declarative_part")
      parent-bol
      0)
     ((query ((non_empty_declarative_part) @node))
      ;; ((node-is "non_empty_declarative_part")
      parent-bol
      ada-ts-mode-indent-offset)

     ((and (parent-is "selective_accept")
           ;; (node-is "select_alternative")
           (ada-ts-mode--prev-sibling-matches-p "guard"))
      (ada-ts-mode--anchor-prev-sibling-matching "guard")
      ada-ts-mode-indent-offset)
     ((and (parent-is "selective_accept")
           (ada-ts-mode--prev-sibling-matches-p "select_alternative"))
      (ada-ts-mode--anchor-prev-sibling-matching "select_alternative")
      0)
     ;; ((and (parent-is "selective_accept")
     ;;       (or (node-is "or")
     ;;           (node-is "else")))
     ;;  parent
     ;;  0)
     ((parent-is "selective_accept")
      parent
      ada-ts-mode-indent-offset)

     ((query ([(subprogram_body           "is"      _ @node "end")
               (package_body              "is"      _ @node "end")
               (package_declaration       "is"      _ @node "end")
               (task_body                 "is"      _ @node "end")
               (entry_body                "is"      _ @node "end")
               (protected_body            "is"      _ @node "end")
               (extended_return_statement "do"      _ @node "end")
               (block_statement           "declare" _ @node "begin")
               (block_statement           "begin"   _ @node "end")
               (loop_statement            "loop"    _ @node "end")
               (if_statement              "then"    _ @node "end")
               (elsif_statement_item      "then"    _ @node)]))
      parent-bol
      ada-ts-mode-indent-offset)

     ;; (
     ;;  (or (and (or (parent-is "subprogram_body")
     ;;               (parent-is "package_body")
     ;;               (parent-is "package_declaration")
     ;;               (parent-is "task_body")
     ;;               (parent-is "entry_body")
     ;;               (parent-is "protected_body"))
     ;;           (or (ada-ts-mode--between-siblings-p "is" "end")
     ;;               ;; (ada-ts-mode--between-siblings-p "begin" "end")
     ;;               ))
     ;;      (and (parent-is "extended_return_statement")
     ;;           (ada-ts-mode--between-siblings-p "do" "end"))
     ;;      (and (parent-is "block_statement")
     ;;           (or (ada-ts-mode--between-siblings-p "declare" "begin")
     ;;               (ada-ts-mode--between-siblings-p "begin" "end")
     ;;               ))
     ;;      (and (parent-is "loop_statement")
     ;;           (ada-ts-mode--between-siblings-p "loop" "end"))
     ;;      (and (parent-is ,(rx bos "if_statement" eos))
     ;;           (ada-ts-mode--between-siblings-p "then" "end"))
     ;;      (and (parent-is "elsif_statement_item")
     ;;           (ada-ts-mode--after-first-sibling-p "then"))
     ;;      )
     ;;  parent-bol
     ;;  ada-ts-mode-indent-offset)

     ;; non_empty_declarative_part
     ;; ((node-is "non_empty_declarative_part") ; first item / pragma
     ;;  parent
     ;;  ada-ts-mode-indent-offset)
     ;; ((parent-is "non_empty_declarative_part") ; remaining items / pragmas
     ;;  grand-parent
     ;;  ada-ts-mode-indent-offset)


     ;; general indentation for comments.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and (node-is "comment")
           ;; (or no-node (node-is "comment"))
           ;;      (ada-ts-mode--next-sibling-not-matching-exists-p "comment" "ERROR"))
           ;; (ada-ts-mode--anchor-of-next-sibling-not-matching "comment" "ERROR")
           ;; (ada-ts-mode--offset-of-next-sibling-not-matching "comment" "ERROR"))
           (ada-ts-mode--next-sibling-not-matching-exists-p "comment"))
      (ada-ts-mode--anchor-of-next-sibling-not-matching "comment")
      (ada-ts-mode--offset-of-next-sibling-not-matching "comment"))

     ;; identifier / selected_component
     ((query ([(identifier) (selected_component)] @node)) parent-bol ada-ts-mode-indent-broken-offset)
     ((query ((selected_component _ @node)))              parent-bol ada-ts-mode-indent-broken-offset)
     ;; ((or (node-is "identifier")
     ;;      (node-is "selected_component")
     ;;      (parent-is "selected_component"))
     ;;  parent-bol
     ;;  ada-ts-mode-indent-broken-offset)

     ;; non-expression opening parenthesis
     ;; ((and (node-is "(")
     ;;       (parent-is "pragma_g"))
     ;;  (ada-ts-mode--anchor-first-sibling-matching "identifier")
     ;;  ada-ts-mode-indent-broken-offst)
     ((query ([(formal_part)
               (enumeration_aggregate)
               (enumeration_type_definition)
               (actual_parameter_part)
               (known_discriminant_part)
               (unknown_discriminant_part)
               "("]
              @node))
      ;; ((or (node-is "formal_part")
      ;;      (node-is "enumeration_aggregate")
      ;;      (node-is "enumeration_type_definition")
      ;;      (node-is "actual_parameter_part")
      ;;      (node-is "known_discriminant_part")
      ;;      (node-is "unknown_discriminant_part")
      ;;      (node-is "("))
      parent-bol
      ada-ts-mode-indent-broken-offset)
     ;; closing parenthesis (including expression)
     ((query ((_ "(" ")" @node)))
      ;; ((and (node-is ")")
      ;;       (ada-ts-mode--sibling-exists-p "("))
      (ada-ts-mode--anchor-first-sibling-matching "(")
      0)
     ((query ((_ "[" "]" @node)))
      ;; ((and (node-is "]")
      ;;       (ada-ts-mode--sibling-exists-p "["))
      (ada-ts-mode--anchor-first-sibling-matching "[")
      0)

     ((query ([":" ":="] @node))
      ;; ((or (node-is ,(rx bos ":" eos))
      ;;      (node-is ,(rx bos ":=" eos)))
      parent 0)

     ((query ("=>" @node))
      ;; ((node-is "=>")
      parent
      ada-ts-mode-indent-broken-offset)

     ;; trival recovery for syntax error or unexpected broken line
     ;; (catch-all prev-line 0)

     (catch-all
      (ada-ts-mode--anchor-of-indent-error-recovery)
      (ada-ts-mode--offset-of-indent-error-recovery))

     ))
  "Tree-sitter indent rules for `ada-ts-mode'.")

;;; Indent Line / Indent Region

(defun ada-ts-mode--indent-line ()
  "Perform line indentation."
  (ada-ts-mode-indent-line ada-ts-mode-indent-backend))

(defun ada-ts-mode--indent-region (beg end)
  "Perform region indentation between BEG and END."
  (ada-ts-mode-indent-region ada-ts-mode-indent-backend beg end))

(cl-defgeneric ada-ts-mode-indent-line (backend)
  "Indent line using BACKEND."
  (error "Unknown indentation backend: %s" backend))

(cl-defgeneric ada-ts-mode-indent-region (backend _beg _end)
  "Indent region between BEG and END using BACKEND."
  (error "Unknown indentation backend: %s" backend))

;;;; LSP

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql lsp)))
  "Indent line using LSP server BACKEND.

If an LSP client is not active, or the line to indent is empty, or if
the LSP region formatting function fails, fallback to tree-sitter based
indentation.

The Ada Language Server does not indent empty lines and will fail to
indent when syntax errors exist, therefore the need to fallback on
tree-sitter indentation in these scenarios."
  (if-let* ((client (lspclient/current)))
      (if (save-excursion
            (forward-line 0)
            (looking-at-p (rx (* whitespace) eol)))
          ;; Handle extraneous space as well as implement a workaround
          ;; for LSP onTypeFormatting for RET as described in
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=70929
          (ada-ts-mode-indent-line 'tree-sitter)
        (condition-case _
            (let ((initial-point-column (current-column))
                  (initial-indentation-column (current-indentation)))
              (ada-ts-mode-indent-region 'lsp (pos-bol) (pos-eol))
              ;; For consistency with built-in indentation behavior,
              ;; if point was in the white-space at the beginning of
              ;; the line, move point to the current indentation.
              (when (<= initial-point-column
                        initial-indentation-column)
                (back-to-indentation)))
          (error
           (ada-ts-mode-indent-line 'tree-sitter))))
    ;; fallback on tree-sitter indentation
    (ada-ts-mode-indent-line 'tree-sitter)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql lsp)) beg end)
  "Indent the region between BEG and END using LSP server BACKEND.

When CLIENT is not nil, use it as the active LSP client."
  (if-let* ((client (lspclient/current)))
      (let ((inhibit-message t)
            (tab-width ada-ts-mode-indent-offset)
            (standard-indent ada-ts-mode-indent-offset))
        (lspclient/format-region client beg end))
    ;; fallback on tree-sitter indentation
    (ada-ts-mode-indent-region 'tree-sitter beg end)))

;;;; Tree-sitter

(cl-defgeneric ada-ts-mode-indent (strategy)
  "Indent using tree-sitter back-end, according to STRATEGY."
  (error "Unknown indentation strategy: %s" strategy))

(cl-defmethod ada-ts-mode-indent ((_strategy (eql line)))
  "Indent using tree-sitter back-end, according to line STRATEGY."
  (treesit-indent))

(cl-defmethod ada-ts-mode-indent ((_strategy (eql aggressive)))
  "Indent using tree-sitter back-end, according to aggressive STRATEGY."
  (let ((initial-point-column (current-column))
        (initial-indentation-column (current-indentation))
        (region
         (save-excursion
           (forward-line 0)
           (skip-chars-forward " \t")
           (unless (looking-at (rx (* whitespace) eol) t)
             (let* ((node (treesit-node-at (point)))
                    (root (treesit-buffer-root-node))
                    (candidate
                     (treesit-parent-until
                      node
                      (lambda (node)
                        (or (treesit-node-eq node root)
                            (string-equal (treesit-node-type node) "ERROR")
                            (ada-ts-mode--compilation-unit-p node)))
                      'include-node)))
               (when (and (ada-ts-mode--compilation-unit-p candidate)
                          (not (treesit-search-subtree
                                candidate
                                (lambda (n)
                                  (let ((type (treesit-node-type n)))
                                    (or (string-equal type "ERROR")
                                        (treesit-node-check n 'missing)
                                        (treesit-node-check n 'has-error)))))))
                 (unless (ada-ts-mode--mismatched-names-p candidate)
                   (when treesit--indent-verbose
                     (message "Aggressive indent triggered for: %s" candidate))
                   (cons (treesit-node-start candidate)
                         (treesit-node-end candidate)))))))))
    (if region
        (progn
          (treesit-indent-region (car region) (cdr region))
          ;; Move point if it was in the indentation.
          (when (<= initial-point-column
                    initial-indentation-column)
            (back-to-indentation)))
      (treesit-indent))))

(cl-defmethod ada-ts-mode-indent-line ((_backend (eql tree-sitter)))
  "Indent line using tree-sitter BACKEND."
  (if (eq ada-ts-mode-indent-backend 'tree-sitter)
      ;; Only utilize indentation strategy when tree-sitter back-end
      ;; is configured, not when tree-sitter back-end is used as a
      ;; fallback, to avoid competing indentation styles.
      (ada-ts-mode-indent ada-ts-mode-indent-strategy)
    (ada-ts-mode-indent 'line)))

(cl-defmethod ada-ts-mode-indent-region ((_backend (eql tree-sitter)) beg end)
  "Indent the region between BEG and END using tree-sitter BACKEND."
  (treesit-indent-region beg end))

(defun ada-ts-mode--mismatched-names-p (node)
  "Determine if NODE names are mismatched."

  ;; NOTE: We need to adjust node as follows:
  ;;    object_declaration:
  ;;      - check if single_task_declaration is child, make that node
  ;;      - check if signle_protected_declaration is child, make that node
  ;;    full_type_declaration:
  ;;      - check if task_type_declaration is child, make that node
  ;;      - check if protected_type_declaration is child, make that node

  (let ((node-t (treesit-node-type node)))
    (cond ((string-equal node-t "object_declaration")
           (when-let* ((first-child-node (treesit-node-child node 0))
                       (first-child-node-t (treesit-node-type first-child-node))
                       ((member first-child-node-t
                                '("single_protected_declaration"
                                  "single_task_declaration"))))
             (setq node first-child-node)))
          ((string-equal node-t "full_type_declaration")
           (when-let* ((first-child-node (treesit-node-child node 0))
                       (first-child-node-t (treesit-node-type first-child-node))
                       ((member first-child-node-t
                                '("protected_type_declaration"
                                  "task_type_declaration"))))
             (setq node first-child-node)))))

  (catch 'not-applicable
    (let* ((node-t (treesit-node-type node))
           (name
            (cond ((member node-t
                           '("entry_body"
                             "package_body"
                             "package_declaration"
                             "protected_body"
                             "protected_type_declaration"
                             "single_protected_declaration"
                             "single_task_declaration"
                             "subprogram_body"
                             "task_body"
                             "task_type_declaration"))
                   (ada-ts-mode--defun-name node))
                  ((string-equal node-t "block_statement")
                   (let ((first-child-node (treesit-node-child node 0)))
                     (when (string-equal (treesit-node-type first-child-node) "loop_label")
                       (ada-ts-mode--node-to-name
                        (treesit-node-child-by-field-name first-child-node "statement_identifier")))))
                  (t (throw 'not-applicable nil))))
           (endname
            (let ((node node))
              (when (member node-t
                            '("protected_type_declaration"
                              "single_protected_declaration"))
                (setq node
                      (car (treesit-filter-child
                            node
                            (lambda (node)
                              (string-equal
                               (treesit-node-type node)
                               "protected_definition"))))))
              (when (member node-t
                            '("single_task_declaration"
                              "task_type_declaration"))
                (setq node
                      (car (treesit-filter-child
                            node
                            (lambda (node)
                              (string-equal
                               (treesit-node-type node)
                               "task_definition"))))))
              (when-let* ((end-node (car (treesit-filter-child
                                          node
                                          (lambda (node)
                                            (string-equal
                                             (treesit-node-type node)
                                             "end")))))
                          (next-node (ada-ts-mode--next-node end-node))
                          (next-node-t (treesit-node-type next-node))
                          ((string-equal next-node-t "identifier")))
                (ada-ts-mode--node-to-name next-node))))

           ;; (cond ((member node-t
           ;;                '("package_body"
           ;;                  "package_declaration"
           ;;                  "subprogram_body"))
           ;;        (ada-ts-mode--node-to-name
           ;;         (treesit-node-child-by-field-name node "endname")))
           ;;       ((member node-t
           ;;                '("block_statement"
           ;;                  "entry_body"
           ;;                  "protected_body"
           ;;                  "task_body"))
           ;;        (when-let* ((end-node (car (treesit-filter-child
           ;;                                    node
           ;;                                    (lambda (node)
           ;;                                      (string-equal
           ;;                                       (treesit-node-type node)
           ;;                                       "end")))))
           ;;                    (next-node (ada-ts-mode--next-node end-node))
           ;;                    (next-node-t (treesit-node-type next-node))
           ;;                    ((string-equal next-node-t "identifier")))
           ;;          (ada-ts-mode--node-to-name next-node)))
           ;;       ((member node-t
           ;;                '("protected_type_declaration"
           ;;                  "single_protected_declaration"))
           ;;        (when-let* ((protected-definition-node
           ;;                     (car (treesit-filter-child
           ;;                                    node
           ;;                                    (lambda (node)
           ;;                                      (string-equal
           ;;                                       (treesit-node-type node)
           ;;                                       "protected_definition")))))


           )
      (when treesit--indent-verbose
        (message "NAME: %s" name)
        (message "ENDNAME: %s" endname))
      (or (not name)
          (not endname)
          (not (string-equal-ignore-case name endname))))))
;; ;; (when endname-node
;; ;;   (not (string-equal name (ada-ts-mode--node-to-name endname-node))))
;; )))

;;; Node Predicates

(defun ada-ts-mode--compilation-unit-p (node)
  "Determine if NODE is a compilation unit."
  (or (ada-ts-mode--declarative-item-p node)
      (ada-ts-mode--statement-p node)
      (let ((node-type (treesit-node-type node)))
        (member node-type
                '("with_clause"
                  "subunit"
                  "entry_declaration")))))

(defun ada-ts-mode--statement-p (node)
  "Determine if NODE is a statement."
  (or (ada-ts-mode--simple-statement-p node)
      (ada-ts-mode--compound-statement-p node)))

(defun ada-ts-mode--simple-statement-p (node)
  "Determine if NODE is a simple statement."
  (let ((node-type (treesit-node-type node)))
    (member node-type
            '("null_statement"
              "assignment_statement"
              "exit_statement"
              "goto_statement"
              "procedure_call_statement"
              "simple_return_statement"
              "requeue_statement"

              ;; delay_statement
              "delay_until_statement"
              "delay_relative_statement"

              "abort_statement"
              "raise_statement"
              "pragma_g"))))

(defun ada-ts-mode--compound-statement-p (node)
  "Determine if NODE is a compound statement."
  (let ((node-type (treesit-node-type node)))
    (member node-type
            '("if_statement"
              "gnatprep_if_statement"
              "case_statement"
              "loop_statement"
              "block_statement"
              "extended_return_statement"
              "accept_statement"

              ;; select_statement
              "selective_accept"
              "timed_entry_call"
              "conditional_entry_call"
              "asynchronous_select"))))

(defun ada-ts-mode--declarative-item-p (node)
  "Determine if NODE is a declarative item."
  (or (ada-ts-mode--basic-declarative-item-p node)
      (let ((node-type (treesit-node-type node)))
        (member node-type
                '(;; proper_body
                  "subprogram_body"
                  "package_body"
                  "task_body"
                  "protected_body"

                  "body_stub")))))

(defun ada-ts-mode--basic-declarative-item-p (node)
  "Determine if NODE is a basic declarative item."
  (or (ada-ts-mode--basic-declaration-p node)
      (let ((node-type (treesit-node-type node)))
        (member node-type
                '(;; aspect_clause
                  "attribute_definition_clause"
                  "enumeration_representation_clause"
                  "record_representation_clause"
                  "at_clause"

                  "use_clause")))))

(defun ada-ts-mode--basic-declaration-p (node)
  "Determine if NODE is a basic declaration."
  (let ((node-type (treesit-node-type node)))
    (member node-type
            '(;; type_declaration
              "full_type_declaration"
              "incomplete_type_declaration"
              "private_type_declaration"
              "private_extension_declaration"

              "subtype_declaration"
              "object_declaration"
              "number_declaration"
              "subprogram_declaration"
              "expression_function_declaration"
              "null_procedure_declaration"
              "package_declaration"

              ;; renaming_declaration
              "object_renaming_declaration"
              "exception_renaming_declaration"
              "package_renaming_declaration"
              "subprogram_renaming_declaration"
              "generic_renaming_declaration"

              "exception_declaration"

              ;; generic_declaration
              "generic_subprogram_declaration"
              "generic_package_declaration"

              "generic_instantiation"))))

(provide 'ada-ts-indentation)

;;; ada-ts-indentation.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("lspclient/" . "ada-ts-mode-lspclient-"))
;; End:
