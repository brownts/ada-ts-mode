;;; ada-ts-casing.el --- Casing support in Ada files  -*- lexical-binding: t; -*-

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

(require 'cl-generic)
(require 'rx)
(require 'treesit)

(declare-function treesit-node-end   "treesit.c" (node))
(declare-function treesit-node-eq    "treesit.c" (node1 node2))
(declare-function treesit-node-start "treesit.c" (node))
(declare-function treesit-node-type  "treesit.c" (node))

(defcustom ada-ts-mode-case-formatting
  '((identifier :formatter upcase-initials
                :dictionary ("ASCII" "GNAT" "IO"))
    (keyword    :formatter downcase))
  "Case formatting rules for casing commands and modes.

Each rule should be of the form (CATEGORY . PROPS), where CATEGORY is
the category to which the formatting should be applied.  PROPS should
have the form:

   [KEYWORD VALUE]...

The following keywords are meaningful:

:formatter

   VALUE must be a function which takes a string and returns the
   formatted string.  This is a required property.

:dictionary

   Dictionary entries take precedence over the formatting function.
   This is an optional property.

   VALUE may be a list of strings whose exact casing is applied to
   candidate words and subwords.

   VALUE may also be a property list, having the form:

      [KEYWORD VALUE]...

   The following keywords are meaningful:

   :words

      VALUE must be a list of strings whose exact casing is applied to
      candidate words and subwords.  This is an optional property.

   :files

      VALUE must be a list of files where the content of each file
      contains a word or subword per line whose exact casing is applied
      to candidate words and subwords.  This is an optional property."
  :type '(alist
          :key-type (symbol :tag "Category")
          :value-type
          (plist
           :tag "Properties"
           :key-type symbol
           :options
           ((:formatter
             (choice
              :tag "Function"
              (function-item :tag "Mixed-Case (strict)" capitalize)
              (function-item :tag "Mixed-Case (loose)"  upcase-initials)
              (function-item :tag "Upper-Case"          upcase)
              (function-item :tag "Lower-Case"          downcase)
              (function      :tag "Custom")))
            (:dictionary
             (choice
              :tag "Dictionary"
              (repeat :tag "Words" (string :tag "Word"))
              (plist
               :tag "Words/Files"
               :key-type symbol
               :options
               ((:words (repeat :tag "Words" (string :tag "Word")))
                (:files (repeat :tag "Files" (file :tag "File"))))))))))
  :group 'ada-ts
  :link '(custom-manual :tag "Casing" "(ada-ts-mode)Casing")
  :package-version '(ada-ts-mode . "0.8.0"))

;;;###autoload
(put 'ada-ts-mode-case-formatting
     'safe-local-variable
     (lambda (rules)
       (while (and (consp rules)
                   (consp (car rules))
                   (not (unsafep (list (plist-get (cdar rules) :formatter)))))
         (setq rules (cdr rules)))
       (null rules)))

(defvar ada-ts-mode--case-dictionary-file-alist nil)
(defvar ada-ts-mode--case-formatting nil)

(defun ada-ts-mode--case-dictionary-load (file)
  "Load dictionary FILE."
  (let (file-words)
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
        (if (looking-at (rx bol (* whitespace) eol) 'inhibit-modify)
            (forward-line 1) ; skip empty lines
          (skip-chars-forward " \t")
          (when-let* ((line-words
                       (string-split
                        (buffer-substring-no-properties (pos-bol) (pos-eol))
                        "*" 'omit-nulls (rx (+ whitespace)))))
            (dolist (line-word line-words)
              (unless (assoc-string line-word file-words t)
                (push line-word file-words))))
          (forward-line 1))))
    (setq ada-ts-mode--case-dictionary-file-alist
          (assoc-delete-all file ada-ts-mode--case-dictionary-file-alist))
    (push (cons file `( :words ,(reverse file-words)
                        :modification-time ,(file-attribute-modification-time
                                             (file-attributes file))))
          ada-ts-mode--case-dictionary-file-alist)))

(defun ada-ts-mode--case-settings-process (symbol newval operation where)
  "Load/Reload dictionary files as needed and compute internal word list.

SYMBOL is expected to be `ada-ts-mode-case-formatting', and OPERATION is
queried to check that it is a `set' operation (as defined by
`add-variable-watcher'), otherwise nothing is updated.  Either compute
the default or buffer-local value for `ada-ts-mode--case-formatting'
based on NEWVAL for SYMBOL and any loaded/reloaded dictionaries."
  (when (and (eq symbol 'ada-ts-mode-case-formatting)
             (eq operation 'set))
    (let (rules)
      (dolist (rule newval)
        (let (words)
          (when-let* ((dictionary (plist-get (cdr rule) :dictionary)))
            (if-let* ((files (plist-get dictionary :files)))
                (dolist (file files)
                  (let* ((file-path (substitute-in-file-name file)))
                    (if (file-name-absolute-p file-path)
                        (setq file-path (expand-file-name file-path))
                      (if-let* ((dir (locate-dominating-file (buffer-file-name) file-path)))
                          (setq file-path (expand-file-name file-path dir))
                        (setq file-path (expand-file-name file-path))))
                    (if (or (not (stringp file-path))
                            (not (file-readable-p file-path)))
                        (message "Cannot read %s, skipping dictionary file." file)
                      (let* ((dictionary-info
                              (cdr (assoc-string
                                    file-path
                                    ada-ts-mode--case-dictionary-file-alist))))
                        (when (or (not dictionary-info)
                                  (not (equal
                                        (plist-get dictionary-info :modification-time)
                                        (file-attribute-modification-time
                                         (file-attributes file-path)))))
                          (ada-ts-mode--case-dictionary-load file-path))))
                    (setq words
                          (append words
                                  (plist-get
                                   (cdr (assoc-string
                                         file-path
                                         ada-ts-mode--case-dictionary-file-alist))
                                   :words)
                                  (plist-get dictionary :words)))))
              (setq words (or (plist-get dictionary :words) dictionary))))
          (let ((new-rule (list (car rule)
                                :formatter (plist-get (cdr rule) :formatter))))
            (when words
              (setq new-rule
                    (append new-rule (list :dictionary words))))
            (push new-rule rules))))
      (setq rules (reverse rules))
      (if where
          (with-current-buffer where
            (setq-local ada-ts-mode--case-formatting rules))
        (setq-default ada-ts-mode--case-formatting rules)))))

(ada-ts-mode--case-settings-process
 'ada-ts-mode-case-formatting
 (default-value 'ada-ts-mode-case-formatting)
 'set nil)

(add-variable-watcher
 'ada-ts-mode-case-formatting
 #'ada-ts-mode--case-settings-process)

(defun ada-ts-mode--case-format-word (beg end formatter &optional dictionary)
  "Apply case formatting to word bounded by BEG and END using FORMATTER.

  When words or subwords are found in the DICTIONARY, the formatting in
  the DICTIONARY takes precedence over the FORMATTER."
  (let* ((point (point))
         (word (buffer-substring-no-properties beg end))
         (replacement
          (seq-find
           (lambda (item)
             (string-equal-ignore-case word item))
           dictionary)))
    ;; Don't modify the buffer unless necessary.  This allows running
    ;; formatting on an unmodified buffer and if there were no
    ;; formatting changes, the buffer won't show as modified.
    (if replacement
        (when-let* (((not (string-equal replacement word)))
                    (end-marker (set-marker (make-marker) end)))
          ;; apply word replacement
          (goto-char beg)
          (insert replacement)
          (delete-region (point) end-marker))
      (setq replacement (funcall formatter word))
      (when-let* (((not (string-equal replacement word)))
                  (end-marker (set-marker (make-marker) end)))
        ;; apply formatting change
        (goto-char beg)
        (insert replacement)
        (delete-region (point) end-marker))
      (when dictionary
        (setq word (buffer-substring-no-properties beg end))
        (let (subwords)
          (dolist (subword (split-string word "_"))
            (setq subword
                  (or
                   (seq-find
                    (lambda (item)
                      (string-equal-ignore-case subword item))
                    dictionary)
                   subword))
            (push subword subwords))
          (setq subwords (nreverse subwords))
          (setq replacement (string-join subwords "_"))
          (when-let* (((not (string-equal replacement word)))
                      (end-marker (set-marker (make-marker) end)))
            ;; apply subword replacements
            (goto-char beg)
            (insert replacement)
            (delete-region (point) end-marker)))))
    ;; Since we may be changing the content around point, we just
    ;; restore it when we're done.  Since the sum total of the
    ;; characters in the buffer hasn't changed (only the casing), the
    ;; saved position of point is still valid.
    (goto-char point)))

;;; Case Commands

(defun ada-ts-mode-case-format-region (beg end)
  "Apply case formatting to region bounded by BEG and END."
  (interactive "r" ada-ts-mode)
  (when-let* ((point
               (save-excursion
                 (goto-char beg)
                 (skip-chars-forward " \t\n" end)
                 (point)))
              (node (treesit-node-at point))
              (node-start (treesit-node-start node))
              (node-end (treesit-node-end node)))
    (while (and node (< node-start end))
      (when-let* ((entry
                   (seq-find
                    (lambda (entry)
                      (ada-ts-mode-case-category-p (car entry) node))
                    ada-ts-mode--case-formatting)))
        (ada-ts-mode--case-format-word
         node-start
         node-end
         (plist-get (cdr entry) :formatter)
         (plist-get (cdr entry) :dictionary)))
      (setq point
            (save-excursion
              (goto-char node-end)
              (skip-chars-forward " \t\n" end)
              (point)))
      (setq node (treesit-node-at point))
      (when node
        (let ((new-start (treesit-node-start node)))
          (if (> new-start node-start)
              (progn
                (setq node-start new-start)
                (setq node-end (treesit-node-end node)))
            (setq node nil)))))))

(defun ada-ts-mode-case-format-buffer ()
  "Apply case formatting to entire buffer."
  (interactive nil ada-ts-mode)
  (ada-ts-mode-case-format-region (point-min) (point-max)))

(defun ada-ts-mode-case-format-at-point ()
  "Apply case formatting at point."
  (interactive nil ada-ts-mode)
  (ada-ts-mode-case-format-region (point) (min (1+ (point)) (point-max))))

(defun ada-ts-mode-case-format-dwim ()
  "Apply case formatting intelligently."
  (interactive nil ada-ts-mode)
  (if (region-active-p)
      (ada-ts-mode-case-format-region (region-beginning) (region-end))
    (ada-ts-mode-case-format-at-point)))

;;; Case Category Predicates

(cl-defgeneric ada-ts-mode-case-category-p
    (category _node &optional _last-input _pos)
  "Return non-nil if NODE is a member of CATEGORY.

  LAST-INPUT is the auto-case triggering character, not yet inserted in
  the buffer.  POS represents the buffer location where LAST-INPUT will be
  inserted."
  (error "Unknown case category: %s" category))

(defvar ada-ts-mode--casing-keyword-keywords-regex nil)
(defvar ada-ts-mode--casing-identifier-keywords-regex
  (rx bos (or "all") eos))

(with-eval-after-load 'ada-ts-mode
  (defvar ada-ts-mode--keywords nil)

  (setq ada-ts-mode--casing-keyword-keywords-regex
        (rx-to-string `(: bos (or ,@ada-ts-mode--keywords) eos))))

(defun ada-ts-mode--casing-prev-node (node)
  "Find previous non-comment NODE, or nil if there isn't one."
  (let* ((prev node)
         (prev-type (treesit-node-type prev)))
    (while (and prev
                (or (treesit-node-eq prev node)
                    (string-equal prev-type "comment")))
      (save-excursion
        (goto-char (treesit-node-start prev))
        (skip-chars-backward " \t\n")
        (if (bobp)
            (setq prev nil)
          (setq prev (treesit-node-at (1- (point)))
                prev-type (treesit-node-type prev)))))
    prev))

(cl-defmethod ada-ts-mode-case-category-p
  ((_category (eql 'identifier)) node &optional last-input pos)
  "Return non-nil if NODE is a member of the \\='identifier\\=' CATEGORY.

  LAST-INPUT is the auto-case triggering character, not yet inserted in
  the buffer.  POS represents the buffer location where LAST-INPUT will be
  inserted."
  (when-let* ((type (treesit-node-type node)))
    (if (null last-input)
        (or (and (string-match-p ada-ts-mode--casing-keyword-keywords-regex type)
                 ;; An attribute name (e.g., "'Access").
                 (when-let* ((prev (ada-ts-mode--casing-prev-node node)))
                   (string-equal (treesit-node-type prev) "tick")))
            (and (string-equal type "identifier")
                 (not (string-match-p
                       ada-ts-mode--casing-identifier-keywords-regex
                       (downcase (treesit-node-text node 'no-property))))))
      (let ((text (downcase
                   (buffer-substring-no-properties
                    (treesit-node-start node)
                    (min pos (treesit-node-end node))))))
        (or
         ;; Identifier staying an identifier
         (and (string-equal type "identifier")
              (or (eq last-input ?_)
                  (and (not (string-match-p
                             ada-ts-mode--casing-identifier-keywords-regex
                             text))
                       (or (eq last-input ?')
                           ;; Check if by inserting the separator, we
                           ;; will be creating a keyword.
                           (not (string-match-p
                                 ada-ts-mode--casing-keyword-keywords-regex
                                 text))
                           ;; Looks like a keyword, but check if it's
                           ;; actually an attribute name with the same
                           ;; name as a keyword (e.g., "Access").
                           (when-let* ((prev (ada-ts-mode--casing-prev-node node)))
                             (string-equal (treesit-node-type prev) "tick"))))))
         ;; Keyword becoming an identifier
         (and (string-match-p ada-ts-mode--casing-keyword-keywords-regex type)
              (or (eq last-input ?_)
                  (eq last-input ?')
                  ;; Looks like a keyword, but check if it's actually
                  ;; an attribute name with the same name as a keyword
                  ;; (e.g., "Access").
                  (when-let* ((prev (ada-ts-mode--casing-prev-node node)))
                    (string-equal (treesit-node-type prev) "tick")))))))))

(cl-defmethod ada-ts-mode-case-category-p
  ((_category (eql 'keyword)) node &optional last-input pos)
  "Return non-nil if NODE is a member of the \\='keyword\\=' CATEGORY.

  LAST-INPUT is the auto-case triggering character, not yet inserted in
  the buffer.  POS represents the buffer location where LAST-INPUT will be
  inserted."
  (when-let* ((type (treesit-node-type node)))
    (if (null last-input)
        (or (and (string-match-p ada-ts-mode--casing-keyword-keywords-regex type)
                 ;; Not an attribute name (e.g., "'Access").
                 (not
                  (when-let* ((prev (ada-ts-mode--casing-prev-node node)))
                    (string-equal (treesit-node-type prev) "tick"))))
            (and (string-equal type "identifier")
                 (string-match-p
                  ada-ts-mode--casing-identifier-keywords-regex
                  (downcase (treesit-node-text node 'no-property)))))
      (let ((text (downcase
                   (buffer-substring-no-properties
                    (treesit-node-start node)
                    (min pos (treesit-node-end node))))))
        (or
         ;; Keyword staying a keyword
         (and (string-match-p ada-ts-mode--casing-keyword-keywords-regex type)
              ;; Not an attribute name (e.g., "'Access").
              (not
               (when-let* ((prev (ada-ts-mode--casing-prev-node node)))
                 (string-equal (treesit-node-type prev) "tick")))
              (not (eq last-input ?_))
              (not (eq last-input ?')))
         ;; Identifier-Keyword staying a keyword
         (and (string-equal type "identifier")
              (not (eq last-input ?_))
              (string-match-p ada-ts-mode--casing-identifier-keywords-regex text))
         ;; Identifier becoming a keyword
         (and (string-equal type "identifier")
              (not (eq last-input ?_))
              (not (eq last-input ?'))
              ;; Check if by inserting the separator, a keyword will
              ;; be created.
              (string-match-p
               ada-ts-mode--casing-keyword-keywords-regex
               text)
              ;; Looks like a keyword, but check if it's actually an
              ;; attribute name with the same name as a keyword (e.g.,
              ;; "Access").
              (not
               (when-let* ((prev (ada-ts-mode--casing-prev-node node)))
                 (string-equal (treesit-node-type prev) "tick")))))))))

;;; Auto-Case Minor Mode

(defun ada-ts-mode--case-format-word-try (_)
  "Attempt to apply case formatting to word before point."
  (prog1
      nil ; return nil so overlaid keybinding triggers
    (when-let* ((last-input last-input-event)
                ;; Prevent key lookups from outside the buffer from
                ;; triggering case formatting
                ((derived-mode-p 'ada-ts-mode))
                ((not (bobp)))
                (prev-point (1- (point)))
                (node (treesit-node-at prev-point))
                ;; Ensure not in whitespace
                ((and (<= (treesit-node-start node) prev-point)
                      (< prev-point (treesit-node-end node))))
                (entry
                 (seq-find
                  (lambda (entry)
                    (ada-ts-mode-case-category-p (car entry) node last-input (point)))
                  ada-ts-mode--case-formatting)))
      ;; Point might be in the middle of a word and therefore about to
      ;; separate it into two words by the yet-to-be-inserted
      ;; key-press.  Only apply formatting before point.  The category
      ;; predicate already took this into consideration when
      ;; determining the category.
      (ada-ts-mode--case-format-word
       (treesit-node-start node)
       (min (point) (treesit-node-end node))
       (plist-get (cdr entry) :formatter)
       (plist-get (cdr entry) :dictionary)))))

(defvar ada-ts-auto-case-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '("RET" "SPC"
                   "_" "%" "&" "*" "(" ")"
                   "-" "=" "+" "|" ";" ":"
                   "'" "\"" "<" "," "." ">" "/"))
      (define-key map (kbd key)
                  `(menu-item "" ignore
                              :filter ada-ts-mode--case-format-word-try)))
    map))

(define-minor-mode ada-ts-auto-case-mode
  "Minor mode for auto-casing in Ada buffers."
  :group 'ada-ts
  :lighter " Ada/c"
  :interactive (ada-ts-mode))

(provide 'ada-ts-casing)

;;; ada-ts-casing.el ends here
