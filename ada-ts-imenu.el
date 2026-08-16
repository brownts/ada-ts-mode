;;; ada-ts-imenu.el -- IMenu support in ADa files -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Troy Brown

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
(require 'treesit)

(ada-ts-mode--declare-treesit-functions)

;;;; Customization

(defcustom ada-ts-mode-imenu-categories
  '(package
    subprogram
    protected
    task
    type-declaration
    with-clause)
  "Configuration of Imenu categories."
  :type '(repeat :tag "Categories"
                 (choice :tag "Category"
                         (const :tag "Package" package)
                         (const :tag "Subprogram" subprogram)
                         (const :tag "Protected" protected)
                         (const :tag "Task" task)
                         (const :tag "Type Declaration" type-declaration)
                         (const :tag "With Clause" with-clause)))
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.6.0"))

(defcustom ada-ts-mode-imenu-category-name-alist
  '((package          . "Package")
    (subprogram       . "Subprogram")
    (protected        . "Protected")
    (task             . "Task")
    (type-declaration . "Type Declaration")
    (with-clause      . "With Clause"))
  "Configuration of Imenu category names."
  :type '(alist :key-type symbol :value-type string)
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.6.0"))

(defcustom ada-ts-mode-imenu-nesting-strategy-function
  #'ada-ts-mode-imenu-nesting-strategy-before
  "Configuration for Imenu nesting strategy function."
  :type `(choice (const :tag "Place Before Nested Entries"
                        ,#'ada-ts-mode-imenu-nesting-strategy-before)
                 (const :tag "Place Within Nested Entries"
                        ,#'ada-ts-mode-imenu-nesting-strategy-within)
                 (function :tag "Custom function"))
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.5.8"))

(defcustom ada-ts-mode-imenu-nesting-strategy-placeholder "<<parent>>"
  "Placeholder for an item used in some Imenu nesting strategies."
  :type 'string
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.5.8"))

(defcustom ada-ts-mode-imenu-sort-function #'identity
  "Configuration for Imenu sorting function."
  :type `(choice (const :tag "In Buffer Order" ,#'identity)
                 (const :tag "Alphabetically" ,#'ada-ts-mode-imenu-sort-alphabetically)
                 (function :tag "Custom function"))
  :group 'ada-ts
  :link '(custom-manual :tag "Imenu" "(ada-ts-mode)Imenu")
  :package-version '(ada-ts-mode . "0.5.8"))

;;;; Strategies

(defun ada-ts-mode-imenu-nesting-strategy-before (item-name marker subtrees)
  "Nesting strategy which places item before the list of nested entries.

An entry is added for ITEM-NAME at item's MARKER location and another
entry is added after it for ITEM-NAME containing SUBTREES."
  (list (cons item-name marker)
        (cons item-name subtrees)))

(defun ada-ts-mode-imenu-nesting-strategy-within (item-name marker subtrees)
  "Nesting strategy which places item within list of nested entries.

An entry is added for ITEM-NAME containing SUBTREES where SUBTREES is
modified to include a `ada-ts-mode-imenu-nesting-strategy-placeholder'
first item at item's MARKER location."
  (let* ((empty-entry
          (cons ada-ts-mode-imenu-nesting-strategy-placeholder marker))
         (new-subtrees
          (cons empty-entry subtrees)))
    (list (cons item-name new-subtrees))))

(defun ada-ts-mode-imenu-sort-alphabetically (items)
  "Alphabetical sort of Imenu ITEMS."
  (sort items
        (lambda (x y)
          (let ((x-name (downcase (car x)))
                (y-name (downcase (car y)))
                (placeholder (downcase
                              ada-ts-mode-imenu-nesting-strategy-placeholder)))
            ;; Always put placeholder first, even if not alphabetical.
            (or (string= x-name placeholder)
                (and (not (string= y-name placeholder))
                     (string< (car x) (car y))))))))

;;;; Indexing

(defun ada-ts-imenu--index (tree item-p branch-p item-name-fn branch-name-fn)
  "Return Imenu index for a specific item category given TREE.

ITEM-P is a predicate for testing the item category's node.
ITEM-NAME-FN determines the name of the item given the item's node.
BRANCH-P is a predicate for determining if a node is a branch.  This is
used to identify higher level nesting structures (i.e., packages,
subprograms, etc.) which encompass the item.  BRANCH-NAME-FN determines
the name of the branch given the branch node."
  (let* ((node (car tree))
         (subtrees
          (funcall ada-ts-mode-imenu-sort-function
                   (mapcan (lambda (tree)
                             (ada-ts-imenu--index tree
                                                  item-p
                                                  branch-p
                                                  item-name-fn
                                                  branch-name-fn))
                           (cdr tree))))
         (marker (set-marker (make-marker)
                             (treesit-node-start node)))
         (item (funcall item-p node))
         (item-name (when item (funcall item-name-fn node)))
         (branch (funcall branch-p node))
         (branch-name (when branch (funcall branch-name-fn node))))
    (cond ((and item (not subtrees))
           (list (cons item-name marker)))
          ((and item subtrees)
           (funcall ada-ts-mode-imenu-nesting-strategy-function
                    item-name marker subtrees))
          ((and branch subtrees)
           (list (cons branch-name subtrees)))
          (t subtrees))))

(defun ada-ts-imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root (treesit-buffer-root-node))
         (defun-tree
          (and (seq-intersection '(package subprogram protected task)
                                 ada-ts-mode-imenu-categories)
               (treesit-induce-sparse-tree root #'ada-ts-mode--defun-p)))
         (index-package
          (and (memq 'package ada-ts-mode-imenu-categories)
               (ada-ts-imenu--index defun-tree
                                    #'ada-ts-mode--package-p
                                    #'ada-ts-mode--defun-p
                                    #'ada-ts-mode--defun-name
                                    #'ada-ts-mode--defun-name)))
         (index-subprogram
          (and (memq 'subprogram ada-ts-mode-imenu-categories)
               (ada-ts-imenu--index defun-tree
                                    #'ada-ts-mode--subprogram-p
                                    #'ada-ts-mode--defun-p
                                    #'ada-ts-mode--defun-name
                                    #'ada-ts-mode--defun-name)))
         (index-protected
          (and (memq 'protected ada-ts-mode-imenu-categories)
               (ada-ts-imenu--index defun-tree
                                    #'ada-ts-mode--protected-p
                                    #'ada-ts-mode--defun-p
                                    #'ada-ts-mode--defun-name
                                    #'ada-ts-mode--defun-name)))
         (index-task
          (and (memq 'task ada-ts-mode-imenu-categories)
               (ada-ts-imenu--index defun-tree
                                    #'ada-ts-mode--task-p
                                    #'ada-ts-mode--defun-p
                                    #'ada-ts-mode--defun-name
                                    #'ada-ts-mode--defun-name)))
         (index-type-declaration
          (and (memq 'type-declaration ada-ts-mode-imenu-categories)
               (ada-ts-imenu--index
                (treesit-induce-sparse-tree
                 root
                 (lambda (node)
                   (or (ada-ts-mode--defun-p node)
                       (ada-ts-mode--type-declaration-p node))))
                #'ada-ts-mode--type-declaration-p
                #'ada-ts-mode--defun-p
                #'ada-ts-mode--type-declaration-name
                #'ada-ts-mode--defun-name)))
         (index-with-clause
          (and (memq 'with-clause ada-ts-mode-imenu-categories)
               (ada-ts-imenu--index
                (treesit-induce-sparse-tree
                 root
                 #'ada-ts-mode--with-clause-name-p
                 nil
                 3) ; Limit search depth for speed
                #'identity
                #'ignore
                #'ada-ts-mode--node-to-name
                #'ignore)))
         (imenu-alist
          ;; Respect category ordering in `ada-ts-mode-imenu-categories'
          (mapcar (lambda (category)
                    (let ((name (alist-get category
                                           ada-ts-mode-imenu-category-name-alist))
                          (index (pcase category
                                   ('package          index-package)
                                   ('subprogram       index-subprogram)
                                   ('protected        index-protected)
                                   ('task             index-task)
                                   ('type-declaration index-type-declaration)
                                   ('with-clause      index-with-clause)
                                   (_ (error "Unknown cateogry: %s" category)))))
                      (cons name index)))
                  ada-ts-mode-imenu-categories)))

    ;; Remove empty categories
    (seq-filter (lambda (i) (cdr i)) imenu-alist)))

(provide 'ada-ts-imenu)

;;; ada-ts-imenu.el ends here
