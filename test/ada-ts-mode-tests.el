;;; ada-ts-mode-tests.el --- Tests for Tree-sitter-based Ada mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Troy Brown

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

;;; Code:

(require 'ada-ts-mode)
(require 'ert)
(require 'ert-font-lock nil 'noerror) ; Emacs 30+
(require 'ert-x)
(require 'treesit)
(require 'which-func)

(defun default-transform ()
  "Default transform function for test."
  (ada-ts-mode)
  (should (not (treesit-search-subtree
                (treesit-buffer-root-node) "ERROR")))
  (setq-local indent-tabs-mode nil))

(defun defun-transform (name)
  "Defun NAME transform function for test."
  (default-transform)
  (should (string-equal (which-function) name)))

(defun filling-transform ()
  "Filling transform function for test."
  (default-transform)
  (fill-paragraph))

(defun imenu-transform (menu &optional setup)
  "IMenu MENU transform function for test.

Only the structure is checked, not the markers.  SETUP can be used to
perform custom initialization."
  (default-transform)
  (when setup
    (funcall setup))
  (cl-labels ((filter-menu (menu-item)
                (cond ((markerp menu-item) nil) ; remove marker
                      ((proper-list-p menu-item)
                       (mapcar #'filter-menu menu-item))
                      ((consp menu-item)
                       (cons (filter-menu (car menu-item))
                             (filter-menu (cdr menu-item))))
                      (t menu-item))))
    (let* ((actual-menu (filter-menu (funcall imenu-create-index-function))))
      (should (equal menu actual-menu)))))

(defun indent-transform (&optional setup)
  "Indent transform function for test.

SETUP can be used to perform custom initialization."
  (default-transform)
  (when setup
    (funcall setup))
  (goto-char (point-min))
  (cl-flet ((line-length () (- (line-end-position)
                               (line-beginning-position))))
    (while (not (eobp))
      (if (> (line-length) 0)
          (if (= (following-char) ?\s)
              (while (and (> (line-length) 0)
                          (= (following-char) ?\s))
                (delete-char 1))
            (insert-char ?\s)))
      (forward-line 1)
      (beginning-of-line)))
  (cl-letf (((symbol-function 'ada-ts-mode--anchor-catch-all)
             (lambda ()
               (lambda (node parent bol &rest _)
                 (let ((prefix "Indentation using catch-all rule: ")
                       (suffix (format "[NODE: %s, PARENT: %s, BOL: %s" node parent bol)))
                   (ert-fail (concat prefix suffix)))))))
    (indent-region (point-min) (point-max))))

(defun mode-transform (&optional version)
  "Mode transform function for test.

If VERSION is nil, the expected mode is \\='ada-ts-mode\\='.  If VERSION
is not nil, for an Emacs major version at or above VERSION, the expected
mode is \\='ada-ts-mode\\=', otherwise the expected mode is
\\='fundamental-mode\\='."
  (let ((inhibit-message t) ; Suppress 'Ignoring unknown mode ...'.
        (expected-mode
         (cond (version
                (if (>= emacs-major-version version)
                    'ada-ts-mode
                  'fundamental-mode))
               (t 'ada-ts-mode))))
    (set-auto-mode)
    (should (eq major-mode expected-mode))))

(defun newline-transform ()
  "Newline transform function for test."
  (default-transform)
  (call-interactively #'newline))


(dolist (file (directory-files (ert-resource-directory)
                               nil
                               directory-files-no-dot-files-regexp))
  (let* ((file-noext (file-name-sans-extension file))
         (file-path (ert-resource-file file))
         (transform (cond ((string-suffix-p "-nl"     file-noext) #'newline-transform)
                          ((string-prefix-p "filling" file-noext) #'filling-transform)
                          ((string-prefix-p "indent"  file-noext) #'indent-transform)
                          (t #'default-transform))))
    (if (string-prefix-p "font-lock" file-noext)
        (eval `(ert-deftest ,(intern (concat "ada-ts-mode-test-" file-noext)) ()
                 (skip-unless (featurep 'ert-font-lock))
                 (with-temp-buffer
                   (insert-file-contents ,file-path)
                   (funcall #',transform))
                 (let ((prev-level treesit-font-lock-level))
                   (unwind-protect
                       (progn
                         ;; Force full fontification
                         (setq treesit-font-lock-level 4)
                         (ert-font-lock-test-file ,file-path 'ada-ts-mode))
                     (setq treesit-font-lock-level prev-level)))))
      (eval `(ert-deftest ,(intern (concat "ada-ts-mode-test-" file-noext)) ()
               (ert-test-erts-file ,file-path #',transform))))))

(provide 'ada-ts-mode-tests)

;;; ada-ts-mode-tests.el ends here
