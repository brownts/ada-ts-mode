;;; ada-ts-mode-tests.el --- Tests for Tree-sitter-based Ada mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Troy Brown

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
  (cl-assert (not (treesit-search-subtree
                   (treesit-buffer-root-node) "ERROR")))
  (setq-local indent-tabs-mode nil))

(defun defun-transform (name)
  "Defun NAME transform function for test."
  (default-transform)
  (cl-assert (string-equal (which-function) name)))

(defun filling-transform ()
  "Filling transform function for test."
  (default-transform)
  (fill-paragraph))

(dolist (file
         (directory-files
          (expand-file-name "resources"
                            (file-name-directory
                             (cond (load-in-progress load-file-name)
                                   ((bound-and-true-p byte-compile-current-file)
                                    byte-compile-current-file)
                                   (t (buffer-file-name)))))
          nil
          (rx bos
              (or (not ".")
                  (seq "." (not "."))
                  (seq ".." (+ anychar)))
              )))
  (let* ((file-noext (file-name-sans-extension file))
         (file-path (ert-resource-file file))
         (transform (cond ((string-prefix-p "filling" file-noext) #'filling-transform)
                          (t #'default-transform))))
    (if (string-prefix-p "font-lock" file-noext)
        (eval `(ert-deftest ,(intern (concat "ada-ts-mode-test-" file-noext)) ()
                 (skip-unless (featurep 'ert-font-lock))
                 (with-temp-buffer
                   (insert-file-contents ,file-path)
                   (funcall #',transform))
                 (ert-font-lock-test-file ,file-path 'ada-ts-mode)))
      (eval `(ert-deftest ,(intern (concat "ada-ts-mode-test-" file-noext)) ()
               (ert-test-erts-file ,file-path #',transform))))))

(provide 'ada-ts-mode-tests)

;;; ada-ts-mode-tests.el ends here
