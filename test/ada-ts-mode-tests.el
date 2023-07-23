;;; ada-ts-mode-tests.el --- Tests for Tree-sitter-based Ada mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Troy Brown

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

(require 'ert)
(require 'ert-x)

(ert-deftest ada-ts-mode-test-filling ()
  (ert-test-erts-file (ert-resource-file "filling.erts")))

(ert-deftest ada-ts-mode-test-navigation ()
  (ert-test-erts-file (ert-resource-file "navigation.erts")))

(provide 'ada-ts-mode-tests)

;;; ada-ts-mode-tests.el ends here
