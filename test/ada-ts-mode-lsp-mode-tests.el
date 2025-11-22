;;; ada-ts-mode-lsp-mode-tests.el --- Tests specific to `lsp-mode' and LSP -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ada-ts-mode)
(require 'ada-ts-mode-test-utils)
(require 'lsp-mode nil 'noerror)
(require 'ert)
(require 'ert-x)

(ert-deftest ada-ts-mode-test-lsp-mode-als-other-file ()
  "Test ALS command 'als-other-file'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (should (string-equal (buffer-file-name (window-buffer (selected-window)))
                          (buffer-file-name (current-buffer))))
    (with-language-server lsp-mode
      (ada-ts-mode-lspclient-command-execute
       client "als-other-file"
       (ada-ts-mode-lspclient-document-id client))
      ;; Wait for window/showDocument
      (with-timeout (5)
        (while (string-equal (buffer-file-name (window-buffer (selected-window)))
                             (buffer-file-name (current-buffer)))
          (sleep-for 0.01)))
      (let* ((buffer (window-buffer (selected-window)))
             (buffer-name (buffer-file-name buffer))
             (filename-adb (buffer-file-name (current-buffer)))
             (filename-ads (concat (file-name-sans-extension filename-adb) ".ads")))
        (should (string-equal buffer-name filename-ads))
        (kill-buffer buffer)))))

(ert-deftest ada-ts-mode-test-lsp-mode-als-project-file ()
  "Test ALS command 'als-project-file'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (should (string-equal
               (ada-ts-mode--lsp-project-file)
               (expand-file-name "hello_world.gpr"
                                 (project-root (project-current))))))))

(ert-deftest ada-ts-mode-test-lsp-mode-formatting ()
  "Test LSP request 'textDocument/formatting'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (setq-local ada-ts-mode-indent-backend 'lsp)
      (setq-local indent-tabs-mode nil)
      (let ((inhibit-message t))
        (ada-ts-mode-tests--check-indentation)))))

(ert-deftest ada-ts-mode-test-lsp-mode-range-formatting ()
  "Test LSP request 'textDocument/rangeFormatting'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (setq-local ada-ts-mode-indent-backend 'lsp)
      (setq-local indent-tabs-mode nil)
      (let ((inhibit-message t))
        (ada-ts-mode-tests--check-line-indentation)))))

(provide 'ada-ts-mode-lsp-mode-tests)

;;; ada-ts-mode-lsp-mode-tests.el ends here
