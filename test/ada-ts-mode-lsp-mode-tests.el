;;; ada-ts-mode-lsp-mode-tests.el --- Tests specific to `lsp-mode' and LSP -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Troy Brown

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

(ert-deftest ada-ts-mode-test-lsp-mode-als-executables ()
  "Test ALS command 'als-executables'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (let ((execs (ada-ts-als-executables)))
        (should (listp execs))
        (should (= (length execs) 1))
        (should (string-equal
                 (car execs)
                 (expand-file-name (concat "hello_world"
                                           (if (eq system-type 'windows-nt) ".exe" ""))
                                   (project-root (project-current)))))))))

(ert-deftest ada-ts-mode-test-lsp-mode-als-get-project-attribute-value ()
  "Test ALS command 'als-get-project-attribute-value'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (let ((dirs (ada-ts-als-get-project-attribute-value "Source_Dirs")))
        (should (listp dirs))
        (should (= (length dirs) 1))
        (should (string-equal (car dirs) "."))))))

(ert-deftest ada-ts-mode-test-lsp-mode-als-mains ()
  "Test ALS command 'als-mains'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (let ((mains (ada-ts-als-mains)))
        (should (listp mains))
        (should (= (length mains) 1))
        (should (string-equal
                 (car mains)
                 (expand-file-name "hello_world.adb"
                                   (project-root (project-current)))))))))

(ert-deftest ada-ts-mode-test-lsp-mode-als-object-dir ()
  "Test ALS command 'als-object-dir'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (let ((object-dir (ada-ts-als-object-dir)))
        (should (stringp object-dir))
        (should (string-equal
                 object-dir
                 (directory-file-name
                  (expand-file-name (project-root (project-current))))))))))

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
      (ada-ts-lspclient-command-execute
       client "als-other-file"
       (ada-ts-lspclient-document-id client))
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
               (ada-ts-als-project-file)
               (expand-file-name "hello_world.gpr"
                                 (project-root (project-current))))))))

(ert-deftest ada-ts-mode-test-lsp-mode-als-source-dirs ()
  "Test ALS command 'als-source-dirs'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (let* ((source-dirs (ada-ts-als-source-dirs))
             (source-dir (car source-dirs)))
        (should (= (length source-dirs) 1))
        (should (stringp source-dir))
        (should (string-equal
                 source-dir
                 (directory-file-name
                  (expand-file-name (project-root (project-current))))))))))

(ert-deftest ada-ts-mode-test-lsp-mode-config-exists ()
  "Tests that `lsp-mode' contains a server configuration for `ada-ts-mode'."
  (skip-unless (featurep 'lsp-mode))
  (require 'lsp-ada)
  (let ((client (gethash 'ada-ls lsp-clients)))
    (should (lsp--client-p client))
    (should (memq 'ada-ts-mode (lsp--client-major-modes client)))))

(ert-deftest ada-ts-mode-test-lsp-mode-config-language ()
  "Tests that `lsp-mode' correctly determines language for `ada-ts-mode'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.adb"
    (with-language-server lsp-mode
      (let ((language (lsp-buffer-language)))
        (should (stringp language))
        (should (string-equal language "ada"))))))

(ert-deftest ada-ts-mode-test-lsp-mode-setup ()
  "Tests that `lsp-mode' is setup correctly for `ada-ts-mode'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (pcase-dolist (`(,name . ,value)
                     '((lsp-enable-imenu . nil)
                       (lsp-enable-indentation . nil)
                       (lsp-enable-on-type-formatting . nil)
                       (lsp-semantic-tokens-enable . t)))
        (should (local-variable-p name))
        (should (equal (symbol-value name) value))))))

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
      (ada-ts-mode-tests--check-indentation))))

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
      (ada-ts-mode-tests--check-line-indentation))))

(provide 'ada-ts-mode-lsp-mode-tests)

;;; ada-ts-mode-lsp-mode-tests.el ends here
