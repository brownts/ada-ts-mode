;;; ada-ts-mode-eglot-tests.el --- Tests specific to Eglot and LSP -*- lexical-binding: t; -*-

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
(require 'eglot)
(require 'ert)
(require 'ert-x)

(ert-deftest ada-ts-mode-test-eglot-als-other-file ()
  "Test ALS command 'als-other-file'.

Eglot must support 'window/showDocument' for this command to work
correctly.  Older versions of Eglot (e.g., the version shipped with
Emacs 29) did not support it."
  (skip-unless (and (executable-find "ada_language_server")
                    (cl-find-method 'eglot-handle-request nil '(t (eql window/showDocument)))))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (should (string-equal (buffer-file-name (window-buffer (selected-window)))
                          (buffer-file-name (current-buffer))))
    (with-language-server eglot
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

(ert-deftest ada-ts-mode-test-eglot-als-project-file ()
  "Test ALS command 'als-project-file'."
  (skip-unless (executable-find "ada_language_server"))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server eglot
      (should (string-equal
               (ada-ts-mode--lsp-project-file)
               (expand-file-name "hello_world.gpr"
                                 (project-root (project-current))))))))

(ert-deftest ada-ts-mode-test-eglot-config-exists ()
  "Tests that Eglot contains a server configuration for `ada-ts-mode'."
  (should (ada-ts-mode-lspclient-eglot--find-mode-config 'ada-ts-mode)))

(ert-deftest ada-ts-mode-test-eglot-config-language ()
  "Tests that Eglot correctly determines language for `ada-ts-mode'."
  (skip-unless (and (ada-ts-mode-lspclient-eglot--find-mode-config 'ada-ts-mode)
                    (executable-find "ada_language_server")))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server eglot
      (let ((language
             (cond ((functionp 'eglot--languageId)
                    (eglot--languageId (eglot-current-server)))
                   ((functionp 'eglot--language-id)
                    (eglot--language-id (eglot-current-server)))
                   (t (ert-fail "Unknown language query API")))))
        (should (stringp language))
        (should (string-equal language "ada"))))))

(ert-deftest ada-ts-mode-test-eglot-formatting ()
  "Test LSP request 'textDocument/formatting'."
  (skip-unless (executable-find "ada_language_server"))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server eglot
      (setq-local ada-ts-mode-indent-backend 'lsp)
      (setq-local indent-tabs-mode nil)
      (let ((inhibit-message t))
        (ada-ts-mode-tests--check-indentation)))))

(ert-deftest ada-ts-mode-test-eglot-range-formatting ()
  "Test LSP request 'textDocument/rangeFormatting'."
  (skip-unless (executable-find "ada_language_server"))
  (with-file-in-project
      "hello_world.adb"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server eglot
      (setq-local ada-ts-mode-indent-backend 'lsp)
      (setq-local indent-tabs-mode nil)
      (let ((inhibit-message t))
        (ada-ts-mode-tests--check-line-indentation)))))

(ert-deftest ada-ts-mode-test-eglot-electric-pair ()
  "Test Eglot in combination with `electric-pair-mode'."
  (skip-unless (executable-find "ada_language_server"))
  (with-file-in-project
      "example.adb"
      (ert-resource-file "electric-pair")
      "example.adb"
    (setq-local indent-tabs-mode nil)
    (setq-local ada-ts-mode-indent-backend 'lsp)
    (let ((buffer (buffer-string))
          (size (buffer-size)))
      (with-language-server eglot
        (message "Emacs               : %s" emacs-version)
        (message "ada-ts-mode         : %s" (lm-version (find-library-name "ada-ts-mode")))
        (message "Eglot               : %s" (lm-version (find-library-name "eglot")))
        (message "ada_language_server : %s" (car (process-lines (executable-find "ada_language_server") "--version")))
        (electric-pair-local-mode)
        (setq-local electric-pair-skip-self t)
        (goto-char 96)
        (ada-ts-mode-tests--simulate-key-press ")")
        ;; Should only move point, not insert the character.
        (should (string-equal buffer (buffer-string)))
        (should (= (point) 97))))))

(provide 'ada-ts-mode-eglot-tests)

;;; ada-ts-mode-eglot-tests.el ends here
