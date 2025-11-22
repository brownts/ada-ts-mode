;;; ada-ts-mode-test-utils.el --- Common utilities shared by tests -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'ert)

;;;; Utilities

(defun ada-ts-mode-tests--modify-and-reindent ()
  "Modify each line and reindent buffer."
  ;; Modifying each line is used to make sure there is an indentation
  ;; rule that moves the line back to the initial location, rather
  ;; than no indentation rule that just leaves the line alone.  This
  ;; is useful for finding lines with missing indentation rules.
  (goto-char (point-min))
  (cl-flet ((line-length () (- (line-end-position)
                               (line-beginning-position))))
    (while (not (eobp))
      (when (> (line-length) 0)
        (if (= (following-char) ?\s)
            (while (and (> (line-length) 0)
                        (= (following-char) ?\s))
              (delete-char 1))
          (insert-char ?\s)))
      (forward-line 1)
      (beginning-of-line)))
  (indent-region (point-min) (point-max)))

(defun ada-ts-mode-tests--modify-and-reindent-by-line ()
  "Modify each indented line and reindent."
  (goto-char (point-min))
  (cl-flet ((line-length () (- (line-end-position)
                               (line-beginning-position))))
    (while (not (eobp))
      (when (and (> (line-length) 0)
                 (= (following-char) ?\s))
        (while (and (> (line-length) 0)
                    (= (following-char) ?\s))
          (delete-char 1))
        (indent-according-to-mode))
      (forward-line 1)
      (beginning-of-line))))

(defun ada-ts-mode-tests--check-indentation ()
  "Check buffer indentation is as expected."
  (let ((buffer (buffer-string))
        (point (point)))
    (ada-ts-mode-tests--modify-and-reindent)
    (should (string-equal buffer (buffer-string)))
    (goto-char point)))

(defun ada-ts-mode-tests--check-line-indentation ()
  "Check line indentation is as expected."
  (let ((buffer (buffer-string))
        (point (point)))
    (ada-ts-mode-tests--modify-and-reindent-by-line)
    (should (string-equal buffer (buffer-string)))
    (goto-char point)))

(defun ada-ts-mode-tests--simulate-key-press (key)
  "Simulate interactively pressing KEY.

Simulates execution of the command associated with the key as well as
execution of pre and post command hooks."
  (let ((inhibit-message t))
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (execute-kbd-macro (kbd key)))))

(defmacro with-language-server (client &rest body)
  "Execute the forms in BODY while language server is connected via CLIENT."
  (declare (indent 1) (debug t))
  (cond
   ((eq client 'eglot)
    `(progn
       (should (not (ada-ts-mode-lspclient-current)))
       (let* ((inhibit-message t)
              (initialized nil)
              (connect-hook #'(lambda (&rest _) (setq initialized t))))
         ;; eglot-connect-hook hooks aren't guaranteed to be called
         ;; from the source buffer, so we need to register globally
         ;; instead of buffer locally, as the hook won't be called
         ;; otherwise.
         (add-hook 'eglot-connect-hook connect-hook)
         (unwind-protect
             (progn
               (call-interactively #'eglot)
               (with-timeout (5)
                 (while (eq initialized nil)
                   (sleep-for 0.01)))
               (should initialized))
           (remove-hook 'eglot-connect-hook connect-hook)))
       (unwind-protect
           (let ((client (ada-ts-mode-lspclient-current))
                 (jsonrpc-default-request-timeout 30))
             (should (eq client 'eglot))
             ,@body)
         (let ((inhibit-message t)
               (timeout 30))
           (eglot-shutdown (eglot-current-server) nil timeout)))))
   ((eq client 'lsp-mode)
    `(progn
       (should (not (ada-ts-mode-lspclient-current)))
       (let ((lsp-auto-guess-root t)
             (lsp-enable-indentation nil)
             (lsp-keep-workspace-alive nil))
         (let ((inhibit-message t)
               (initialized nil))
           (lsp-workspace-remove-all-folders)
           (add-hook 'lsp-after-initialize-hook
                     (lambda () (setq initialized t)) nil 'local)
           (lsp)
           (with-timeout (5)
             (while (eq initialized nil)
               (sleep-for 0.01)))
           (should initialized))
         (unwind-protect
             (let ((client (ada-ts-mode-lspclient-current)))
               (should (eq client 'lsp-mode))
               ,@body)
           (let ((inhibit-message t))
             (lsp--global-teardown))))))
   (t (error "Unknown LSP client"))))

(defmacro with-file-in-project (file root extra-root-markers &rest body)
  "Open FILE in project at ROOT and execute forms in BODY.

EXTRA-ROOT-MARKERS are used to anchor the project."
  (declare (indent 3) (debug t))
  `(let* ((filename (expand-file-name ,file ,root))
          (project-vc-extra-root-markers (ensure-list ,extra-root-markers)))
     (should (not (get-file-buffer filename)))
     (let ((buffer (find-file filename)))
       (unwind-protect
           (progn
             (should (eq major-mode 'ada-ts-mode))
             (should (string-equal
                      (expand-file-name (directory-file-name (project-root (project-current))))
                      ,root))
             ,@body)
         (kill-buffer buffer)))))

(provide 'ada-ts-mode-test-utils)

;;; ada-ts-mode-test-utils.el ends here
