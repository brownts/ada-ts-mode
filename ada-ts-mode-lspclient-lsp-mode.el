;;; ada-ts-mode-lspclient-lsp-mode.el -- LSP client interface for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Troy Brown

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
(eval-when-compile
  ;; Needed for cl-labels
  (require 'cl-lib))

(declare-function lsp-can-execute-command?      "ext:lsp-mode" (command-name))
(declare-function lsp-configuration-section     "ext:lsp-mode" (section))
(declare-function lsp-format-region             "ext:lsp-mode" (s e))
(declare-function lsp-text-document-identifier  "ext:lsp-mode" ())
(declare-function lsp-workspace-command-execute "ext:lsp-mode" (command &optional args))
(declare-function lsp-workspace-root            "ext:lsp-mode" (&optional path))

(defun ada-ts-mode-lspclient-lsp-mode ()
  "Return lsp-mode client."
  (when (and (local-variable-p 'lsp-mode)
             lsp-mode)
    'lsp-mode))

(cl-defmethod ada-ts-mode-lspclient-command-execute ((_client (eql lsp-mode)) command &rest arguments)
  "Execute COMMAND with ARGUMENTS using Language Server."
  (lsp-workspace-command-execute command (vconcat arguments)))

(cl-defmethod ada-ts-mode-lspclient-command-supported-p ((_client (eql lsp-mode)) command)
  "Determine if Language Server supports COMMAND."
  (lsp-can-execute-command? command))

(cl-defmethod ada-ts-mode-lspclient-document-id ((_client (eql lsp-mode)))
  "Determine document identifier of current buffer."
  (lsp-text-document-identifier))

(cl-defmethod ada-ts-mode-lspclient-format-region ((_client (eql lsp-mode)) beg end)
  "Format region BEG to END using Language Server."
  (lsp-format-region beg end))

(cl-defmethod ada-ts-mode-lspclient-workspace-configuration ((_client (eql lsp-mode)) scope)
  "Retrieve workspace configuration for SCOPE."
  (cl-labels
      ((htable-to-plist (htable)
         (let ((plist))
           (maphash
            (lambda (key value)
              (setq value
                    (cond ((hash-table-p value)
                           (htable-to-plist value))
                          ((vectorp value)
                           (append value nil))
                          ((eq value :json-false)
                           nil)
                          (t value)))
              (when value
                (setq plist (plist-put plist
                                       (intern (concat ":" key))
                                       value))))
            htable)
           plist)))
    (when-let* ((namespaces (string-split scope "\\."))
                (htable (lsp-configuration-section (car namespaces)))
                (plist (htable-to-plist htable)))
      ;; Remove scope namespaces
      (seq-do
       (lambda (namespace)
         (setq plist (plist-get plist (intern (concat ":" namespace)))))
       namespaces)
      plist)))

(cl-defmethod ada-ts-mode-lspclient-workspace-root ((_client (eql lsp-mode)) path)
  "Determine workspace root for PATH."
  (when-let* ((root (lsp-workspace-root path)))
    (file-name-as-directory (expand-file-name root))))

(add-hook 'ada-ts-mode-lspclient-find-functions #'ada-ts-mode-lspclient-lsp-mode)

(provide 'ada-ts-mode-lspclient-lsp-mode)

;;;###autoload
(with-eval-after-load 'ada-ts-mode
  (with-eval-after-load 'lsp-mode
    (require 'ada-ts-mode-lspclient-lsp-mode)))

;;; ada-ts-mode-lspclient-lsp-mode.el ends here
