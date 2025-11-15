;;; ada-ts-mode-lspclient-eglot.el -- LSP client interface for Eglot -*- lexical-binding: t; -*-

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
(require 'eglot)

(defun ada-ts-mode-lspclient-eglot ()
  "Return Eglot client."
  (when (eglot-managed-p)
    'eglot))

(cl-defmethod ada-ts-mode-lspclient-command-execute ((_client (eql eglot)) command &rest arguments)
  "Execute COMMAND with ARGUMENTS using Language Server."
  (cond ((functionp 'eglot-execute-command)
         (eglot-execute-command (eglot-current-server)
                                command (vconcat arguments)))
        ((functionp 'eglot-execute)
         (eglot-execute (eglot-current-server)
                        `( :command   ,command
                           :arguments ,(vconcat arguments))))))

(cl-defmethod ada-ts-mode-lspclient-command-supported-p ((_client (eql eglot)) command)
  "Determine if Language Server supports COMMAND."
  (when-let* ((server-capable
               (cond ((functionp 'eglot-server-capable)  #'eglot-server-capable)
                     ((functionp 'eglot--server-capable) #'eglot--server-capable)))
              (command-provider (funcall server-capable :executeCommandProvider))
              (commands (plist-get command-provider :commands)))
    (seq-contains-p commands command)))

(cl-defmethod ada-ts-mode-lspclient-document-id ((_client (eql eglot)))
  "Determine document identifier of current buffer."
  (when-let* ((path-to-uri
               (cond ((functionp 'eglot-path-to-uri)  #'eglot-path-to-uri)
                     ((functionp 'eglot--path-to-uri) #'eglot--path-to-uri))))
    `(:uri ,(funcall path-to-uri (buffer-file-name)))))

(cl-defmethod ada-ts-mode-lspclient-format-region ((_client (eql eglot)) beg end)
  "Format region BEG to END of using Language Server."
  (eglot-format beg end))

(cl-defmethod ada-ts-mode-lspclient-workspace-configuration ((_client (eql eglot)) scope)
  "Retrieve workspace configuration for SCOPE."
  (when-let* ((namespaces (string-split scope "\\."))
              (plist (eglot--workspace-configuration-plist (eglot-current-server))))
    ;; Remove scope namespaces
    (seq-do
     (lambda (namespace)
       (setq plist (plist-get plist (intern (concat ":" namespace)))))
     namespaces)
    plist))

(cl-defmethod ada-ts-mode-lspclient-workspace-root ((_client (eql eglot)) path)
  "Determine workspace root for PATH."
  (when-let* ((expanded-path (expand-file-name path))
              (workspace-folders
               (seq-map
                (lambda (folder)
                  (file-name-as-directory
                   (expand-file-name (plist-get folder :name))))
                (eglot-workspace-folders (eglot-current-server)))))
    (seq-find
     (lambda (folder)
       (string-prefix-p folder expanded-path))
     workspace-folders)))

(defun ada-ts-mode-lspclient-eglot--find-mode-config (mode-to-find)
  "Find Eglot server configuration for MODE-TO-FIND."
  (seq-find
   (pcase-lambda (`(,modes . ,contact))
     (when (or (and (symbolp modes)
                    (eq mode-to-find modes))
               (and (listp modes)
                    (keywordp (cadr modes))
                    (eq mode-to-find (car modes)))
               (and (listp modes)
                    (seq-find
                     (lambda (mode)
                       (or (and (symbolp mode)
                                (eq mode-to-find mode))
                           (and (listp mode)
                                (keywordp (cadr mode))
                                (eq mode-to-find (car mode)))))
                     modes)))
       (cons modes contact)))
   eglot-server-programs))

(defun ada-ts-mode-lspclient-eglot--setup ()
  "Setup Eglot for mode.

No configuration was provided for `ada-ts-mode' in the version of Eglot
as shipped with Emacs 29, so it is added if it cannot be found.

The language id was not properly inferred for tree-sitter major modes in
the version of Eglot shipped with Emacs 29, so the language id is
included if the mode configuration must be added."
  (unless (ada-ts-mode-lspclient-eglot--find-mode-config 'ada-ts-mode)
    (if-let* ((config '(ada-ts-mode :language-id "ada"))
              (entry (ada-ts-mode-lspclient-eglot--find-mode-config 'ada-mode))
              (modes (car entry))
              (contact (cdr entry))
              (new-modes
               (cond ((symbolp modes)
                      (list modes config))
                     ((and (listp modes)
                           (keywordp (cadr modes)))
                      (list modes config))
                     ((listp modes)
                      (append modes (list config)))
                     (t nil))))
        ;; Update existing Ada configuration
        (add-to-list 'eglot-server-programs (cons new-modes contact))
      ;; Add Ada configuration
      (add-to-list 'eglot-server-programs
                   (list `(ada-mode ,config) "ada_language_server")))))

(ada-ts-mode-lspclient-eglot--setup)

(add-hook 'ada-ts-mode-lspclient-find-functions #'ada-ts-mode-lspclient-eglot)

(provide 'ada-ts-mode-lspclient-eglot)

;;;###autoload
(with-eval-after-load 'ada-ts-mode
  (with-eval-after-load 'eglot
    (require 'ada-ts-mode-lspclient-eglot)))

;;; ada-ts-mode-lspclient-eglot.el ends here
