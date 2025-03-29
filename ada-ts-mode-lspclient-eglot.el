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
(require 'json)

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

(cl-defmethod ada-ts-mode-lspclient-workspace-configuration ((_client (eql eglot)) scope &optional false)
  "Retrieve workspace configuration for SCOPE.

FALSE specifies the representation to use for JSON false values."

  ;; Since Eglot's property list configuration may not contain the
  ;; desired FALSE encoding, convert the configuration to JSON, then
  ;; convert back controlling the desired encoding.

  (when-let* ((namespaces (string-split scope "\\."))
              (config-json
               (let ((json-false :json-false))
                 (json-encode
                  (eglot--workspace-configuration-plist (eglot-current-server)))))
              (config-plist
               (let ((json-object-type 'plist)
                     (json-key-type 'keyword)
                     (json-false false))
                 (json-read-from-string config-json))))
    ;; Remove scope namespaces
    (map-nested-elt config-plist
                    (seq-map (lambda (n)
                               (intern (concat ":" n)))
                             namespaces))))

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

(add-hook 'ada-ts-mode-lspclient-find-functions #'ada-ts-mode-lspclient-eglot)

(provide 'ada-ts-mode-lspclient-eglot)

;;;###autoload
(with-eval-after-load 'ada-ts-mode
  (with-eval-after-load 'eglot
    (require 'ada-ts-mode-lspclient-eglot)))

;;; ada-ts-mode-lspclient-eglot.el ends here
