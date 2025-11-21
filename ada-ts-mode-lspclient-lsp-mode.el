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

(declare-function lsp-can-execute-command?      "ext:lsp-mode" (command-name))
(declare-function lsp-configuration-section     "ext:lsp-mode" (section))
(declare-function lsp-format-buffer             "ext:lsp-mode" ())
(declare-function lsp-format-region             "ext:lsp-mode" (s e))
(declare-function lsp-text-document-identifier  "ext:lsp-mode" ())
(declare-function lsp-workspaces                "ext:lsp-mode" ())
(declare-function lsp-workspace-command-execute "ext:lsp-mode" (command &optional args))
(declare-function lsp-workspace-root            "ext:lsp-mode" (&optional path))
(declare-function lsp--workspace-buffers        "ext:lsp-mode" (workspace))
(declare-function lsp--workspace-root           "ext:lsp-mode" (workspace))
(defvar lsp-clients nil)

(defun ada-ts-mode-lspclient-lsp-mode ()
  "Return lsp-mode client."
  (when (and (local-variable-p 'lsp-mode)
             lsp-mode)
    'lsp-mode))

(cl-defmethod ada-ts-mode-lspclient-command-execute ((_client (eql lsp-mode)) command &rest arguments)
  "Execute COMMAND with ARGUMENTS using Language Server."
  (ada-ts-mode-lspclient--lsp-mode-normalize
   (lsp-workspace-command-execute command (vconcat arguments))))

(cl-defmethod ada-ts-mode-lspclient-command-supported-p ((_client (eql lsp-mode)) command)
  "Determine if Language Server supports COMMAND."
  (lsp-can-execute-command? command))

(cl-defmethod ada-ts-mode-lspclient-document-id ((_client (eql lsp-mode)))
  "Determine document identifier of current buffer."
  (lsp-text-document-identifier))

(cl-defmethod ada-ts-mode-lspclient-format-region ((_client (eql lsp-mode)) beg end)
  "Format region BEG to END using Language Server."
  (if (= (- end beg) (buffer-size))
      (lsp-format-buffer)
    (lsp-format-region beg end)))

(cl-defmethod ada-ts-mode-lspclient-workspace-configuration ((_client (eql lsp-mode)) scope)
  "Retrieve workspace configuration for SCOPE."
  (when-let* ((namespaces (string-split scope "\\."))
              (htable (lsp-configuration-section (car namespaces)))
              (plist (ada-ts-mode-lspclient--lsp-mode-normalize htable)))
    ;; Remove scope namespaces
    (seq-do
     (lambda (namespace)
       (setq plist (plist-get plist (intern (concat ":" namespace)))))
     namespaces)
    plist))

(defvar ada-ts-mode-lspclient--lsp-workspace-extra-dirs-alist nil)

(cl-defmethod ada-ts-mode-lspclient-workspace-dirs-add ((_client (eql lsp-mode)) dirs)
  "Add workspace DIRS to session."
  (when-let* ((root (lsp--workspace-root (car (lsp-workspaces))))
              (root-dir (file-name-as-directory root)))
    (setq dirs (seq-filter
                (lambda (dir)
                  (not (string-prefix-p root-dir dir)))
                dirs))
    (setq ada-ts-mode-lspclient--lsp-workspace-extra-dirs-alist
          (assoc-delete-all root ada-ts-mode-lspclient--lsp-workspace-extra-dirs-alist))
    (when dirs
      (push (cons root dirs) ada-ts-mode-lspclient--lsp-workspace-extra-dirs-alist))))

(cl-defmethod ada-ts-mode-lspclient-workspace-root ((_client (eql lsp-mode)) path)
  "Determine workspace root for PATH."
  (when-let* ((root (lsp-workspace-root path)))
    (file-name-as-directory (expand-file-name root))))

(defun ada-ts-mode-lspclient--lsp-mode-normalize (value)
  "Normalize VALUE using lists, property lists, etc."
  (cond ((hash-table-p value)
         (let ((plist))
           (maphash
            (lambda (key value)
              (setq value (ada-ts-mode-lspclient--lsp-mode-normalize value))
              (when value
                (setq plist (plist-put plist
                                       (intern (concat ":" key))
                                       value))))
            value)
           plist))
        ((listp value)
         (seq-map #'ada-ts-mode-lspclient--lsp-mode-normalize value))
        ((vectorp value)
         (append (seq-map #'ada-ts-mode-lspclient--lsp-mode-normalize value) nil))
        ((eq value :json-false) nil)
        (t value)))

(defun ada-ts-mode-lspclient--lsp-mode-initialized ()
  "Notify registered hooks of LSP session establishment."
  (when-let* ((workspace (car (lsp-workspaces)))
              (buffer (car (lsp--workspace-buffers workspace))))
    (with-current-buffer buffer
      (run-hooks 'ada-ts-mode-lspclient-session-hook))))

(add-hook 'ada-ts-mode-lspclient-find-functions #'ada-ts-mode-lspclient-lsp-mode)
(add-hook 'lsp-after-initialize-hook #'ada-ts-mode-lspclient--lsp-mode-initialized)

(defvar ada-ts-mode-lspclient--lsp-library-folders-fn nil)

(defun ada-ts-mode-lspclient--lsp-extra-folders (workspace)
  "Find extra folders for WORKSPACE."
  (let (folders)
    (when ada-ts-mode-lspclient--lsp-library-folders-fn
      (setq folders (funcall ada-ts-mode-lspclient--lsp-library-folders-fn workspace)))
    (if-let* ((root (lsp--workspace-root workspace)))
        (append folders
                (cdr (assoc-string root ada-ts-mode-lspclient--lsp-workspace-extra-dirs-alist)))
      folders)))

(with-eval-after-load 'lsp-ada
  (eval
   '(when-let* ((client (gethash 'ada-ls lsp-clients)))
      (setq ada-ts-mode-lspclient--lsp-library-folders-fn
            (lsp--client-library-folders-fn client))
      (setf (lsp--client-library-folders-fn client)
            #'ada-ts-mode-lspclient--lsp-extra-folders))))

(provide 'ada-ts-mode-lspclient-lsp-mode)

;;;###autoload
(with-eval-after-load 'ada-ts-mode
  (with-eval-after-load 'lsp-mode
    (require 'ada-ts-mode-lspclient-lsp-mode)))

;;; ada-ts-mode-lspclient-lsp-mode.el ends here
