;;; ada-ts-lspclient-eglot.el -- LSP client interface for Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Troy Brown

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

(require 'ada-ts-lspclient)
(require 'cl-generic)
(require 'eglot)
(require 'json)

;;;; Customization

(defcustom ada-ts-lspclient-eglot-stay-out-of
  '(;; Let major mode control Imenu
    imenu)
  "Mode specific settings for Eglot's `eglot-stay-out-of'."
  :type '(repeat symbol)
  :group 'ada-ts-lspclient
  :link '(custom-manual :tag "LSP Client Support" "(ada-ts-mode)LSP Client Support")
  :package-version '(ada-ts-mode . "0.9.0"))

(defcustom ada-ts-lspclient-eglot-ignored-server-capabilities
  '(;; Interferes with Emacs indenting
    ;; See: https://github.com/AdaCore/ada_language_server/issues/1197
    :documentOnTypeFormattingProvider)
  "Mode specific settings for Eglot's `eglot-ignored-server-capabilities'."
  :type '(repeat symbol)
  :group 'ada-ts-lspclient
  :link '(custom-manual :tag "LSP Client Support" "(ada-ts-mode)LSP Client Support")
  :package-version '(ada-ts-mode . "0.9.0"))

;;;; LSP Client Support

(defun ada-ts-lspclient-eglot-try ()
  "Return Eglot client."
  (when (eglot-managed-p)
    'eglot))

(cl-defmethod ada-ts-lspclient-command-execute ((_client (eql eglot)) command &rest arguments)
  "Execute COMMAND with ARGUMENTS using Language Server."
  (ada-ts-lspclient-eglot--normalize
   (cond ((functionp 'eglot-execute-command)
          (eglot-execute-command (eglot-current-server)
                                 command (vconcat arguments)))
         ((functionp 'eglot-execute)
          (eglot-execute (eglot-current-server)
                         `( :command   ,command
                            :arguments ,(vconcat arguments)))))))

(cl-defmethod ada-ts-lspclient-command-supported-p ((_client (eql eglot)) command)
  "Determine if Language Server supports COMMAND."
  (when-let* ((server-capable
               (cond ((functionp 'eglot-server-capable)  #'eglot-server-capable)
                     ((functionp 'eglot--server-capable) #'eglot--server-capable)))
              (command-provider (funcall server-capable :executeCommandProvider))
              (commands (plist-get command-provider :commands)))
    (seq-contains-p commands command)))

(cl-defmethod ada-ts-lspclient-document-id ((_client (eql eglot)))
  "Determine document identifier of current buffer."
  (when-let* ((path-to-uri
               (cond ((functionp 'eglot-path-to-uri)  #'eglot-path-to-uri)
                     ((functionp 'eglot--path-to-uri) #'eglot--path-to-uri))))
    `(:uri ,(funcall path-to-uri (buffer-file-name)))))

(cl-defmethod ada-ts-lspclient-format-region ((_client (eql eglot)) beg end)
  "Format region BEG to END of using Language Server."
  (if (= (- end beg) (buffer-size))
      (eglot-format-buffer)
    (eglot-format beg end)))

(cl-defmethod ada-ts-lspclient-workspace-configuration ((_client (eql eglot)) scope &optional false)
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

(cl-defmethod ada-ts-lspclient-workspace-root ((_client (eql eglot)) path)
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

(defun ada-ts-lspclient-eglot--normalize (value)
  "Normalize VALUE using lists, property lists, etc."
  (cond ((listp value)
         (seq-map #'ada-ts-lspclient-eglot--normalize value))
        ((vectorp value)
         (append (seq-map #'ada-ts-lspclient-eglot--normalize value) nil))
        ((eq value :json-false) nil)
        (t value)))

;;;; Configuration

(defun ada-ts-lspclient-eglot--find-mode-config (mode-to-find)
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

(defun ada-ts-lspclient-eglot--config ()
  "Configure Eglot for mode.

No configuration was provided for `ada-ts-mode' in the version of Eglot
as shipped with Emacs 29, so it is added if it cannot be found.

The language id was not properly inferred for tree-sitter major modes in
the version of Eglot shipped with Emacs 29, so the language id is
included if the mode configuration must be added."
  (unless (ada-ts-lspclient-eglot--find-mode-config 'ada-ts-mode)
    (if-let* ((config '(ada-ts-mode :language-id "ada"))
              (entry (ada-ts-lspclient-eglot--find-mode-config 'ada-mode))
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

(ada-ts-lspclient-eglot--config)

;;;; Setup

(defun ada-ts-lspclient-eglot--setup ()
  "Mode specific settings for Eglot."
  (when ada-ts-lspclient-eglot-stay-out-of
    (setq-local eglot-stay-out-of
                (seq-union (default-value 'eglot-stay-out-of)
                           ada-ts-lspclient-eglot-stay-out-of)))
  (when ada-ts-lspclient-eglot-ignored-server-capabilities
    (setq-local eglot-ignored-server-capabilities
                (seq-union (default-value 'eglot-ignored-server-capabilities)
                           ada-ts-lspclient-eglot-ignored-server-capabilities))))

(add-hook 'ada-ts-lspclient-setup-hook #'ada-ts-lspclient-eglot--setup)

(when (derived-mode-p 'ada-ts-mode)
  (ada-ts-lspclient-eglot--setup))

(add-hook 'ada-ts-lspclient-find-functions #'ada-ts-lspclient-eglot-try)

(provide 'ada-ts-lspclient-eglot)

;;;###autoload
(with-eval-after-load 'ada-ts-mode
  (with-eval-after-load 'eglot
    (require 'ada-ts-lspclient-eglot)))

;;; ada-ts-lspclient-eglot.el ends here
