;;; ada-ts-als.el -- Language Server support in Ada files -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Code:

(require 'ada-ts-mode-lspclient)
(eval-when-compile (require 'cl-lib)) ; cl-labels
(require 'json)
(require 'project)
(require 'rx)
(require 'url-parse)
(require 'url-util)
(require 'xdg)

;;; ALS Configuration Management

(defun ada-ts-als--user-config-file ()
  "Locate ALS user configuration file, which may not exist."
  (file-name-concat (xdg-config-home) "als" "config.json"))

(defun ada-ts-als--workspace-config-file ()
  "Locate ALS workspace configuration file, which may not exist."
  (if-let* ((client (lspclient/current))
            (root (lspclient/workspace-root client (buffer-file-name))))
      (file-name-concat root ".als.json")
    (if-let* ((project (project-current))
              (root (project-root project)))
        (file-name-concat root ".als.json"))))

(defvar ada-ts-als--config-verbose nil)

(defun ada-ts-als--composite-config (&optional false)
  "Construct composite configuration.

If FALSE is provided, it is used as the value for a corresponding JSON
\\='false\\=' value, otherwise nil is used."
  (cl-labels
      ((display-config (desc config)
         (message "%s:" desc)
         (while config
           (let ((key (pop config))
                 (value (pop config)))
             (message "   %s => %s" key value))))
       (merge-resolve (old new)
         (if (and (plistp old) (plistp new))
             (map-merge-with 'plist #'merge-resolve old new)
           new)))
    (let (config configs)
      (when-let* ((client (lspclient/current)))
        (setq config (lspclient/workspace-configuration client "ada" false))
        (push config configs)
        (when ada-ts-als--config-verbose
          (display-config "LSP Client" config)))
      (let ((workspace-config-file (ada-ts-als--workspace-config-file))
            (user-config-file (ada-ts-als--user-config-file)))
        (dolist (file (list user-config-file workspace-config-file))
          (when (file-exists-p file)
            (if (not (file-readable-p file))
                (message "ALS Configuration unreadable: %s" file)
              (setq config (ada-ts-als--read-json-file file false))
              (push config configs)
              (when ada-ts-als--config-verbose
                (display-config file config))))))
      (apply #'map-merge-with 'plist #'merge-resolve configs))))

(defun ada-ts-als--read-json-file (file &optional false)
  "Read JSON FILE converting to a property list.

FALSE specifies the representation to use for JSON false values.

The JSON file may contain comments or trailing commas."
  (with-temp-buffer
    (insert-file-contents file)
    (dolist (subexp
             '((or (group-n 1 (: "/*" (*? anychar) "*/"))        ; Multi-line comment
                   (group-n 1 (: "//" (* nonl))))                ; Single-line comment
               (: (group-n 1 ",") (* whitespace) (or "}" "]")))) ; Trailing comma
      (let ((regexp
             (rx-to-string
              `(or ,subexp
                   (: "\"" (or "\""                                   ; Empty string
                               (: (*? anychar) (not "\\") "\""))))))) ; Non-empty string
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil 'noerror)
          (when (match-beginning 1)
            (replace-match "" nil nil nil 1)))))
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-false false))
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (when (not (eobp))
        (json-read-object)))))

;;; Miscellaneous Utilities

(defun ada-ts-als--project-root ()
  "Locate project root."
  (if-let* ((client (lspclient/current)))
      (lspclient/workspace-root client (buffer-file-name))
    (if-let* ((project (project-current)))
        (project-root project))))

(defun ada-ts-als--project-file-absolute-path (project-file-path-or-uri)
  "Normalize PROJECT-FILE-PATH-OR-URI to an absolute path."
  (when-let*
      ((project-file
        (let* ((obj (url-generic-parse-url (url-unhex-string project-file-path-or-uri)))
               (type (url-type obj)))
          (if (and type (string-equal type "file"))
              (let ((path (url-filename obj)))
                ;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=76982
                (if (and (eq system-type 'windows-nt)
                         (string-equal (substring path 0 1) "/"))
                    (substring path 1) ; Strip leading separator on Windows
                  path))
            ;; Doesn't appear to be a URI, treat as path
            project-file-path-or-uri)))
       (root (ada-ts-als--project-root)))
    (directory-file-name (expand-file-name project-file root))))

(defun ada-ts-als--uri-to-path (uri)
  "Convert URI to file path."
  (let* ((obj (url-generic-parse-url (url-unhex-string uri)))
         (path (url-filename obj)))
    ;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=76982
    (when (and (eq system-type 'windows-nt)
               (string-equal (substring path 0 1) "/"))
      ;; Strip leading separator on Windows
      (setq path (substring path 1)))
    (directory-file-name path)))

(defun ada-ts-als--source-dirs (&optional client)
  "Retrieve project's source directories, using CLIENT if provided."
  (when-let* ((client (or client (lspclient/current)))
              (command "als-source-dirs")
              ((lspclient/command-supported-p client command))
              (result (lspclient/command-execute client command)))
    (seq-map
     (lambda (dir-info)
       (ada-ts-als--uri-to-path (plist-get dir-info :uri)))
     result)))

;;; Session Management

(defun ada-ts-als--lsp-session-setup ()
  "Perform LSP session setup.

Let the LSP client know the set of source directories to associate with
the session.  This set of directories may contain locations which are
outside the project root, but should still be associated with the same
LSP session.

Also, register any project external roots."
  (when-let* (((derived-mode-p 'ada-ts-mode))
              (client (lspclient/current))
              (source-dirs (ada-ts-als--source-dirs client)))
    ;; Let LSP client know about all source directories to preserve
    ;; the session across different directory roots.
    (lspclient/workspace-dirs-add client source-dirs)))

(add-hook 'lspclient/session-hook #'ada-ts-als--lsp-session-setup)

;;; Mode-level Internal Utilities

(defun ada-ts-als--project-file ()
  "Determine path of GNAT Project file, using Language Server.

The project file will be checked for in the ALS configuration.  When not
found in the configuration, the Language Server will be queried if an
LSP client is active."

  ;; First, check the ALS and LSP client configuration.  This is
  ;; preferred if the actual project file cannot be found since the
  ;; Language Server will return a default project file name (when
  ;; queried for the project file) when it can't find the configured
  ;; project file.  In this situation we prefer the non-existent user
  ;; configured project file over a non-existent Language Server
  ;; project file.

  (if-let* ((config (ada-ts-als--composite-config))
            (project-file (plist-get config :projectFile)))
      (ada-ts-als--project-file-absolute-path project-file)
    (when-let* ((client (lspclient/current))
                (project-file
                 (let ((command "als-project-file"))
                   (and (lspclient/command-supported-p client command)
                        (lspclient/command-execute client command)))))
      ;; The Ada Language Server can return an empty string when it
      ;; can't find the project file.
      (unless (string-empty-p project-file)
        (ada-ts-als--project-file-absolute-path project-file)))))

(defun ada-ts-als--maybe-find-other-file ()
  "Attempt to find other file, returning non-nil when succeeded."
  (when-let* ((client (lspclient/current))
              (command "als-other-file")
              ((lspclient/command-supported-p client command))
              (document-id (lspclient/document-id client)))
    (prog1
        t ; Let caller know we handled this command
      (lspclient/command-execute client command document-id))))

;;; Commands

(defun ada-ts-als-show-composite-config ()
  "Show Ada Language Server composite configuration.

The composite configuration consists of the ALS user configuration (if
exists), the ALS workspace configuration (if exists) and the LSP client
specific configuration (if exists).

The composite configuration is layered such that the LSP client
configuration is higher priority than the ALS workspace configuration,
which is higher priority than the ALS user configuration."
  (interactive nil ada-ts-mode)
  (let ((config (ada-ts-als--composite-config :json-false)))
    (with-current-buffer (get-buffer-create "*ALS composite configuration*")
      (read-only-mode -1)
      (with-silent-modifications
        (erase-buffer)
        (insert
         (let ((json-false :json-false))
           (json-encode config)))
        (json-pretty-print-buffer)
        (goto-char (point-min)))
      (cond ((require 'json-mode nil 'noerror)
             (when (fboundp 'json-mode)
               (json-mode)))
            ((require 'js nil 'noerror)
             (when (fboundp 'js-json-mode)
               (js-json-mode))))
      (read-only-mode +1)
      (view-mode)
      (pop-to-buffer (current-buffer)))))

(defun ada-ts-als-find-user-config-file ()
  "Find ALS User Configuration File."
  (interactive nil ada-ts-mode)
  (when-let* ((file (ada-ts-als--user-config-file)))
    (find-file file)))

(defun ada-ts-als-find-workspace-config-file ()
  "Find ALS Workspace Configuration File."
  (interactive nil ada-ts-mode)
  (when-let* ((file (ada-ts-als--workspace-config-file)))
    (find-file file)))

(provide 'ada-ts-als)

;;; ada-ts-als.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("lspclient/" . "ada-ts-mode-lspclient-"))
;; End:
