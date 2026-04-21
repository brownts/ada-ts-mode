;;; ada-ts-als.el -- Ada Language Server support -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Code:

(require 'ada-ts-lspclient)
(eval-when-compile (require 'cl-lib)) ; cl-labels
(require 'json)
(require 'project)
(require 'rx)
(require 'url-parse)
(require 'url-util)
(require 'xdg)

;;;; Ada Language Server Configuration

(defun ada-ts-als-user-config-file ()
  "Locate ALS user configuration file, which may not exist."
  (let ((config-home
         (if (file-remote-p default-directory)
             (expand-file-name
              (concat (file-remote-p default-directory)
                      (let ((config-home (getenv "XDG_CONFIG_HOME")))
                        (if (or (null config-home)
                                (not (file-name-absolute-p config-home)))
                            "~/.config"
                          config-home))))
           (xdg-config-home))))
    (file-name-concat config-home "als" "config.json")))

(defun ada-ts-als-find-user-config-file ()
  "Find ALS User Configuration File."
  (interactive nil ada-ts-mode)
  (when-let* ((file (ada-ts-als-user-config-file)))
    (find-file file)))

(defun ada-ts-als-workspace-config-file ()
  "Locate ALS workspace configuration file, which may not exist."
  (if-let* ((client (lspclient/current))
            (root (lspclient/workspace-root client (buffer-file-name))))
      (file-name-concat root ".als.json")
    (if-let* ((project (project-current))
              (root (project-root project)))
        (file-name-concat root ".als.json"))))

(defun ada-ts-als-find-workspace-config-file ()
  "Find ALS Workspace Configuration File."
  (interactive nil ada-ts-mode)
  (when-let* ((file (ada-ts-als-workspace-config-file)))
    (find-file file)))

(defvar ada-ts-als--config-verbose nil)

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

(defun ada-ts-als-composite-config (&optional false)
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
      (let ((workspace-config-file (ada-ts-als-workspace-config-file))
            (user-config-file (ada-ts-als-user-config-file)))
        (dolist (file (list user-config-file workspace-config-file))
          (when (file-exists-p file)
            (if (not (file-readable-p file))
                (message "ALS Configuration unreadable: %s" file)
              (setq config (ada-ts-als--read-json-file file false))
              (push config configs)
              (when ada-ts-als--config-verbose
                (display-config file config))))))
      (apply #'map-merge-with 'plist #'merge-resolve configs))))

(defun ada-ts-als-show-composite-config ()
  "Show Ada Language Server composite configuration.

The composite configuration consists of the ALS user configuration (if
exists), the ALS workspace configuration (if exists) and the LSP client
specific configuration (if exists).

The composite configuration is layered such that the LSP client
configuration is higher priority than the ALS workspace configuration,
which is higher priority than the ALS user configuration."
  (interactive nil ada-ts-mode)
  (let ((config (ada-ts-als-composite-config :json-false)))
    (with-current-buffer (get-buffer-create "*ALS composite configuration*")
      (read-only-mode -1)
      (with-silent-modifications
        (erase-buffer)
        (if config
            (let ((json-false :json-false))
              (insert (json-encode config))
              (json-pretty-print-buffer))
          (insert "{\n\n}"))
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
              (source-dirs (ada-ts-als-source-dirs)))
    ;; Let LSP client know about all source directories to preserve
    ;; the session across different directory roots.
    (lspclient/workspace-dirs-add client source-dirs)))

(add-hook 'lspclient/session-hook #'ada-ts-als--lsp-session-setup)

;;;; Formatting

(defun ada-ts-als--format-region (client beg end indent-offset)
  "Format region BEG to END with INDENT-OFFSET using Language Server CLIENT."
  (let ((inhibit-message t)
        (tab-width indent-offset)
        (standard-indent indent-offset))
    (lspclient/format-region client beg end)))

(defun ada-ts-als-format-line (indent-offset)
  "Format current line with INDENT-OFFSET using Language Server.

There are cases when the formatting may fail, such as when there is no
Language Server present, when asked to format an empty line or one
containing only spaces (see Emacs Bug#70929).  Also, depending on the
Language Server configuration, the LSP can signal an error when the
source contains syntax errors.

Returns non-nil if the line was successfully formatted."
  (when-let* ((client (lspclient/current)))
    (unless (save-excursion
              (forward-line 0)
              (looking-at-p (rx (* whitespace) eol)))
      (ignore-error error
        (let ((initial-point-column (current-column))
              (initial-indentation-column (current-indentation)))
          (ada-ts-als--format-region client (pos-bol) (pos-eol) indent-offset)
          ;; For consistency with built-in indentation behavior,
          ;; if point was in the white-space at the beginning of
          ;; the line, move point to the current indentation.
          (when (<= initial-point-column
                    initial-indentation-column)
            (back-to-indentation)))
        'success))))

(defun ada-ts-als-format-region (beg end indent-offset)
  "Format region BEG to END with INDENT-OFFSET using Language Server.

There are cases when the formatting may fail, such as when there is no
Language Server present.  Depending on the Language Server
configuration, the LSP can signal an error when the source contains
syntax errors.

Returns non-nil if the region was successfully formatted."
  (when-let* ((client (lspclient/current)))
    (ignore-error error
      (ada-ts-als--format-region client beg end indent-offset)
      'success)))

;;;; Ada Language Server Commands

;; NOTE: For commands that return paths, `expand-file-name' is used to
;; convert to the Emacs canonical path format.  This is noticeable
;; with Windows paths which are not already in this format.

(defun ada-ts-als-executables ()
  "Attempt to find the set of executables for the project."
  (when-let* ((client (lspclient/current))
              (command "als-executables")
              ((lspclient/command-supported-p client command))
              (execs (lspclient/command-execute client command)))
    (seq-map #'expand-file-name execs)))

(defun ada-ts-als-get-project-attribute-value (attribute &optional package index)
  "Attempt to find ATTRIBUTE in project file.

When specified, ATTRIBUTE, PACKAGE and INDEX should be strings.  If
PACKAGE is nil or an empty string, a top-level project attribute is
requested.  If the ATTRIBUTE is indexed, INDEX specifies the desired
index.

Returns a string (or list of strings if the attribute is a list), when
the attribute was found, else nil."
  (when-let* ((client (lspclient/current))
              (command "als-get-project-attribute-value")
              ((lspclient/command-supported-p client command)))
    ;; When the attribute isn't known, ALS will respond with an error.
    (ignore-error error
      (lspclient/command-execute client command
                                 (list :attribute attribute
                                       :pkg (or package "")
                                       :index (or index ""))))))

(defun ada-ts-als-mains ()
  "Attempt to find the mains for the project."
  (when-let* ((client (lspclient/current))
              (command "als-mains")
              ((lspclient/command-supported-p client command))
              (mains (lspclient/command-execute client command)))
    (seq-map #'expand-file-name mains)))

(defun ada-ts-als-object-dir ()
  "Attempt to find the object directory for the project."
  (when-let* ((client (lspclient/current))
              (command "als-object-dir")
              ((lspclient/command-supported-p client command))
              (object-dir (lspclient/command-execute client command)))
    (expand-file-name object-dir)))

(defun ada-ts-als-other-file ()
  "Attempt to find other file, returning non-nil when succeeded."
  (when-let* ((client (lspclient/current))
              (command "als-other-file")
              ((lspclient/command-supported-p client command))
              (document-id (lspclient/document-id client)))
    (prog1
        t ; Let caller know we handled this command
      (lspclient/command-execute client command document-id))))

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
                ;; Workaround for https://debbugs.gnu.org/76982
                (if (and (eq system-type 'windows-nt)
                         (string-equal (substring path 0 1) "/"))
                    (substring path 1) ; Strip leading separator on Windows
                  path))
            ;; Doesn't appear to be a URI, treat as path
            project-file-path-or-uri)))
       (root (ada-ts-als--project-root)))
    (directory-file-name (expand-file-name project-file root))))

(defun ada-ts-als-project-file ()
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

  (if-let* ((config (ada-ts-als-composite-config))
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

(defun ada-ts-als--uri-to-path (uri)
  "Convert URI to file path."
  (let* ((obj (url-generic-parse-url (url-unhex-string uri)))
         (path (url-filename obj)))
    ;; Workaround for https://debbugs.gnu.org/76982
    (when (and (eq system-type 'windows-nt)
               (string-equal (substring path 0 1) "/"))
      ;; Strip leading separator on Windows
      (setq path (substring path 1)))
    (directory-file-name path)))

(defun ada-ts-als-source-dirs ()
  "Retrieve project's source directories."
  (when-let* ((client (lspclient/current))
              (command "als-source-dirs")
              ((lspclient/command-supported-p client command))
              (result (lspclient/command-execute client command)))
    (seq-map
     (lambda (dir-info)
       (ada-ts-als--uri-to-path (plist-get dir-info :uri)))
     result)))

(provide 'ada-ts-als)

;;; ada-ts-als.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("lspclient/" . "ada-ts-lspclient-"))
;; End:
