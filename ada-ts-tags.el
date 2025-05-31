;;; ada-ts-tags.el -- Tags support in Ada files -*- lexical-binding: t; -*-

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

(require 'etags)
(eval-when-compile (require 'rx))

;;;; Completion

(defconst ada-ts-tags--tag-regexp
  (rx (group (+ anychar))
      "/"
      (group (or "b" "f" "k" "p" "s" "t"))
      eos)
  "Regular expression matching Ada tag, including type suffix.")

(defun ada-ts-tags--adjust-completion-table (table)
  "Adjust completion TABLE to remove type suffixes from Ada tags."
  (lambda (string pred action)
    (let ((res (complete-with-action action table string pred)))
      (cond
       ;; like 'try-completion`
       ((stringp res)
        (if-let* (((string-match ada-ts-tags--tag-regexp res))
                  (mod-cand (match-string 1 res)))
            (or
             (string-equal-ignore-case mod-cand string) ; Exact match
             mod-cand) ; Longest substring, single match
          res)) ; Longest substring, multiple matches
       ;; like `all-completions`
       ((eq action t)
        (seq-map
         (lambda (cand)
           (if-let* (((string-match ada-ts-tags--tag-regexp cand))
                     (mod-cand (match-string 1 cand))
                     (type (match-string 2 cand)))
               (progn
                 (put-text-property 0 1 'etags-ada-type type mod-cand)
                 mod-cand)
             cand))
         res))
       ;; Everything else
       (t res)))))

(defun ada-ts-tags-completion-at-point-function ()
  "Ada completion at point function for tags."
  (pcase (tags-completion-at-point-function)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(ada-ts-tags--adjust-completion-table table)
            :annotation-function
            ,(lambda (cand)
               (pcase (get-text-property 0 'etags-ada-type cand)
                 ("b" " Package Body")
                 ("f" " Function")
                 ("k" " Task")
                 ("p" " Procedure")
                 ("s" " Package Spec")
                 ("t" " Type")))
            :company-kind
            ,(lambda (cand)
               (pcase (get-text-property 0 'etags-ada-type cand)
                 ("b" 'module)
                 ("f" 'function)
                 ("k" 'interface)
                 ("p" 'function)
                 ("s" 'module)
                 ("t" 'interface)))
            ,@plist))))

;;;; Cross-Reference

;; Workaround for Emacs `xref-find-definitions` bug when used with Ada tags.
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=78489

(defun ada-ts-tags--tag-exact-match-p (tag)
  "Return non-nil if current tag line matches TAG exactly."
  (let* ((ada-tag-suffix (rx "/" (or "b" "f" "k" "p" "s" "t")))
         (ada-tag (concat (regexp-quote tag) ada-tag-suffix)))
    (or (and (looking-at (concat ada-tag-suffix (rx ?\001)))
             (eq (char-after (- (point) (length tag) 1)) ?\177))
        (looking-at (concat (rx (* (not (or ?\177 ?\n))) ?\177) ada-tag (rx ?\001))))))

(when (< emacs-major-version 31)
  (add-to-list 'etags-xref-find-definitions-tag-order
               'ada-ts-tags--tag-exact-match-p 'append))

;;;; Setup

(defun ada-ts-tags--setup ()
  "Setup tags support for mode."
  (setq-local completion-at-point-functions
              '(ada-ts-tags-completion-at-point-function)))

(provide 'ada-ts-tags)

;;; ada-ts-tags.el ends here
