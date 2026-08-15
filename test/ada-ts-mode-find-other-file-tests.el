;;; ada-ts-mode-find-other-file-tests.el --- Tests specific to finding other files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Troy Brown

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
(require 'ert)
(require 'ert-x)

(ert-deftest ada-ts-mode-test-find-other-file-gnat ()
  "Test command `ada-ts-mode-find-other-file' with GNAT file extension convention."
  (let* ((proj-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.ads")
         (spec-path (expand-file-name spec-file proj-dir))
         (body-file "hello_world.adb")
         (body-path (expand-file-name body-file proj-dir))
         (root-marker "hello_world.gpr")
         (data `((,spec-file ,spec-path ,body-path)    ; spec -> body
                 (,body-file ,body-path ,spec-path)))) ; body -> spec
    (unwind-protect
        (progn
          (make-empty-file (expand-file-name root-marker proj-dir) 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          (pcase-dolist (`(,initial-file ,initial-path ,final-path) data)
            (with-file-in-project initial-file proj-dir root-marker
              (let* ((window (selected-window)))
                (should (file-equal-p (buffer-file-name (window-buffer window)) initial-path))
                (let ((inhibit-message t)
                      (ff-always-try-to-create nil))
                  (ada-ts-mode-find-other-file))
                (let* ((buffer (window-buffer (selected-window)))
                       (buffer-name (buffer-file-name buffer)))
                  (should (file-equal-p buffer-name final-path))
                  (should (equal (selected-window) window))
                  (kill-buffer buffer))))))
      (delete-directory proj-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-vax ()
  "Test command `ada-ts-mode-find-other-file' with VAX file extension convention."
  (let* ((proj-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "HELLO_WORLD_.ADA")
         (spec-path (expand-file-name spec-file proj-dir))
         (body-file "HELLO_WORLD.ADA")
         (body-path (expand-file-name body-file proj-dir))
         (root-marker "HELLO_WORLD.GPR")
         (data `((,spec-file ,spec-path ,body-path)    ; spec -> body
                 (,body-file ,body-path ,spec-path)))) ; body -> spec
    (unwind-protect
        (progn
          (make-empty-file (expand-file-name root-marker proj-dir) 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          (pcase-dolist (`(,initial-file ,initial-path ,final-path) data)
            (with-file-in-project initial-file proj-dir root-marker
              (let* ((window (selected-window)))
                (should (file-equal-p (buffer-file-name (window-buffer window)) initial-path))
                (let ((inhibit-message t)
                      (ff-always-try-to-create nil))
                  (ada-ts-mode-find-other-file))
                (let* ((buffer (window-buffer (selected-window)))
                       (buffer-name (buffer-file-name buffer)))
                  (should (file-equal-p buffer-name final-path))
                  (should (equal (selected-window) window))
                  (kill-buffer buffer))))))
      (delete-directory proj-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-apex ()
  "Test command `ada-ts-mode-find-other-file' with Apex file extension convention."
  (let* ((proj-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.1.ada")
         (spec-path (expand-file-name spec-file proj-dir))
         (body-file "hello_world.2.ada")
         (body-path (expand-file-name body-file proj-dir))
         (root-marker "hello_world.gpr")
         (data `((,spec-file ,spec-path ,body-path)    ; spec -> body
                 (,body-file ,body-path ,spec-path)))) ; body -> spec
    (unwind-protect
        (progn
          (make-empty-file (expand-file-name root-marker proj-dir) 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          (pcase-dolist (`(,initial-file ,initial-path ,final-path) data)
            (with-file-in-project initial-file proj-dir root-marker
              (let* ((window (selected-window)))
                (should (file-equal-p (buffer-file-name (window-buffer window)) initial-path))
                (let ((inhibit-message t)
                      (ff-always-try-to-create nil))
                  (ada-ts-mode-find-other-file))
                (let* ((buffer (window-buffer (selected-window)))
                       (buffer-name (buffer-file-name buffer)))
                  (should (file-equal-p buffer-name final-path))
                  (should (equal (selected-window) window))
                  (kill-buffer buffer))))))
      (delete-directory proj-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-other-dir ()
  "Test command `ada-ts-mode-find-other-file' when other file is in a different directory."
  (let* ((proj-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "spec/hello_world.ads")
         (spec-path (expand-file-name spec-file proj-dir))
         (body-file "body/hello_world.adb")
         (body-path (expand-file-name body-file proj-dir))
         (root-marker "hello_world.gpr")
         (data `((,spec-file ,spec-path ,body-path)    ; spec -> body
                 (,body-file ,body-path ,spec-path)))) ; body -> spec
    (unwind-protect
        (progn
          (make-empty-file (expand-file-name root-marker proj-dir) 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          (pcase-dolist (`(,initial-file ,initial-path ,final-path) data)
            (with-file-in-project initial-file proj-dir root-marker
              (let* ((window (selected-window)))
                (should (file-equal-p (buffer-file-name (window-buffer window)) initial-path))
                (let ((inhibit-message t)
                      (ff-always-try-to-create nil))
                  (ada-ts-mode-find-other-file))
                (let* ((buffer (window-buffer (selected-window)))
                       (buffer-name (buffer-file-name buffer)))
                  (should (file-equal-p buffer-name final-path))
                  (should (equal (selected-window) window))
                  (kill-buffer buffer))))))
      (delete-directory proj-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-other-window ()
  "Test command `ada-ts-mode-find-other-file' opening other file in a different window."
  (let* ((proj-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.ads")
         (spec-path (expand-file-name spec-file proj-dir))
         (body-file "hello_world.adb")
         (body-path (expand-file-name body-file proj-dir))
         (root-marker "hello_world.gpr")
         (data `((,spec-file ,spec-path ,body-path)    ; spec -> body
                 (,body-file ,body-path ,spec-path)))) ; body -> spec
    (unwind-protect
        (progn
          (make-empty-file (expand-file-name root-marker proj-dir) 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          (pcase-dolist (`(,initial-file ,initial-path ,final-path) data)
            (with-file-in-project initial-file proj-dir root-marker
              (let* ((window (selected-window)))
                (should (file-equal-p (buffer-file-name (window-buffer window)) initial-path))
                (let ((inhibit-message t)
                      (ff-always-try-to-create nil))
                  (ada-ts-mode-find-other-file 'in-other-window))
                (let* ((buffer (window-buffer (selected-window)))
                       (buffer-name (buffer-file-name buffer)))
                  (should (file-equal-p buffer-name final-path))
                  (should (not (equal (selected-window) window)))
                  (kill-buffer buffer))))))
      (delete-directory proj-dir 'recursive))))

(provide 'ada-ts-mode-find-other-file-tests)

;;; ada-ts-mode-find-other-file-tests.el ends here
