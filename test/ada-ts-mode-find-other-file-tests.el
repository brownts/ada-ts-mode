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
  (let* ((temp-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.ads")
         (spec-path (expand-file-name spec-file temp-dir))
         (body-file "hello_world.adb")
         (body-path (expand-file-name body-file temp-dir))
         (proj-file "hello_world.gpr")
         (proj-path (expand-file-name proj-file temp-dir)))
    (unwind-protect
        (progn
          (make-empty-file proj-path 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          ;; spec -> body
          (with-file-in-project spec-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) spec-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name body-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer))))
          ;; body -> spec
          (with-file-in-project body-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) body-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name spec-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer)))))
      (delete-directory temp-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-vax ()
  "Test command `ada-ts-mode-find-other-file' with VAX file extension convention."
  (let* ((temp-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "HELLO_WORLD_.ADA")
         (spec-path (expand-file-name spec-file temp-dir))
         (body-file "HELLO_WORLD.ADA")
         (body-path (expand-file-name body-file temp-dir))
         (proj-file "HELLO_WORLD.GPR")
         (proj-path (expand-file-name proj-file temp-dir)))
    (unwind-protect
        (progn
          (make-empty-file proj-path 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          ;; spec -> body
          (with-file-in-project spec-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) spec-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name body-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer))))
          ;; body -> spec
          (with-file-in-project body-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) body-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name spec-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer)))))
      (delete-directory temp-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-apex ()
  "Test command `ada-ts-mode-find-other-file' with Apex file extension convention."
  (let* ((temp-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.1.ada")
         (spec-path (expand-file-name spec-file temp-dir))
         (body-file "hello_world.2.ada")
         (body-path (expand-file-name body-file temp-dir))
         (proj-file "hello_world.gpr")
         (proj-path (expand-file-name proj-file temp-dir)))
    (unwind-protect
        (progn
          (make-empty-file proj-path 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          ;; spec -> body
          (with-file-in-project spec-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) spec-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name body-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer))))
          ;; body -> spec
          (with-file-in-project body-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) body-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name spec-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer)))))
      (delete-directory temp-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-other-dir ()
  "Test command `ada-ts-mode-find-other-file' when other file is in a different directory."
  (let* ((temp-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.ads")
         (spec-path (expand-file-name spec-file (concat temp-dir "/spec")))
         (body-file "hello_world.adb")
         (body-path (expand-file-name body-file (concat temp-dir "/body")))
         (proj-file "hello_world.gpr")
         (proj-path (expand-file-name proj-file temp-dir)))
    (unwind-protect
        (progn
          (make-empty-file proj-path 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          ;; spec -> body
          (with-file-in-project (concat "spec/" spec-file) temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) spec-path))
              (let ((inhibit-message t)
                    (ff-quiet-mode t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name body-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer))))
          ;; body -> spec
          (with-file-in-project (concat "body/" body-file) temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) body-path))
              (let ((inhibit-message t)
                    (ff-quiet-mode t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name spec-path))
                (should (equal (selected-window) window))
                (kill-buffer buffer)))))
      (delete-directory temp-dir 'recursive))))

(ert-deftest ada-ts-mode-test-find-other-file-other-window ()
  "Test command `ada-ts-mode-find-other-file' opening other file in a different window."
  (let* ((temp-dir (make-temp-file "ada-ts-test-" 'dir))
         (spec-file "hello_world.ads")
         (spec-path (expand-file-name spec-file temp-dir))
         (body-file "hello_world.adb")
         (body-path (expand-file-name body-file temp-dir))
         (proj-file "hello_world.gpr")
         (proj-path (expand-file-name proj-file temp-dir)))
    (unwind-protect
        (progn
          (make-empty-file proj-path 'parents)
          (make-empty-file spec-path 'parents)
          (make-empty-file body-path 'parents)
          ;; spec -> body
          (with-file-in-project spec-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) spec-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file 'in-other-window))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name body-path))
                (should (not (equal (selected-window) window)))
                (kill-buffer buffer))))
          ;; body -> spec
          (with-file-in-project body-file temp-dir proj-file
            (let* ((window (selected-window)))
              (should (file-equal-p (buffer-file-name (window-buffer window)) body-path))
              (let ((inhibit-message t)
                    (ff-always-try-to-create nil))
                (ada-ts-mode-find-other-file 'in-other-window))
              (let* ((buffer (window-buffer (selected-window)))
                     (buffer-name (buffer-file-name buffer)))
                (should (file-equal-p buffer-name spec-path))
                (should (not (equal (selected-window) window)))
                (kill-buffer buffer)))))
      (delete-directory temp-dir 'recursive))))

(provide 'ada-ts-mode-find-other-file-tests)

;;; ada-ts-mode-find-other-file-tests.el ends here
