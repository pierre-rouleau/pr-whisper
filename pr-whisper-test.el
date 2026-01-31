;;; pr-whisper-test.el --- Tests for pr-whisper -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for pr-whisper environment validation.

;;; Code:

(require 'ert)
(require 'pr-whisper)

(ert-deftest pr-whisper-test-validate-missing-sox ()
  "Test that validation fails when sox is not found."
  (let ((pr-whisper-sox "nonexistent-sox-binary-12345"))
    (should-error (pr-whisper--validate-environment)
                  :type 'user-error)))

(ert-deftest pr-whisper-test-validate-invalid-homedir ()
  "Test that validation fails with invalid homedir."
  (let ((pr-whisper-homedir "/nonexistent/whisper/path/12345"))
    (should-error (pr-whisper--validate-environment)
                  :type 'user-error)))

(ert-deftest pr-whisper-test-validate-missing-cli ()
  "Test that validation fails when whisper-cli is missing."
  (let ((pr-whisper-backend 'cli)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create models dir with a fake model
          (make-directory (concat pr-whisper-homedir "/models") t)
          (write-region "" nil (concat pr-whisper-homedir "/models/" pr-whisper-model))
          ;; No build/bin/whisper-cli exists
          (should-error (pr-whisper--validate-environment)
                        :type 'user-error))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-validate-missing-server ()
  "Test that validation fails when whisper-server is missing."
  (let ((pr-whisper-backend 'server)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create models dir with a fake model
          (make-directory (concat pr-whisper-homedir "/models") t)
          (write-region "" nil (concat pr-whisper-homedir "/models/" pr-whisper-model))
          ;; No build/bin/whisper-server exists
          (should-error (pr-whisper--validate-environment)
                        :type 'user-error))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-validate-missing-model ()
  "Test that validation fails when model file is missing."
  (let ((pr-whisper-backend 'cli)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create build/bin with executable whisper-cli
          (make-directory (concat pr-whisper-homedir "/build/bin") t)
          (let ((cli-path (concat pr-whisper-homedir "/build/bin/whisper-cli")))
            (write-region "#!/bin/sh\n" nil cli-path)
            (set-file-modes cli-path #o755))
          ;; No models dir
          (should-error (pr-whisper--validate-environment)
                        :type 'user-error))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-validate-cli-not-executable ()
  "Test that validation fails when whisper-cli exists but is not executable."
  (let ((pr-whisper-backend 'cli)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create build/bin with non-executable whisper-cli
          (make-directory (concat pr-whisper-homedir "/build/bin") t)
          (let ((cli-path (concat pr-whisper-homedir "/build/bin/whisper-cli")))
            (write-region "" nil cli-path)
            (set-file-modes cli-path #o644))  ; Not executable
          ;; Create models dir with model
          (make-directory (concat pr-whisper-homedir "/models") t)
          (write-region "" nil (concat pr-whisper-homedir "/models/" pr-whisper-model))
          (should-error (pr-whisper--validate-environment)
                        :type 'user-error))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-validate-server-not-executable ()
  "Test that validation fails when whisper-server exists but is not executable."
  (let ((pr-whisper-backend 'server)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create build/bin with non-executable whisper-server
          (make-directory (concat pr-whisper-homedir "/build/bin") t)
          (let ((server-path (concat pr-whisper-homedir "/build/bin/whisper-server")))
            (write-region "" nil server-path)
            (set-file-modes server-path #o644))  ; Not executable
          ;; Create models dir with model
          (make-directory (concat pr-whisper-homedir "/models") t)
          (write-region "" nil (concat pr-whisper-homedir "/models/" pr-whisper-model))
          (should-error (pr-whisper--validate-environment)
                        :type 'user-error))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-validate-success-cli ()
  "Test that validation succeeds with valid CLI setup."
  (let ((pr-whisper-backend 'cli)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create build/bin with executable whisper-cli
          (make-directory (concat pr-whisper-homedir "/build/bin") t)
          (let ((cli-path (concat pr-whisper-homedir "/build/bin/whisper-cli")))
            (write-region "#!/bin/sh\n" nil cli-path)
            (set-file-modes cli-path #o755))
          ;; Create models dir with model
          (make-directory (concat pr-whisper-homedir "/models") t)
          (write-region "" nil (concat pr-whisper-homedir "/models/" pr-whisper-model))
          ;; Should not error
          (should (eq nil (pr-whisper--validate-environment))))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-validate-success-server ()
  "Test that validation succeeds with valid server setup."
  (let ((pr-whisper-backend 'server)
        (pr-whisper-homedir (make-temp-file "whisper-test" t)))
    (unwind-protect
        (progn
          ;; Create build/bin with executable whisper-server
          (make-directory (concat pr-whisper-homedir "/build/bin") t)
          (let ((server-path (concat pr-whisper-homedir "/build/bin/whisper-server")))
            (write-region "#!/bin/sh\n" nil server-path)
            (set-file-modes server-path #o755))
          ;; Create models dir with model
          (make-directory (concat pr-whisper-homedir "/models") t)
          (write-region "" nil (concat pr-whisper-homedir "/models/" pr-whisper-model))
          ;; Should not error
          (should (eq nil (pr-whisper--validate-environment))))
      (delete-directory pr-whisper-homedir t))))

(ert-deftest pr-whisper-test-server-path ()
  "Test that server path is constructed correctly."
  (let ((pr-whisper-homedir "/path/to/whisper.cpp/"))
    (should (string= (pr-whisper--server-path)
                     "/path/to/whisper.cpp/build/bin/whisper-server")))
  (let ((pr-whisper-homedir "/path/to/whisper.cpp"))  ; No trailing slash
    (should (string= (pr-whisper--server-path)
                     "/path/to/whisper.cpp/build/bin/whisper-server"))))

(ert-deftest pr-whisper-test-cli-path ()
  "Test that CLI path is constructed correctly."
  (let ((pr-whisper-homedir "/path/to/whisper.cpp/"))
    (should (string= (pr-whisper--cli-path)
                     "/path/to/whisper.cpp/build/bin/whisper-cli")))
  (let ((pr-whisper-homedir "/path/to/whisper.cpp"))  ; No trailing slash
    (should (string= (pr-whisper--cli-path)
                     "/path/to/whisper.cpp/build/bin/whisper-cli"))))

(ert-deftest pr-whisper-test-noise-detection ()
  "Test that noise patterns are detected correctly."
  (should (pr-whisper--noise-p "(silence)"))
  (should (pr-whisper--noise-p "[silence]"))
  (should (pr-whisper--noise-p "(typing)"))
  (should (pr-whisper--noise-p "[music]"))
  (should (pr-whisper--noise-p "(applause)"))
  (should-not (pr-whisper--noise-p "Hello world"))
  (should-not (pr-whisper--noise-p "The silence was deafening")))

(ert-deftest pr-whisper-test-too-short-detection ()
  "Test that short transcriptions are detected."
  (let ((pr-whisper-history-min-length 3))
    (should (pr-whisper--too-short-p "ab"))
    (should (pr-whisper--too-short-p ""))
    (should-not (pr-whisper--too-short-p "abc"))
    (should-not (pr-whisper--too-short-p "hello"))))

(provide 'pr-whisper-test)

;;; pr-whisper-test.el ends here
