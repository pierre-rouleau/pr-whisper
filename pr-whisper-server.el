;;; pr-whisper-server.el --- Server backend for pr-whisper -*- lexical-binding: t -*-

;; Copyright (C) 2025 Raoul Comninos, Pierre Rouleau

;; Author: Pierre Rouleau based on original work done by Raoul Comninos
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; HTTP server backend for pr-whisper using whisper-server.
;; Loaded automatically via autoload when pr-whisper-backend is 'server.

;;; Code:

(require 'pr-whisper)
(require 'url)

(defvar pr-whisper--server-process nil
  "Process handle for whisper-server when using server backend.")

(defun pr-whisper--server-path ()
  "Return path to whisper-server executable."
  (format "%s/build/bin/whisper-server"
          (directory-file-name pr-whisper-homedir)))

(defun pr-whisper--start-server ()
  "Start whisper-server in background for transcription."
  (when (and pr-whisper--server-process
             (process-live-p pr-whisper--server-process))
    (kill-process pr-whisper--server-process))
  (setq pr-whisper--server-process
        (start-process "whisper-server" nil
                       (pr-whisper--server-path)
                       "-m" (expand-file-name (pr-whisper--model-path pr-whisper-model))
                       "--port" (number-to-string pr-whisper-server-port))))

(defun pr-whisper--stop-server ()
  "Stop whisper-server if running."
  (when (and pr-whisper--server-process
             (process-live-p pr-whisper--server-process))
    (kill-process pr-whisper--server-process)
    (setq pr-whisper--server-process nil)))

(defun pr-whisper--server-ready-p ()
  "Check if whisper-server is ready to accept requests."
  (condition-case nil
      (let ((url-request-method "GET"))
        (with-current-buffer
            (url-retrieve-synchronously
             (format "http://localhost:%d/health" pr-whisper-server-port)
             t nil 0.1)
          (prog1 t (kill-buffer))))
    (error nil)))

(defun pr-whisper--wait-for-server (timeout)
  "Wait up to TIMEOUT seconds for server to be ready."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (< (float-time) deadline)
                (not (pr-whisper--server-ready-p)))
      (sleep-for 0.1))))

(defun pr-whisper--make-multipart-body (wav-file boundary)
  "Create multipart/form-data body for WAV-FILE upload with BOUNDARY."
  (let ((file-content (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally wav-file)
                        (buffer-string))))
    (concat
     "--" boundary "\r\n"
     "Content-Disposition: form-data; name=\"file\"; filename=\""
     (file-name-nondirectory wav-file) "\"\r\n"
     "Content-Type: audio/wav\r\n\r\n"
     file-content "\r\n"
     "--" boundary "\r\n"
     "Content-Disposition: form-data; name=\"response_format\"\r\n\r\n"
     "text\r\n"
     "--" boundary "--\r\n")))

(defun pr-whisper--transcribe-via-server (wav-file &optional use-default-insert)
  "Transcribe WAV-FILE using whisper-server HTTP API.
If USE-DEFAULT-INSERT is non-nil, bypass custom insert function."
  (let* ((url (format "http://localhost:%d/inference" pr-whisper-server-port))
         (boundary (format "----EmacsFormBoundary%d" (random 1000000000)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))))
         (url-request-data (pr-whisper--make-multipart-body wav-file boundary)))
    ;; Wait for server to be ready (should already be warm from recording time)
    (pr-whisper--wait-for-server 5)
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "Whisper server error: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "\r?\n\r?\n" nil t)  ; Skip HTTP headers
         (pr-whisper--handle-transcription
          (buffer-substring (point) (point-max))
          use-default-insert))
       ;; Cleanup
       (kill-buffer)
       (when (file-exists-p wav-file)
         (delete-file wav-file))
       ;; Stop server after transcription
       (pr-whisper--stop-server))
     nil t)))

(provide 'pr-whisper-server)
;;; pr-whisper-server.el ends here
