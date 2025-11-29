;;; my-whisper.el --- Speech-to-text using Whisper.cpp -*- lexical-binding: t -*-

;; Copyright (C) 2025 Raoul Comninos, Pierre Rouleau

;; Author: Pierre Rouleau based on original work done by Raoul Comninos
;; Version: 0.0.2
;; Package-Version: 20251129.1057
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, speech, whisper, transcription
;; URL: https://github.com/emacselements/my-whisper
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

;; My-Whisper provides simple speech-to-text transcription using
;; Whisper.cpp.  It records audio via sox and transcribes it using
;; Whisper models, inserting the transcribed text at your cursor.

;; Features:
;; - `my-whisper-mode', a global minor mode that records audio and insert
;;   transcribed text in buffer.
;; - Several transcription modes are supported by customization
;; - Custom vocabulary support for specialized terminology
;; - Automatic vocabulary length validation
;; - Async processing with process sentinels
;; - Clean temporary file management

;; Basic usage:
;;
;;  - Activate whisper-mode with: M-x my-whisper-mode
;;    - This starts audio recording store in a WAV file.
;;  - Stop recording and insert transcribed text by one of the following:
;;    - C-c g  : to stop, insert transcribed text but keep mode active.
;;      - You can start recording again with: C-c r
;;    - M-x my-whisper-mode : stop recording, insert transcribed text and
;;                            stop the mode.
;;
;; The `my-whisper-mode' is a global minor mode; recording audio does not halt
;; Emacs and you can execute any other Emacs operation.
;;
;; Text is inserted inside the buffer that is active at the moment you stop
;; recording (or stop the minor mode)
;;
;; Recommended keybindings to toggle `my-whisper-mode' to add to your init.el
;; file:
;;
;;   (global-set-key (kbd "C-c w") #'my-whisper-mode)

;; See the README for installation and configuration details.

;;; Code:

(defgroup my-whisper nil
  "Speech-to-text using Whisper.cpp system."
  :group 'convenience
  :link '(url-link :tag "my-whisper @ Github"
                   "https://github.com/emacselements/my-whisper"))

(defcustom my-whisper-lighter-when-recording " 🎙️ "
  "Mode line lighter used by `my-whisper-mode' when recording."
  :group 'my-whisper
  :type 'string)

(defcustom my-whisper-lighter-when-idle " ⏸️"
  "Mode line lighter used by `my-whisper-mode' when idle."
  :group 'my-whisper
  :type 'string)

(defcustom my-whisper-key-for-stop-record (kbd "C-c .")
  "Key binding for `my-whisper-stop-record'.
Use a string of the same format that what is the output of `kbd'."
  :group 'my-whisper
  :type 'key)

(defcustom my-whisper-key-for-record-again (kbd "C-c ,")
  "Key binding for `my-whisperrecord-again'.
Use a string of the same format that what is the output of `kbd'."
  :group 'my-whisper
  :type 'key)

(defcustom my-whisper-homedir "~/whisper.cpp/"
  "The whisper.cpp top directory."
  :group 'my-whisper
  :type 'directory)

(defcustom my-whisper-sox "sox"
  "The SoX Sound eXchange program name, with or without path.

The default is sox, without a path; you can use this unchanged if it
is installed in a directory in your PATH.  If it is stored somewhere else, then
include its directory path."
  :group 'my-whisper
  :type 'string)

(defcustom my-whisper-model "ggml-medium.en.bin"
  "Whisper model for accurate transcription mode.
This model is used by `my-whisper-mode'.

The common options (all English) models:
- ggml-large-v3-turbo.bin: Best accuracy, slower
- ggml-medium.en.bin     : Good balance of speed and accuracy
- ggml-small.en.bin      : Faster than medium, less accurate
- ggml-base.en.bin       : Fastest, least accurate

See the ggerganov whisper.cpp models link for other models.
Store the models in the directory identified by `my-whisper-homedir'."
  :group 'my-whisper
  :link '(url-link :tag "ggerganov whisper.cpp models"
                   "https://huggingface.co/ggerganov/whisper.cpp/tree/main")
  :link '(url-link :tag "whisper.cpp @ Github"
                   "https://github.com/ggml-org/whisper.cpp")
  :type '(choice
          (const :tag "Large  model, best accuracy, slower  " "ggml-large-v3-turbo.bin")
          (const :tag "Medium model, balance speed/accuracy " "ggml-medium.en.bin")
          (const :tag "Small  model, faster, less accurate  " "ggml-small.en.bin")
          (const :tag "Base   model, fastest, least accurate" "ggml-base.en.bin")
          (string :tag "Other model")))

(defun my-whisper-model-desc (model)
  "Return a description string of specified MODEL, a string."
  (cond
   ((string= model "ggml-large-v3-turbo.bin") "Large model, best accuracy, slower")
   ((string= model "ggml-medium.en.bin")      "Medium model, balance speed/accuracy")
   ((string= model "ggml-small.en.bin")       "Small model, faster, less accurate" )
   ((string= model "ggml-base.en.bin")        "Base model, fastest, least accurate")
   (t model)))

(defun my-whisper--cli-path ()
  "Return the path to the whisper-cli executable."
  (format "%s/build/bin/whisper-cli"
          (directory-file-name my-whisper-homedir)))

(defun my-whisper--model-path (&optional model)
  "Return the path to the whisper model file.
If MODEL is nil, use `my-whisper-model'."
  (format "%s/models/%s"
          (directory-file-name my-whisper-homedir)
          (or model my-whisper-model)))

(defcustom my-whisper-vocabulary-file (expand-file-name
                                       (locate-user-emacs-file
                                        "whisper-vocabulary.txt"))
  "Path to file containing vocabulary hints for Whisper.
This should contain proper nouns, specialized terms, etc.
The file should contain comma-separated words/phrases that Whisper
should recognize.

You can either customize this path or set it in your init.el:
  (setq my-whisper-vocabulary-file \"/path/to/your/vocabulary.txt\")"
  :group 'my-whisper
  :type 'file)

(defun my-whisper--get-vocabulary-prompt ()
  "Read vocabulary file and return as a prompt string for Whisper.
Returns nil if file doesn't exist or is empty."
  (when (and my-whisper-vocabulary-file
             (file-exists-p my-whisper-vocabulary-file))
    (with-temp-buffer
      (insert-file-contents my-whisper-vocabulary-file)
      (let ((content (string-trim (buffer-string))))
        (unless (string-empty-p content)
          content)))))

(defun my-whisper--check-vocabulary-length ()
  "Check vocabulary file length and return word count.
Returns nil if file doesn't exist or is empty."
  (when (and my-whisper-vocabulary-file
             (file-exists-p my-whisper-vocabulary-file))
    (with-temp-buffer
      (insert-file-contents my-whisper-vocabulary-file)
      (let* ((content (string-trim (buffer-string)))
             (word-count (length (split-string content))))
        (unless (string-empty-p content)
          word-count)))))

(defun my-whisper--validate-environment (&optional model)
  "Validate current settings.
If MODEL is nil, use `my-whisper-model'."
  (let ((cli-path (my-whisper--cli-path))
        (model-path (my-whisper--model-path model)))
    (unless (executable-find my-whisper-sox)
      (user-error "The sox command is not accessible; is my-whisper-sox valid?"))
    (unless (file-directory-p my-whisper-homedir)
      (user-error "Invalid my-whisper-homedir (%s)" my-whisper-homedir))
    (unless (file-executable-p cli-path)
      (if (file-exists-p cli-path)
          (user-error "My-whisper-cli (%s) is not an executable file!"
                      cli-path))
      (user-error "My-whisper-cli (%s) does not exist!" cli-path))
    (unless (file-exists-p model-path)
      (user-error "My-whisper-model-path (%s) does not exist!"
                  model-path))))

(defun my-whisper--start-message (model vocab-word-count)
  "Inform user recording is starting with MODEL, warn if vocabulary is too large.
The VOCAB-WORD-COUNT is the number of words detected in the vocabulary file."
  (if (and vocab-word-count (> vocab-word-count 150))
      (message "\
Recording starting with %s. Editing halted. Press C-g to stop.
WARNING: Vocabulary file has %d words (max: 150)!"
               (my-whisper-model-desc model)
               vocab-word-count)
    (message "\
Recording starting with %s. Editing halted. Press C-g to stop."
             (my-whisper-model-desc model))))

(defvar my-whisper--recording-process-name nil
  "Nil when inactive, name of recording process when recording.")
(defvar my-whisper--wav-file nil
  "Name of wave-file used during mode execution.")

(defun my-whisper--set-lighter-to (lighter)
  "Update my-whisper lighter in the mode lines of all buffers to LIGHTER."
  (setcar (cdr (assq 'my-whisper-mode minor-mode-alist)) lighter)
  ;; force update of all modelines
  (force-mode-line-update t))

(defun my-whisper-record-audio ()
  "Record audio, store it in the specified WAV-FILE."
  ;; Start recording audio.
  ;; Use the sox command. Ref: https://sourceforge.net/projects/sox/
  ;;  -d : record audio
  ;;  -r sample-rate in Hz
  ;;  -c channel : number of channel audio in the file.  Use 1.
  ;;  -b bits: bit-length of each encoded sample.
  ;;  --no-show-progress : do not print a progress bar in stdout.
  (let ((record-process-name (format "my-whisper-record-audio-for-%s" (emacs-pid))))
    (start-process record-process-name
                   nil my-whisper-sox
                   "-d" "-r" " 16000" "-c" "1" "-b" "16"
                   my-whisper--wav-file
                   "--no-show-progress")
    (setq my-whisper--recording-process-name record-process-name)
    (my-whisper--set-lighter-to my-whisper-lighter-when-recording)
    (message "Recording audio!")))

(defun my-whisper--transcribe ()
  "Transcribe audio previously recorded."
  (let* ((model my-whisper-model)
         (wav-file my-whisper--wav-file)
         (original-buf (current-buffer))
         (original-point (point-marker)) ; Marker tracks position even if buffer changes
         (vocab-prompt (my-whisper--get-vocabulary-prompt))
         (temp-buf (generate-new-buffer " *Whisper Temp*"))

         (whisper-cmd-list (list (expand-file-name (my-whisper--cli-path))
                                 "-m" (expand-file-name (my-whisper--model-path model))
                                 "-f" (expand-file-name wav-file)
                                 "-nt"
                                 "-np")))
    ;; if a vocabulary is defined, add a prompt command with it.
    (when vocab-prompt
      (setq whisper-cmd-list (reverse whisper-cmd-list))
      (push "--prompt" whisper-cmd-list)
      (push (replace-regexp-in-string "\"" "\\\\\"" vocab-prompt) whisper-cmd-list)
      (setq whisper-cmd-list (reverse whisper-cmd-list)))

    ;; Run Whisper STT (Speech To Text) with selected model
    (make-process
     :name "whisper-stt"
     :buffer temp-buf
     :command whisper-cmd-list
     :connection-type 'pipe
     :stderr (get-buffer-create "*my-whisper err*")
     :sentinel (lambda (_proc event)
                 (if (string= event "finished\n")
                     (when (buffer-live-p temp-buf)
                       ;; Trim excess white space
                       (let ((output (string-trim
                                      (with-current-buffer temp-buf
                                        (buffer-string)))))
                         (if (string-empty-p output)
                             (message "Whisper: No transcription output.")
                           (when (buffer-live-p original-buf)
                             (with-current-buffer original-buf
                               (goto-char original-point)
                               ;; Insert text, then a single space
                               (insert output " ")))))
                       ;; Clean up temporary buffer
                       (kill-buffer temp-buf)
                       ;; And delete WAV file that has been processed.
                       (when (file-exists-p wav-file)
                         (delete-file wav-file)))
                   ;; No detection of end: error!
                   (message "Whisper process error: %s" event))))))

;;;###autoload
(defun my-whisper-stop-record ()
  "Stop recording, insert transcribed text at point."
  (interactive)
  (interrupt-process my-whisper--recording-process-name)
  (setq my-whisper--recording-process-name nil)
  (message "Audio recording stopped.")
  (my-whisper--set-lighter-to my-whisper-lighter-when-idle)
  (my-whisper--transcribe))

;;;###autoload
(defun my-whisper-record-again ()
  "Start recording again."
  (interactive)
  (if my-whisper--recording-process-name
      (user-error "Already recording!")
    (my-whisper-record-audio)))

(defvar my-whisper-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map my-whisper-key-for-stop-record #'my-whisper-stop-record)
    (define-key map my-whisper-key-for-record-again #'my-whisper-record-again)
    map))

;;;###autoload
(define-minor-mode my-whisper-mode
  "Minor mode to transcribe speech to text.
When activate, start recording.
When stopped, stops recording and insert transcribed text in current
buffer.

\\{my-whisper-keymap}"
  :lighter my-whisper-lighter-when-recording
  :keymap my-whisper-keymap
  :global t
  (let ((model my-whisper-model))
    (my-whisper--validate-environment model)
    (let ((wav-file (format "/tmp/whisper-recording-%s.wav" (emacs-pid)))
          (vocab-word-count (my-whisper--check-vocabulary-length)) )
      (if my-whisper-mode
          ;; Start minor mode: start recording
          (progn
            ;; Inform user recording is starting. Warn if vocabulary is too large.
            (my-whisper--start-message model vocab-word-count)
            ;; Record audio in the specified wav-file
            (setq my-whisper--wav-file wav-file)
            (my-whisper-record-audio))

        ;; Stop minor mode: stop recording and insert transcribed text
        (when my-whisper--recording-process-name
          (my-whisper-stop-record)
          (my-whisper--transcribe)
          (setq my-whisper--wav-file nil))))))


(provide 'my-whisper)

;; Local variables:
;; time-stamp-format: "%Y%02m%02d.%02H%02M"
;; time-stamp-start: "Package-Version:[ \t]+\\\\?"
;; time-stamp-end: "\n"
;; time-stamp-line-limit: 15
;; End:

;;; my-whisper.el ends here
