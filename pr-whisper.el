;;; pr-whisper.el --- Speech-to-text using Whisper.cpp -*- lexical-binding: t -*-

;; Copyright (C) 2025 Raoul Comninos, Pierre Rouleau

;; Author: Pierre Rouleau based on original work done by Raoul Comninos
;; Version: 0.0.2
;; Package-Version: 20251129.1236
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, speech, whisper, transcription
;; URL: https://github.com/emacselements/pr-whisper
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

;; Pr-Whisper provides simple speech-to-text transcription using
;; Whisper.cpp.  It records audio via sox and transcribes it using
;; Whisper models, inserting the transcribed text at your cursor.

;; Features:
;; - `pr-whisper-mode', a global minor mode that enables speech-to-text.
;;   While active, use C-c . to toggle recording on/off.
;; - `pr-whisper-transcribe-file' command to transcribe an existing WAV file.
;; - Custom vocabulary support for specialized terminology
;; - Async processing with process sentinels

;; Basic usage:
;;
;;  1. Enable the mode: M-x pr-whisper-mode
;;  2. Start recording: C-c .
;;  3. Stop recording and transcribe: C-c . (same key toggles)
;;
;; The mode can stay enabled - just use C-c . whenever you want to dictate.
;; Disabling the mode (M-x pr-whisper-mode again) stops any active recording.

;; See the README for installation and configuration details.

;;; Code:

(require 'ring)
(declare-function vterm-send-string "vterm")

(defgroup pr-whisper nil
  "Speech-to-text using Whisper.cpp system."
  :group 'convenience
  :link '(url-link :tag "pr-whisper @ Github"
                   "https://github.com/emacselements/pr-whisper"))

(defcustom pr-whisper-lighter-when-recording " 🎙️ "
  "Mode line lighter used by `pr-whisper-mode' when recording."
  :group 'pr-whisper
  :type 'string)

(defcustom pr-whisper-lighter-when-idle " ⏸️"
  "Mode line lighter used by `pr-whisper-mode' when idle."
  :group 'pr-whisper
  :type 'string)

(defcustom pr-whisper-key-for-toggle (kbd "C-c .")
  "Key binding for `pr-whisper-toggle-recording'.
Use a string of the same format that what is the output of `kbd'."
  :group 'pr-whisper
  :type 'key)

(defcustom pr-whisper-homedir "~/whisper.cpp/"
  "The whisper.cpp top directory."
  :group 'pr-whisper
  :type 'directory)

(defcustom pr-whisper-sox "sox"
  "The SoX Sound eXchange program name, with or without path.

The default is sox, without a path; you can use this unchanged if it
is installed in a directory in your PATH.  If it is stored somewhere else, then
include its directory path."
  :group 'pr-whisper
  :type 'string)

(defcustom pr-whisper-model "ggml-medium.en.bin"
  "Whisper model for accurate transcription mode.
This model is used by `pr-whisper-mode'.

The common options (all English) models:
- ggml-large-v3-turbo.bin: Best accuracy, slower
- ggml-medium.en.bin     : Good balance of speed and accuracy
- ggml-small.en.bin      : Faster than medium, less accurate
- ggml-base.en.bin       : Fastest, least accurate

See the ggerganov whisper.cpp models link for other models.
Store the models in the directory identified by `pr-whisper-homedir'."
  :group 'pr-whisper
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

(defun pr-whisper-model-desc (model)
  "Return a description string of specified MODEL, a string."
  (cond
   ((string= model "ggml-large-v3-turbo.bin") "Large model, best accuracy, slower")
   ((string= model "ggml-medium.en.bin")      "Medium model, balance speed/accuracy")
   ((string= model "ggml-small.en.bin")       "Small model, faster, less accurate" )
   ((string= model "ggml-base.en.bin")        "Base model, fastest, least accurate")
   (t model)))

(defun pr-whisper--cli-path ()
  "Return the path to the whisper-cli executable."
  (format "%s/build/bin/whisper-cli"
          (directory-file-name pr-whisper-homedir)))

(defun pr-whisper--model-path (&optional model)
  "Return the path to the whisper model file.
If MODEL is nil, use `pr-whisper-model'."
  (format "%s/models/%s"
          (directory-file-name pr-whisper-homedir)
          (or model pr-whisper-model)))

(defcustom pr-whisper-history-capacity 20
  "Maximum number of transcriptions to keep in history ring."
  :group 'pr-whisper
  :type 'integer)

(defcustom pr-whisper-noise-regexp
  (rx (or (seq "(" (or "typing" "silence" "music" "applause") ")")
          (seq "[" (or "typing" "silence" "music" "applause") "]")))
  "Regexp matching noise transcriptions to ignore.
Whisper outputs these when it detects non-speech audio.
Matching transcriptions are not inserted and not added to history.
Set to nil to disable noise filtering."
  :group 'pr-whisper
  :type '(choice (const :tag "No filtering" nil)
                 (regexp :tag "Filter regexp")))

(defcustom pr-whisper-history-min-length 3
  "Minimum character length for transcription to be added to history.
Transcriptions shorter than this are filtered out."
  :group 'pr-whisper
  :type 'integer)

(defcustom pr-whisper-vocabulary-file (expand-file-name
                                       (locate-user-emacs-file
                                        "whisper-vocabulary.txt"))
  "Path to file containing vocabulary hints for Whisper.
This file provides context to help Whisper recognize proper nouns,
technical terms, and specialized vocabulary.  Any text format works:
contextual sentences, comma-separated lists, or mixed formats.
See VOCABULARY-GUIDE.md for detailed guidance.

You can either customize this path or set it in your init.el:
  (setq pr-whisper-vocabulary-file \"/path/to/your/vocabulary.txt\")"
  :group 'pr-whisper
  :type 'file)

(defun pr-whisper--get-vocabulary-prompt ()
  "Read vocabulary file and return as a prompt string for Whisper.
Returns nil if file doesn't exist or is empty."
  (when (and pr-whisper-vocabulary-file
             (file-exists-p pr-whisper-vocabulary-file))
    (with-temp-buffer
      (insert-file-contents pr-whisper-vocabulary-file)
      (let ((content (string-trim (buffer-string))))
        (unless (string-empty-p content)
          content)))))

(defun pr-whisper--check-vocabulary-length ()
  "Check vocabulary file length and return word count.
Returns nil if file doesn't exist or is empty."
  (when (and pr-whisper-vocabulary-file
             (file-exists-p pr-whisper-vocabulary-file))
    (with-temp-buffer
      (insert-file-contents pr-whisper-vocabulary-file)
      (let* ((content (string-trim (buffer-string)))
             (word-count (length (split-string content))))
        (unless (string-empty-p content)
          word-count)))))

(defun pr-whisper--validate-environment (&optional model)
  "Validate current settings.
If MODEL is nil, use `pr-whisper-model'."
  (let ((cli-path (pr-whisper--cli-path))
        (model-path (pr-whisper--model-path model)))
    (unless (executable-find pr-whisper-sox)
      (user-error "The sox command is not accessible; is pr-whisper-sox valid?"))
    (unless (file-directory-p pr-whisper-homedir)
      (user-error "Invalid pr-whisper-homedir (%s)" pr-whisper-homedir))
    (unless (file-executable-p cli-path)
      (if (file-exists-p cli-path)
          (user-error "Whisper-cli (%s) is not an executable file!"
                      cli-path))
      (user-error "Whisper-cli (%s) does not exist!" cli-path))
    (unless (file-exists-p model-path)
      (user-error "Whisper model (%s) does not exist!"
                  model-path))))

(defun pr-whisper--start-message (model vocab-word-count)
  "Inform user recording is starting with MODEL, warn if vocabulary is too large.
The VOCAB-WORD-COUNT is the number of words detected in the vocabulary file."
  (if (and vocab-word-count (> vocab-word-count 150))
      (message "\
Recording starting with %s. Editing halted. Press C-g to stop.
WARNING: Vocabulary file has %d words (max: 150)!"
               (pr-whisper-model-desc model)
               vocab-word-count)
    (message "\
Recording starting with %s. Editing halted. Press C-g to stop."
             (pr-whisper-model-desc model))))

(defvar pr-whisper--recording-process-name nil
  "Nil when inactive, name of recording process when recording.")
(defvar pr-whisper--wav-file nil
  "Name of wave-file used during mode execution.")
(defvar pr-whisper--history-ring nil
  "Ring of recent transcriptions.
Each entry is a cons cell (TEXT . BUFFER-NAME).
Entries are promoted to most recent when re-inserted via
`pr-whisper-insert-from-history', implementing LRU-style eviction.")

(defun pr-whisper--set-lighter-to (lighter)
  "Update pr-whisper lighter in the mode lines of all buffers to LIGHTER."
  (setcar (cdr (assq 'pr-whisper-mode minor-mode-alist)) lighter)
  ;; force update of all modelines
  (force-mode-line-update t))

(defun pr-whisper-record-audio ()
  "Record audio, store it in the specified WAV-FILE."
  ;; Start recording audio.
  ;; Use the sox command. Ref: https://sourceforge.net/projects/sox/
  ;;  -d : record audio
  ;;  -r sample-rate in Hz
  ;;  -c channel : number of channel audio in the file.  Use 1.
  ;;  -b bits: bit-length of each encoded sample.
  ;;  --no-show-progress : do not print a progress bar in stdout.
  (let ((record-process-name (format "pr-whisper-record-audio-for-%s" (emacs-pid))))
    (start-process record-process-name
                   nil pr-whisper-sox
                   "-d" "-r" " 16000" "-c" "1" "-b" "16"
                   pr-whisper--wav-file
                   "--no-show-progress")
    (setq pr-whisper--recording-process-name record-process-name)
    (pr-whisper--set-lighter-to pr-whisper-lighter-when-recording)
    (message "Recording audio!")))

(defun pr-whisper--noise-p (text)
  "Return non-nil if TEXT is noise that should be ignored."
  (and pr-whisper-noise-regexp
       (string-match-p pr-whisper-noise-regexp text)))

(defun pr-whisper--too-short-p (text)
  "Return non-nil if TEXT is too short for history."
  (< (length text) pr-whisper-history-min-length))

(defun pr-whisper--add-to-history (text buffer-name)
  "Add TEXT with BUFFER-NAME to the history ring.
TEXT is excluded if it matches `pr-whisper-noise-regexp' or is
shorter than `pr-whisper-history-min-length'."
  (unless (or (pr-whisper--noise-p text)
              (pr-whisper--too-short-p text))
    (unless pr-whisper--history-ring
      (setq pr-whisper--history-ring (make-ring pr-whisper-history-capacity)))
    (ring-insert pr-whisper--history-ring (cons text buffer-name))))

(defun pr-whisper--transcribe ()
  "Transcribe audio previously recorded."
  (let* ((model pr-whisper-model)
         (wav-file pr-whisper--wav-file)
         (original-buf (current-buffer))
         (original-point (point-marker)) ; Marker tracks position even if buffer changes
         (vocab-prompt (pr-whisper--get-vocabulary-prompt))
         (temp-buf (generate-new-buffer " *Whisper Temp*"))

         (whisper-cmd-list (list (expand-file-name (pr-whisper--cli-path))
                                 "-m" (expand-file-name (pr-whisper--model-path model))
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
     :stderr (get-buffer-create "*pr-whisper err*")
     :sentinel (lambda (_proc event)
                 (if (string= event "finished\n")
                     (when (buffer-live-p temp-buf)
                       ;; Trim excess white space
                       (let ((output (string-trim
                                      (with-current-buffer temp-buf
                                        (buffer-string)))))
                         (cond
                          ((string-empty-p output)
                           (message "Whisper: No transcription output."))
                          ((pr-whisper--noise-p output)
                           (message "Whisper: Ignored noise: %s" output))
                          (t
                           ;; Add to history first, before attempting insertion
                           ;; so transcription is saved even if insertion fails
                           (pr-whisper--add-to-history output (buffer-name original-buf))
                           (when (buffer-live-p original-buf)
                             (with-current-buffer original-buf
                               (condition-case nil
                                   (if (eq major-mode 'vterm-mode)
                                       (vterm-send-string (concat output " "))
                                     (goto-char original-point)
                                     ;; Insert text, then a single space
                                     (insert output " "))
                                 (buffer-read-only
                                  (message "Whisper: Buffer is read-only, text saved to history: %s"
                                           (truncate-string-to-width output 50 nil nil "...")))))))))
                       ;; Clean up temporary buffer
                       (kill-buffer temp-buf)
                       ;; And delete WAV file that has been processed.
                       (when (file-exists-p wav-file)
                         (delete-file wav-file)))
                   ;; No detection of end: error!
                   (message "Whisper process error: %s" event))))))

;;;###autoload
(defun pr-whisper-stop-record ()
  "Stop recording, insert transcribed text at point."
  (interactive)
  (unless pr-whisper--recording-process-name
    (user-error "Not currently recording"))
  (interrupt-process pr-whisper--recording-process-name)
  (setq pr-whisper--recording-process-name nil)
  (message "Audio recording stopped.")
  (pr-whisper--set-lighter-to pr-whisper-lighter-when-idle)
  (pr-whisper--transcribe))

;;;###autoload
(defun pr-whisper-toggle-recording ()
  "Toggle recording on/off."
  (interactive)
  (if pr-whisper--recording-process-name
      (pr-whisper-stop-record)
    (let ((model pr-whisper-model)
          (vocab-word-count (pr-whisper--check-vocabulary-length)))
      (pr-whisper--validate-environment model)
      (pr-whisper--start-message model vocab-word-count)
      (pr-whisper-record-audio))))

(defvar pr-whisper-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map pr-whisper-key-for-toggle #'pr-whisper-toggle-recording)
    map))

;;;###autoload
(define-minor-mode pr-whisper-mode
  "Minor mode to transcribe speech to text.
When activated, enables recording controls but does not start recording.
Use \\[pr-whisper-toggle-recording] to start/stop recording.
When recording stops, transcribed text is inserted at point.

\\{pr-whisper-keymap}"
  :lighter pr-whisper-lighter-when-idle
  :keymap pr-whisper-keymap
  :global t
  (if pr-whisper-mode
      ;; Start minor mode: set up wav file path, don't start recording yet
      (let ((wav-file (format "/tmp/whisper-recording-%s.wav" (emacs-pid))))
        (setq pr-whisper--wav-file wav-file)
        (pr-whisper--set-lighter-to pr-whisper-lighter-when-idle)
        (message "pr-whisper-mode enabled. Press %s to start recording."
                 (key-description pr-whisper-key-for-toggle)))

    ;; Stop minor mode: stop recording if active
    (when pr-whisper--recording-process-name
      (pr-whisper-stop-record))
    (setq pr-whisper--wav-file nil)))

;;;###autoload
(defun pr-whisper-transcribe-file (fname)
  "Transcribe a recorded audio file FNAME, insert transcribed text at point.
This command can only be used when `pr-whisper-mode is inactive."
  (interactive "fWAV file to transcribe: ")
  ;; Validate requirements first
  (if pr-whisper-mode
      (user-error "Cannot use this command while pr-whisper-mode is active!"))
  (unless (file-exists-p fname)
    (user-error "Specified file does not exist: %s" fname))
  (pr-whisper--validate-environment)

  ;; All is OK, transcribe the file.
  ;; Set the name of the file
  (setq pr-whisper--wav-file fname)
  (pr-whisper--transcribe)
  (setq pr-whisper--wav-file nil))

;;;###autoload
(defun pr-whisper-insert-from-history ()
  "Insert a previous transcription from history.
Prompts with completing-read showing transcriptions with their source buffer.

When a transcription is selected, it is promoted to the most recent
position in the history ring, making it less likely to be evicted
when the ring reaches capacity."
  (interactive)
  (unless (and pr-whisper--history-ring
               (not (ring-empty-p pr-whisper--history-ring)))
    (user-error "No transcription history"))
  (let* ((entries (ring-elements pr-whisper--history-ring))
         (candidates (mapcar (lambda (entry)
                               (let ((text (car entry))
                                     (buf-name (cdr entry)))
                                 ;; Format: truncated-text [buffer-name] -> entry
                                 (cons (format "%s  [%s]"
                                               (truncate-string-to-width text 60 nil nil "...")
                                               buf-name)
                                       entry)))
                             entries))
         (choice (completing-read "Insert transcription: " candidates nil t))
         (entry (cdr (assoc choice candidates))))
    (when entry
      ;; Promote entry to most recent position (LRU behavior)
      (let ((idx (ring-member pr-whisper--history-ring entry)))
        (when idx
          (ring-remove pr-whisper--history-ring idx)
          (ring-insert pr-whisper--history-ring entry)))
      (insert (car entry) " "))))

;; ---------------------------------------------------------------------------
(provide 'pr-whisper)

;; Local variables:
;; time-stamp-format: "%Y%02m%02d.%02H%02M"
;; time-stamp-start: "Package-Version:[ \t]+\\\\?"
;; time-stamp-end: "\n"
;; time-stamp-line-limit: 15
;; End:

;;; pr-whisper.el ends here
