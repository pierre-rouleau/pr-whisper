;;; pr-whisper-reflow.el --- Reflow transcriptions using LLM -*- lexical-binding: t -*-

;;; Commentary:
;; Use gptel to reflow whisper transcriptions into logical paragraphs.
;;
;; Why not a local model? Reflowing requires understanding enough English
;; context to identify sentence boundaries, topic transitions, and spoken
;; commands like "new paragraph". Small local models (e.g., Qwen 1.5B,
;; Llama 3B) lack the comprehension needed for reliable results. This
;; module uses Google's TPU-based Gemini Flash models as a compromise:
;; fast response times with sufficient language understanding.
;;
;; To use as insert function, add to your init.el:
;;   (require 'pr-whisper-reflow)
;;   (setq pr-whisper-insert-function #'pr-whisper-reflow-insert)

;;; Code:

(defvar gptel-model)
(declare-function gptel-request "gptel")
(declare-function pr-whisper-default-insert "pr-whisper")

(defvar pr-whisper-reflow-prompt
  "Reflow this transcription into logical paragraphs. Rules:
1. Replace spoken commands like \"new paragraph\", \"new line\" with actual line breaks
2. Add paragraph breaks at natural topic transitions
3. Keep ALL words exactly as transcribed - do not correct, rephrase, or summarize
4. Output one header with a 7 word max summary of the topic with format \"re: <header>\n\n\" then only the reflowed text, nothing else

Transcription:
%s"
  "Prompt template for reflowing transcriptions.
%s is replaced with the transcription text.")

(defcustom pr-whisper-reflow-model "gemini-2.5-flash-lite"
  "Model to use for reflow.
Examples: \"gemini-2.0-flash-lite\", \"qwen2.5:1.5b\" (Ollama)."
  :type 'string
  :group 'pr-whisper)

(defun pr-whisper-reflow--claude-code-buffer-p (buf)
  "Return non-nil if BUF is a claude-code buffer."
  (and buf
       (with-current-buffer buf (derived-mode-p 'vterm-mode))
       (string-match-p "\\`\\*claude-code\\[" (buffer-name buf))))

(defun pr-whisper-reflow-insert (text marker)
  "Insert function for `pr-whisper-insert-function'.
Reflows TEXT via LLM and inserts at MARKER when complete.
Only reflows when MARKER's buffer is a claude-code buffer;
otherwise uses default insertion."
  (let ((buf (marker-buffer marker)))
    (if (pr-whisper-reflow--claude-code-buffer-p buf)
        (let ((gptel-model pr-whisper-reflow-model))
          (message "Reflowing transcription...")
          (gptel-request
           (format pr-whisper-reflow-prompt text)
           :callback (lambda (response _info)
                       (pr-whisper-default-insert
                        (if response (string-trim response) text)
                        marker)
                       (message "Reflow complete."))))
      (pr-whisper-default-insert text marker))))

(provide 'pr-whisper-reflow)

;;; pr-whisper-reflow.el ends here
