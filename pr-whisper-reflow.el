;;; pr-whisper-reflow.el --- Reflow transcriptions using LLM -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.1") (gptel "0.9.9"))

;;; Commentary:
;; Use gptel to reflow whisper transcriptions into logical paragraphs.
;; Requires gptel >= 0.9.9 (for inline preset specs in
;; `gptel-with-preset').
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

(require 'gptel)
(declare-function pr-whisper-default-insert "pr-whisper")

(defcustom pr-whisper-reflow-prompt
  "Reflow this transcription into logical paragraphs. Rules:
1. Replace spoken commands like \"new paragraph\", \"new line\" with actual line breaks
2. Add paragraph breaks at natural topic transitions
3. Keep ALL words exactly as transcribed - do not correct, rephrase, or summarize
4. Output one header with a 7 word max summary of the topic with format \"re: <header>\n\n\" then only the reflowed text, nothing else

Transcription:
%s"
  "Prompt template for reflowing transcriptions.
%s is replaced with the transcription text."
  :type 'string
  :group 'pr-whisper)

(defcustom pr-whisper-reflow-backend "Gemini"
  "gptel backend name for reflow.
Must match a backend registered with gptel (e.g. \"Gemini\",
\"ChatGPT\", \"Claude\")."
  :type 'string
  :group 'pr-whisper)

(defcustom pr-whisper-reflow-model 'gemini-2.5-flash-lite
  "Model to use for reflow.
Must be a symbol matching a model in `pr-whisper-reflow-backend'.
List available models with:
  (gptel-backend-models (gptel-get-backend pr-whisper-reflow-backend))"
  :type 'symbol
  :group 'pr-whisper)

(defcustom pr-whisper-reflow-min-length 100
  "Minimum text length in characters to trigger reflow.
Transcriptions shorter than this are inserted directly without LLM reflow."
  :type 'natnum
  :group 'pr-whisper)

(defun pr-whisper-reflow-default-p (text _marker)
  "Return non-nil if TEXT is long enough for reflow."
  (>= (length text) pr-whisper-reflow-min-length))

(defcustom pr-whisper-reflow-predicate #'pr-whisper-reflow-default-p
  "Predicate to decide whether to reflow a transcription.
Called with TEXT and MARKER.  Return non-nil to reflow."
  :type 'function
  :group 'pr-whisper)

(defun pr-whisper-reflow-insert (text marker)
  "Insert function for `pr-whisper-insert-function'.
Reflows TEXT via LLM and inserts at MARKER when complete.
Calls `pr-whisper-reflow-predicate' to decide whether to reflow;
otherwise uses default insertion."
  (if (funcall pr-whisper-reflow-predicate text marker)
      ;; The buffer's default-directory may no longer exist (e.g. a
      ;; deleted temp dir), which breaks process invocation in gptel.
      (let ((default-directory temporary-file-directory))
        (gptel-with-preset `(:backend ,pr-whisper-reflow-backend
                            :model ,pr-whisper-reflow-model)
          (message "Reflowing transcription...")
          (gptel-request
           (format pr-whisper-reflow-prompt text)
           :callback (lambda (response _info)
                       (if (stringp response)
                           (progn
                             (pr-whisper-default-insert
                              (string-trim response) marker)
                             (message "Reflow complete."))
                         (pr-whisper-default-insert text marker)
                         (message "Reflow failed; inserted original transcription."))))))
    (pr-whisper-default-insert text marker)))

(provide 'pr-whisper-reflow)

;;; pr-whisper-reflow.el ends here
