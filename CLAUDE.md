# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

pr-whisper is an Emacs package providing speech-to-text functionality using whisper.cpp. It records audio via sox,
transcribes using Whisper models, and inserts text at the cursor position.

## Build & Test

Byte-compile with warnings as errors:
```bash
./build
```

Or manually:
```bash
emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile pr-whisper.el
```

Run ERT tests:
```bash
emacs -Q --batch -L . -l pr-whisper-test.el -f ert-run-tests-batch-and-exit
```

## Architecture

Single-file Emacs Lisp package (`pr-whisper.el`) with these key components:

- **`pr-whisper-mode`**: Global minor mode that enables recording controls (C-c . to toggle)
- **Recording**: Uses sox to record 16kHz mono 16-bit WAV files to `/tmp/whisper-recording-<pid>.wav`
- **Transcription backends**:
  - `cli` (default): Async process calling `whisper-cli` with process sentinel
  - `server`: HTTP API via `whisper-server` (faster, server starts during recording to warm up)
- **Vocabulary hints**: Optional `--prompt` argument passed to whisper-cli for better recognition of proper nouns
- **History ring**: Stores recent transcriptions with LRU-style eviction on re-use

### Key Functions

- `pr-whisper-toggle-recording`: Start/stop recording (bound to C-c . in mode)
- `pr-whisper-record-audio`: Starts sox recording process
- `pr-whisper-stop-record`: Interrupts recording, triggers transcription
- `pr-whisper--transcribe`: Dispatches to CLI or server backend based on `pr-whisper-backend`
- `pr-whisper--transcribe-via-cli`: Async whisper-cli call with sentinel
- `pr-whisper--transcribe-via-server`: HTTP POST to whisper-server using url.el
- `pr-whisper--validate-environment`: Checks sox, whisper-cli/server, and model paths before operations
- `pr-whisper-transcribe-file`: Transcribes an existing WAV file without recording
- `pr-whisper-insert-from-history`: Browse and re-insert previous transcriptions

### Customization Variables

- `pr-whisper-homedir`: whisper.cpp installation directory (default: `~/whisper.cpp/`)
- `pr-whisper-model`: Model file name (e.g., `ggml-medium.en.bin`)
- `pr-whisper-backend`: Transcription backend - `cli` (default) or `server`
- `pr-whisper-server-port`: Port for whisper-server (default: 8178)
- `pr-whisper-vocabulary-file`: Path to vocabulary hints file
- `pr-whisper-sox`: sox executable path
- `pr-whisper-key-for-toggle`: Key binding for toggle (default: `C-c .`)
- `pr-whisper-history-capacity`: Max transcriptions in history ring (default: 20)

## External Dependencies

- **sox**: Audio recording (`sox -d -r 16000 -c 1 -b 16 output.wav`)
- **whisper.cpp**:
  - CLI backend: `pr-whisper-homedir/build/bin/whisper-cli`
  - Server backend: `pr-whisper-homedir/build/bin/whisper-server`
- **Models**: Located at `pr-whisper-homedir/models/<model-name>`
