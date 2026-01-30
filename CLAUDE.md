# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

my-whisper is an Emacs package providing speech-to-text functionality using whisper.cpp. It records audio via sox,
transcribes using Whisper models, and inserts text at the cursor position.

## Build & Lint

Byte-compile with warnings as errors:
```bash
./build
```

Or manually:
```bash
emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile my-whisper.el
```

## Architecture

Single-file Emacs Lisp package (`my-whisper.el`) with these key components:

- **`my-whisper-mode`**: Global minor mode that enables recording controls (C-c . to toggle)
- **Recording**: Uses sox to record 16kHz mono 16-bit WAV files to `/tmp/whisper-recording-<pid>.wav`
- **Transcription**: Async process calling `whisper-cli` with process sentinel for completion handling
- **Vocabulary hints**: Optional `--prompt` argument passed to whisper-cli for better recognition of proper nouns
- **History ring**: Stores recent transcriptions with LRU-style eviction on re-use

### Key Functions

- `my-whisper-toggle-recording`: Start/stop recording (bound to C-c . in mode)
- `my-whisper-record-audio`: Starts sox recording process
- `my-whisper-stop-record`: Interrupts recording, triggers transcription
- `my-whisper--transcribe`: Async whisper-cli call with sentinel that inserts output at marker position
- `my-whisper--validate-environment`: Checks sox, whisper-cli, and model paths before operations
- `my-whisper-transcribe-file`: Transcribes an existing WAV file without recording
- `my-whisper-insert-from-history`: Browse and re-insert previous transcriptions

### Customization Variables

- `my-whisper-homedir`: whisper.cpp installation directory (default: `~/whisper.cpp/`)
- `my-whisper-model`: Model file name (e.g., `ggml-medium.en.bin`)
- `my-whisper-vocabulary-file`: Path to vocabulary hints file
- `my-whisper-sox`: sox executable path
- `my-whisper-key-for-toggle`: Key binding for toggle (default: `C-c .`)
- `my-whisper-history-capacity`: Max transcriptions in history ring (default: 20)

## External Dependencies

- **sox**: Audio recording (`sox -d -r 16000 -c 1 -b 16 output.wav`)
- **whisper.cpp**: Located at `my-whisper-homedir/build/bin/whisper-cli`
- **Models**: Located at `my-whisper-homedir/models/<model-name>`
