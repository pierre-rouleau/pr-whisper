# Pr-Whisper - Speech-to-Text for Emacs

A simple Emacs package that provides speech-to-text functionality using Whisper.cpp.

Record audio directly from Emacs and have it transcribed and inserted in current buffer at point.
The package provides `pr-whisper-mode`, a global minor mode that starts audio recording and transcribes text when stopped.
It also provides the `pr-whisper-transcribe-file` command to transcribe an already recorded WAV audio file.

I will eventually change this page in my fork.  This is under construction.

More information on how to use this fork of the project is available
in my PEL project inside the [Writing Tools PDF](https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/writing-tools.pdf#page=2).
[PEL](https://github.com/pierre-rouleau/pel#readme) can install the package automatically and creates bindings for the global commands.



## Demo Video

[![Perfect Speech to Text in Emacs](https://img.youtube.com/vi/bTU9ctFtyBA/0.jpg)](https://youtu.be/bTU9ctFtyBA)

**[Perfect Speech to Text in Emacs](https://youtu.be/bTU9ctFtyBA)** - See the package in action!

## Features

- **Simple toggle interface**: Single key (`C-c .`) to start/stop recording
- **Model selection**: Choose from multiple Whisper models via customization
- **Vocabulary hints**: Provide a custom vocabulary file to improve recognition of proper nouns and specialized
  terms (e.g., Greek names like Socrates, Alcibiades, Diotima)
- **Transcription history**: Browse and re-insert previous transcriptions with `M-x pr-whisper-insert-from-history`
- Automatic transcription using Whisper.cpp
- Text insertion at cursor position
- Async processing - Emacs remains responsive during transcription

## Prerequisites

Before setting up this package, you need to install the following system dependencies:

### 1. Sox (for audio recording)

**Ubuntu/Debian:**
```bash
sudo apt install sox
```

**macOS:**
```bash
brew install sox
```

**Arch Linux:**
```bash
sudo pacman -S sox
```

### 2. Whisper.cpp

Clone and build Whisper.cpp:

```bash
# Clone the repository
git clone https://github.com/ggerganov/whisper.cpp.git
cd whisper.cpp

# Build the project
make

# Download models
# For fast mode (required)
bash ./models/download-ggml-model.sh base.en

# For accurate mode (required)
bash ./models/download-ggml-model.sh medium.en
```

## Usage


This provides the **pr-whisper-mode**, a global minor mode that provides the ability to record audio and then process it with whisper.cpp and insert transcribed text in the current buffer at point.

⚠️  🚧 🚧 🚧 Most of the text below is obsolete and needs to be updated.

### Key Bindings

- **`M-x pr-whisper-mode`**: Enable the global minor mode (does not start recording).
- **`C-c .`**: Toggle recording on/off. When stopped, transcribes and inserts text at point.
- **`M-x pr-whisper-mode`**: Disable the mode (stops any active recording).


### Basic Workflow

1. **Enable mode**: `M-x pr-whisper-mode`
2. **Start recording**: Press `C-c .`
3. **Stop and transcribe**: Press `C-c .` again
4. **Get results**: The transcribed text is automatically inserted at your cursor position

### Example

1. Open any text buffer in Emacs
2. Enable the mode: `M-x pr-whisper-mode`
3. Position your cursor where you want the transcribed text
4. Press `C-c .` to start recording
5. Speak into your microphone
6. Press `C-c .` when finished speaking
7. Wait a moment for transcription to complete
8. The text appears at your cursor position

## Configuration

You can customize pr-whisper through Emacs' built-in customization interface or directly in your `init.el`.

### Using Emacs Customize Interface

Run `M-x customize-group RET pr-whisper RET` to access all customization options:

- **pr-whisper-homedir**: Directory where Whisper.cpp is installed (default: `~/whisper.cpp/`)
- **pr-whisper-model**: Which model to use by default
  - `ggml-base.en.bin` - Fast mode (quick, good accuracy)
  - `ggml-medium.en.bin` - Accurate mode (slower, better accuracy)
  - Custom model filename
- **pr-whisper-vocabulary-file**: Path to vocabulary hints file (default: `~/.emacs.d/whisper-vocabulary.txt`)
- **pr-whisper-backend**: Transcription backend - `cli` (default) or `server`
- **pr-whisper-server-port**: Port for whisper-server (default: 8178)

### Custom Configuration in init.el

```elisp
;; Set custom Whisper.cpp installation directory
(setq pr-whisper-homedir "/usr/local/whisper.cpp/")

;; Choose default model (base.en or medium.en)
(setq pr-whisper-model "ggml-medium.en.bin")

;; Set custom vocabulary file location
(setq pr-whisper-vocabulary-file "~/Documents/vocabulary.txt")

;; Use server backend for faster transcription (~29% speedup)
;; Server starts during recording and warms up while you speak
(setq pr-whisper-backend 'server)
```

### Custom Key Bindings

To change the toggle key binding, customize `pr-whisper-key-for-toggle` or set it before loading:

```elisp
;; Use a different key binding for toggle
(setq pr-whisper-key-for-toggle (kbd "C-c s"))
```

### Custom Vocabulary for Proper Nouns

To improve transcription accuracy for proper nouns, technical terms, or specialized vocabulary, create a vocabulary file at `~/.emacs.d/whisper-vocabulary.txt`.

**Example `~/.emacs.d/whisper-vocabulary.txt`:**
```
This transcription discusses classical Greek philosophy, including scholars and figures such as Thrasymachus, Socrates, Plato, Diotima, Alcibiades, and Phaedrus.
```

**Custom vocabulary location:**
```elisp
(setq pr-whisper-vocabulary-file "~/Documents/vocabulary.txt")
```

**For detailed guidance** on vocabulary formats, tips, domain-specific examples, and managing multiple vocabularies, see [VOCABULARY-GUIDE.md](VOCABULARY-GUIDE.md).

## Troubleshooting

### Common Issues

1. **"sox: command not found"**
   - Install sox using your system package manager

2. **"whisper-cli: command not found" or path errors**
   - Ensure Whisper.cpp is built and the path is correct
   - Check that `~/whisper.cpp/build/bin/whisper-cli` exists
   - For server backend, check that `~/whisper.cpp/build/bin/whisper-server` exists
   - If installed elsewhere, customize `pr-whisper-homedir` to match your installation
   - Run `M-x customize-group RET pr-whisper RET` to verify paths

3. **No audio recorded**
   - Check your microphone permissions
   - Test sox manually: `sox -d -r 16000 -c 1 -b 16 test.wav`

4. **Transcription not working**
   - The package now validates paths on startup and will show clear error messages
   - Verify the model files exist in `~/whisper.cpp/models/`
   - Run `M-x pr-whisper-transcribe-fast` to see validation errors
   - Test whisper-cli manually with a wav file

### Testing the Setup

Test each component individually:

```bash
# Test sox recording (record 5 seconds)
sox -d -r 16000 -c 1 -b 16 test.wav trim 0 5

# Test whisper transcription (fast mode)
~/whisper.cpp/build/bin/whisper-cli -m ~/whisper.cpp/models/ggml-base.en.bin -f test.wav

# Test whisper transcription (accurate mode)
~/whisper.cpp/build/bin/whisper-cli -m ~/whisper.cpp/models/ggml-medium.en.bin -f test.wav
```

## How It Works

1. **Recording**: Uses `sox` to record audio at 16kHz, mono, 16-bit
2. **Processing**: Calls `whisper-cli` with the recorded audio file
3. **Integration**: Captures the output and inserts it into your Emacs buffer
4. **Cleanup**: Automatically cleans up temporary files and buffers

## License

This project is released under the MIT License.

## Development

### Running Tests

```bash
emacs -Q --batch -L . -l pr-whisper-test.el -f ert-run-tests-batch-and-exit
```

### Byte Compilation

```bash
./build
```

## Contributing

Feel free to submit issues and pull requests to improve this package.
