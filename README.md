# My Whisper - Speech-to-Text for Emacs

A simple Emacs package that provides speech-to-text functionality using Whisper.cpp. Record audio directly from Emacs and have it transcribed and inserted at your cursor position.

## Features

- Record audio with a simple key binding (`C-c v`)
- Automatic transcription using Whisper.cpp
- Text insertion at cursor position
- Non-blocking recording with user-controlled stop

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

# Download a model (base.en is recommended for English)
bash ./models/download-ggml-model.sh base.en
```

Make sure the paths in the code match your installation:
- Whisper binary: `~/whisper.cpp/build/bin/whisper-cli`
- Model file: `~/whisper.cpp/models/ggml-base.en.bin`

If you install Whisper.cpp in a different location, you'll need to update the paths in `my-whisper.el`.

## Installation

### Option 1: Manual Installation

1. Clone or download this repository:
   ```bash
   git clone <your-repo-url> ~/.emacs.d/my-whisper
   ```

2. Add the following to your `init.el` or `.emacs` file:
   ```elisp
   ;; Add the package directory to load-path
   (add-to-list 'load-path "~/.emacs.d/my-whisper")

   ;; Load the package
   (require 'my-whisper)
   ```

### Option 2: Direct File Installation

1. Copy `my-whisper.el` to your Emacs configuration directory:
   ```bash
   cp my-whisper.el ~/.emacs.d/
   ```

2. Add the following to your `init.el`:
   ```elisp
   ;; Load the whisper package
   (load-file "~/.emacs.d/my-whisper.el")
   ```

### Option 3: Using use-package

If you use `use-package`, add this to your `init.el`:

```elisp
(use-package my-whisper
  :load-path "~/.emacs.d/my-whisper"
  :bind ("C-c v" . run-whisper-stt))
```

## Usage

1. **Start recording**: Press `C-c v` to begin recording audio
2. **Stop recording**: Press `C-g` to stop recording and start transcription
3. **Get results**: The transcribed text will be automatically inserted at your cursor position

### Example Workflow

1. Open any text buffer in Emacs
2. Position your cursor where you want the transcribed text
3. Press `C-c v`
4. Speak into your microphone
5. Press `C-g` when finished speaking
6. Wait a moment for transcription to complete
7. The text appears at your cursor position

## Configuration

### Custom Key Binding

To change the key binding, modify your `init.el`:

```elisp
;; Use a different key binding
(global-set-key (kbd "C-c s") 'run-whisper-stt)  ; Instead of C-c v
```

### Custom Paths

If your Whisper.cpp installation is in a different location, you'll need to modify the paths in `my-whisper.el`:

```elisp
;; Example: if whisper-cli is in /usr/local/bin/
;; Edit the format string in my-whisper.el from:
;; "~/whisper.cpp/build/bin/whisper-cli -m ~/whisper.cpp/models/ggml-base.en.bin ..."
;; to:
;; "/usr/local/bin/whisper-cli -m /path/to/your/model.bin ..."
```

## Troubleshooting

### Common Issues

1. **"sox: command not found"**
   - Install sox using your system package manager

2. **"whisper-cli: command not found"**
   - Ensure Whisper.cpp is built and the path is correct
   - Check that `~/whisper.cpp/build/bin/whisper-cli` exists

3. **No audio recorded**
   - Check your microphone permissions
   - Test sox manually: `sox -d -r 16000 -c 1 -b 16 test.wav`

4. **Transcription not working**
   - Verify the model file exists at `~/whisper.cpp/models/ggml-base.en.bin`
   - Test whisper-cli manually with a wav file

### Testing the Setup

Test each component individually:

```bash
# Test sox recording (record 5 seconds)
sox -d -r 16000 -c 1 -b 16 test.wav trim 0 5

# Test whisper transcription
~/whisper.cpp/build/bin/whisper-cli -m ~/whisper.cpp/models/ggml-base.en.bin -f test.wav
```

## How It Works

1. **Recording**: Uses `sox` to record audio at 16kHz, mono, 16-bit
2. **Processing**: Calls `whisper-cli` with the recorded audio file
3. **Integration**: Captures the output and inserts it into your Emacs buffer
4. **Cleanup**: Automatically cleans up temporary files and buffers

## License

This project is released under the MIT License.

## Contributing

Feel free to submit issues and pull requests to improve this package.# my-whisper
