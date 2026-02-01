# Whisper Speed Test Results

**Date:** 2026-01-30
**Model:** ggml-medium.en.bin
**Audio:** testinput.wav (18.2 seconds)
**Hardware:** Apple M3

## Speed Comparison

| Run | CLI (beam) | CLI (greedy) | Fresh Server | Warm Server |
|-----|------------|--------------|--------------|-------------|
| 1   | 2.56s      | 2.06s        | 2.06s        | 1.49s       |
| 2   | 2.56s      | 2.06s        | 2.11s        | 1.47s       |
| 3   | 2.56s      | 2.09s        | 2.05s        | 1.47s       |
| **Avg** | **2.56s** | **2.07s** | **2.07s** | **1.48s** |

## Default Parameters

| Setting    | CLI default | Server default |
|------------|-------------|----------------|
| beam-size  | 5           | -1 (greedy)    |
| best-of    | 5           | 2              |

## Transcription Quality Comparison

### CLI with beam search (default: -bs 5 -bo 5)
```
Hello, my name is Tommy. Nice to meet you. Thank you for coming to my TED Talk. My TED
Talk is on how to record an audio file. Mary had a little lamb. Blah, blah, blah, blah,
blah, blah. And here we go. Thank you.
```

### CLI with greedy decoding (-bs 1 -bo 1)
```
Hello, my name is Tommy. Nice to meet you. Thank you for coming to my TED Talk. My TED
Talk is on how to record an audio file. Mary had a little lamb blah blah blah blah blah
blah. And here we go. Thank you.
```

### Server (greedy by default)
```
Hello, my name is Tommy. Nice to meet you. Thank you for coming to my TED Talk. My TED
Talk is on how to record an audio file. Mary had a little lamb, blah blah blah blah blah
blah. And here we go. Thank you.
```

## Punctuation Differences

| Method       | "lamb" → "blah" | Between "blah"s |
|--------------|-----------------|-----------------|
| CLI beam     | Period, capital | Commas          |
| CLI greedy   | No punctuation  | No punctuation  |
| Server       | Comma           | No punctuation  |

## Conclusions

1. **CLI vs Server with same parameters:** Identical speed (~2.07s including model load)
2. **Warm server advantage:** ~0.6s faster (no model load), ~29% speedup
3. **Beam search cost:** ~0.5s slower than greedy (~24% overhead)
4. **Transcription quality:** All methods produce identical words; beam search yields better punctuation
