# LoopMyWav

This software is for uncompressed 16-bit PCM WAV files
as used in the Akai MPC 1000.
Specifically, it is for creating loop points for one-note
samples of synths and other sounds
for use in JJOS2XL "INST" programs.

These programs allow the MPC 1000 to be played
with a MIDI keyboard as if it was a synth.

The loop points allow the notes to be sustained indefinitely,
and many sample packs are created in a way that allows such
loops to be created.
However, the loop points often must be set manually.

This software currently reads in the WAV file and parses
its information, performing different actions depening on usage
as shown below.

## Best Effort

This is "best effort" software
that doesn't mind using `assert`
instead of more elaborate error handling.
I want to enter a contest soon with a bunch of looped samples,
so I'm going fast.

## Usage

Print CSV for the samples in the WAV file:

    dotnet run my.wav -s

Print JSON for the parsed WAV info and display
it with the external tool, `jq`:

    dotnet run my.wav -j | jq .

Print start and end loop points as zero-based offsets:

    dotnet run my.wav

## Dependencies

This software requires the .NET Core 5 SDK
and was tested on Linux.
Required libraries are installed
when the software is run
in place using the usage patterns above.

## Coming Soon

Next the software will write a new WAV file with embedded loop points.