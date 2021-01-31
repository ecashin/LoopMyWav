# LoopMyWav

This software is for uncompressed
16-bit PCM WAV files
as used in the Akai MPC 1000.
Originally, it was for creating loop points for one-note
samples of synths and other sounds
for use in JJOS2XL "INST" programs.
These programs allow the MPC 1000 to be played
with a MIDI keyboard as if it was a synth.

Several other subcommands have been added
to provide other functionality.

    USAGE: LoopMyWav [--help] [<subcommand> [<options>]]

    SUBCOMMANDS:

        granular <options>    Play grains from input files
        walker-demo <options> Show values from random walkers
        jsonwav <options>     Show JSON for WAV file internals
        decwav <options>      Show decimal in CSV for WAV samples
        loopswav <options>    Read input WAV file names to be looped from lines in
                            file
        noise <options>       Add noise to WAV file according to JSON configuration

        Use 'LoopMyWav <subcommand> --help' for additional information.

    OPTIONS:

        --help                display this list of options.

## Platform

Currently deployment is limited only to Linux,
because the cross-platform GUI support of Eto.Forms
is not used.

## Best Effort

This is "best effort" software
that doesn't mind using `assert`
instead of more elaborate error handling.
I want to write it and use it in music production ASAP,
so I'm going fast.

## Usage

Usage documentation in the subcommand sections below
can be translated to commands run in this repository
by typing `dotnet run -- ` instead of `LoopMyWav`.

## Looping

The loop points allow the notes to be sustained indefinitely,
and many sample packs are created in a way that allows such
loops to be created.
However, the loop points often must be set manually.

The `loopswav` subcommand automates the insertion
of forward loops at "good" loop points
for every input file found in the `WAVSFILE`.

For best results, loop short audio samples,
a few seconds long,
with enough variation to be interesting
but enough consistency or periodicity
for looping to sound good.

    USAGE: LoopMyWav loopswav [--help] [<WAVSFILE>]

    LOOPWAVSFILE:

        <WAVSFILE>            Text file with input WAV file name per line

    OPTIONS:

        --help                display this list of options.

## Granular

Most granular synthesizers use only mono samples
or ignore the distinction between the left and right
channels.

LoopMyWav in granular mode preserves multi-channel audio,
using multi-channel grains.

If you supply more than one audio file,
LoopMyWav will use a slewed Dirichlet process
to mix the granulated tracks.

    USAGE: LoopMyWav granular [--help] [--n-grains <nGrains>]
                            [--n-seconds <nSeconds>]
                            --out-wavfile <outWavFileName>
                            --in-wavfiles [<inWavFileName>...]

    OPTIONS:

        --n-grains <nGrains>  Number of grains to use
        --n-seconds <nSeconds>
                            Number of seconds for output WAV
        --out-wavfile <outWavFileName>
                            The name of the WAV file to create for output
        --in-wavfiles [<inWavFileName>...]
                            The sources from which to draw grains
        --help                display this list of options.

## Walker Demo

The noise generator described below uses randomized accelerations
to modulate speeds that modulate lengths
of delay lines.
To make that more concrete, you can use this demo
to print data for viewing in `ggplot2` in R
or something worse.  (Just kidding.)

    USAGE: LoopMyWav walker-demo [--help] [--n-steps <nSteps>]

    OPTIONS:

        --n-steps <nSteps>    Number of walker steps
        --help                display this list of options.

## JSON WAV Output

This subcommand parses the input WAV file
and prints JSON for its innards.

    USAGE: LoopMyWav jsonwav [--help] [<WAVFILE>]

    JSONWAVFILE:

        <WAVFILE>             WAV file for JSON display

    OPTIONS:

        --help                display this list of options.

## Decimal WAV Output as CSV

This subcommand prints the WAV files samples as CSV.

    USAGE: LoopMyWav decwav [--help] [<WAVFILE>]

    DECWAVFILE:

        <WAVFILE>             WAV file for CSV decimal sample display

    OPTIONS:

        --help                display this list of options.

## Noise

This subcommand causes LoopMyWav to add noise to the input WAV audio
as configured by a JSON file.
A random walker modulates delay lines that are mixed
with the original audio.

A GUI allows you to act as the objective function
for Bayesian optimization over the
random walker's parameters.

Currently this mode *only* supports searching,
but the last one played remains in the output file
when you quit by clicking the GUI's close-window icon.

TODO:
* Add logging of parameters and associated losses
* Support loading of pre-recorded parameters and losses

    USAGE: LoopMyWav noise [--help] [--noiseinputwav <WAVFILE>]
                        [--jsonconfigfile <JSONFILE>] [--optimize]

    OPTIONS:

        --noiseinputwav <WAVFILE>
                            Input WAV file to be noisified
        --jsonconfigfile <JSONFILE>
                            Configuration JSON file
        --optimize            Use a GUI to participate in Bayesian optimization
        --help                display this list of options.

## Dependencies

This software requires the .NET Core 5 SDK
and was tested on Linux.
Required libraries are installed
when the software is run
in place using the usage patterns above.

Audio playback in the noise generator
requires `mpv`,
an audio player.
To use JACK, add `--ao=jack `
to the `mpv` command arguments in `Program.fs`.

The `jq` tool is nice for JSON output
but is not required.
