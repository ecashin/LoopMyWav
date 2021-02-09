module Arguments

open System

open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
type GranularArgs =
    | N_Grains of nGrains:int
    | N_Seconds of nSeconds:int
    | Noise_Config of noiseConfig:string
    | [<Mandatory>] Out_WavFile of outWavFileName:string
    | [<Mandatory>] In_WavFiles of inWavFileName:string list
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | N_Grains _ -> "Number of grains to use"
            | N_Seconds _ -> "Number of seconds for output WAV"
            | Noise_Config _ -> "JSON configuring noise to be added"
            | In_WavFiles _ -> "The sources from which to draw grains"
            | Out_WavFile _ -> "The name of the WAV file to create for output"
and WalkerDemoArgs =
    | N_Steps of nSteps:int
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | N_Steps _ -> "Number of walker steps"
and JsonWavDisplayArgs =
    | [<MainCommand>] JsonWavFile of WAVFILE:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | JsonWavFile _ -> "WAV file for JSON display"
and DecWavDisplayArgs =
    | [<MainCommand>] DecWavFile of WAVFILE:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | DecWavFile _ -> "WAV file for CSV decimal sample display"
and LoopWavsFromFileArgs =
    | [<MainCommand>] LoopWavsFile of WAVSFILE:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | LoopWavsFile _ -> "Text file with input WAV file name per line"
and NoiseWavArgs =
    | [<Mandatory>] NoiseInputWav of WAVFILE:string
    | [<Mandatory>] JsonConfigFile of JSONFILE:string
    | Optimize of SEARCHLOG:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | NoiseInputWav _ -> "Input WAV file to be noisified"
            | JsonConfigFile _ -> "Configuration JSON file"
            | Optimize _ -> "Log Bayesian optimization search to named file"
and TransientsArgs =
    | Demo
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Demo _ -> "Demonstrate transient detection"
and LoopMyWavArgs =
    | [<CliPrefix(CliPrefix.None)>] Granular of ParseResults<GranularArgs>
    | [<CliPrefix(CliPrefix.None)>] Walker_Demo of ParseResults<WalkerDemoArgs>
    | [<CliPrefix(CliPrefix.None)>] JsonWav of ParseResults<JsonWavDisplayArgs>
    | [<CliPrefix(CliPrefix.None)>] DecWav of ParseResults<DecWavDisplayArgs>
    | [<CliPrefix(CliPrefix.None)>] LoopsWav of ParseResults<LoopWavsFromFileArgs>
    | [<CliPrefix(CliPrefix.None)>] Noise of ParseResults<NoiseWavArgs>
    | [<CliPrefix(CliPrefix.None)>] Transients of ParseResults<TransientsArgs>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Granular _ -> "Play grains from input files"
            | Walker_Demo _ -> "Show values from random walkers"
            | JsonWav _ -> "Show JSON for WAV file internals"
            | DecWav _ -> "Show decimal in CSV for WAV samples"
            | LoopsWav _ -> "Read input WAV file names to be looped from lines in file"
            | Noise _ -> "Add noise to WAV file according to JSON configuration"
            | Transients _ -> "Find transients"

let parse argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<LoopMyWavArgs>(programName = "LoopMyWav", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    results
