module Arguments

open System

open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
type GranularArgs =
    | N_Grains of nGrains:int
    | N_Seconds of nSeconds:int
    | Out_WavFile of outWavFileName:string
    | In_WavFiles of inWavFileName:string list
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | N_Grains _ -> "Number of grains to use"
            | N_Seconds _ -> "Number of seconds for output WAV"
            | In_WavFiles _ -> "The sources from which to draw grains"
            | Out_WavFile _ -> "The name of the WAV file to create for output"
and LoopMyWavArgs =
    | [<CliPrefix(CliPrefix.None)>] Granular of ParseResults<GranularArgs>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Granular _ -> "Play grains from input files"

let parse argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<LoopMyWavArgs>(programName = "LoopMyWav", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    printfn "Got parse results %A" <| results.GetAllResults()
    results