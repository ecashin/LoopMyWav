module Transient

open System

open Deedle
open MathNet.Numerics
open XPlot.Plotly

let samplesToChannels (samples: int [] []) =
    let nChans = samples.[0].Length
    Array.init nChans (fun i ->
        Array.map (fun (smpl: int []) -> smpl.[i] |> float) samples
    )

let smoothedAmplitudeChannels (sr:int) (channels: Series<int,float> []) =
    channels
    |> Array.map (fun x -> Deedle.Math.Stats.ewmMean (x=x, alpha=0.1))

let demo (sr:int) (nSamples:int) (nChans:int) =
    let theta = Series.ofValues (Generate.LinearSpaced(nSamples, 0.0, 20.0 * Math.PI))
    let sinTheta = (Series.Sin(theta)) * 200.0
    let rng = Random.SystemRandomSource.Default
    let normal = Distributions.Normal(0.0, 1.0, rng)
    let noiseSamples =
        normal.Samples()
        |> Seq.map (fun x -> x * 100.0 |> int)
        |> Seq.chunkBySize nChans
        |> Seq.take nSamples
        |> Seq.toArray
    // Now make these LoopMyWav-style noise samples into an array of channels
    // and add a sin wav to that noise, creating a noisy but recognizable signal.
    let channels =
        samplesToChannels noiseSamples
        |> Array.map (Series.ofValues >> (fun s -> s + sinTheta))
    let channelNames prefix =
        Array.init noiseSamples.[0].Length (fun i -> sprintf "%s%d" prefix (i + 1))
    let smoothed = smoothedAmplitudeChannels sr channels
    let columns = Array.concat [|
        samplesToChannels noiseSamples |> Array.map Series.ofValues
        smoothed
        [|sinTheta|]
    |]
    let colNames = Array.concat [|
        channelNames "raw"
        channelNames "smoothed"
        [|"sin"|]
    |]
    let df =
        frame [
            for i in 0..(columns.Length - 1) do
                colNames.[i] => columns.[i]
        ]
    df.SaveCsv("/dev/stdout")
