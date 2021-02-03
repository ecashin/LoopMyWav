module Transient

open System

open Deedle
open MathNet.Numerics
open XPlot.Plotly

type Envelope = {
    Pos: int
    Env: float []
}

let advanceEnvelope reTrigger env trigger =
    let nextEnv e =
        let nextPos = e.Pos + 1
        if nextPos >= e.Env.Length then
            None
        else
            Some({e with Pos = nextPos})
    match env, reTrigger, trigger with
    | Some(e), false, _
    | Some(e), true, false ->
        e.Env.[e.Pos], nextEnv e
    | Some(_), true, true
    | None, _, true ->
        let newEnv = {
            Pos = 0
            Env = Window.Tukey(40, 0.3)  // XXXtodo: parameterize
        }
        newEnv.Env.[newEnv.Pos], Some(newEnv)
    | _, _, _ -> 0.0, None

let samplesToChannels (samples: int [] []) =
    let nChans = samples.[0].Length
    Array.init nChans (fun i ->
        Array.map (fun (smpl: int []) -> smpl.[i] |> float) samples
    )

let smoothedAmplitudeChannels (sr:int) (channels: Series<int,float> []) =
    channels
    |> Array.map (fun x -> Deedle.Math.Stats.ewmMean (x=x, alpha=0.1))

let channelAttacks smoothedChannel =
    let forward = Deedle.Math.Stats.ewmMean (x=smoothedChannel, alpha=0.1)
    let backward = (Deedle.Math.Stats.ewmMean (x=smoothedChannel.Reversed, alpha=0.1)).Reversed
    let n = Series.countKeys smoothedChannel
    let attacks = Array.init n (fun i ->
        if i = 0 || i + 1 = n then
            0.0
        else if forward.[i] < smoothedChannel.[i] && smoothedChannel.[i] > backward.[i + 1] then
            220.0
        else
            0.0
    )
    Series.ofValues attacks

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
    let attacks =
        smoothed
        |> Array.map channelAttacks
    let enveloped =
        let rec getEnvs (i:int) currEnv =
            if i >= nSamples then
                []
            else
                let attackSumAtI =
                    attacks
                    |> Array.sumBy (fun s -> s |> Series.get i)
                let amp, nextEnv = advanceEnvelope false currEnv (attackSumAtI > 0.0)
                amp :: (getEnvs (i + 1) nextEnv)
        (Series.ofValues (getEnvs 0 None)) * 200.0 // XXXdebug: 200
    let columns = Array.concat [|
        samplesToChannels noiseSamples |> Array.map Series.ofValues
        smoothed
        attacks
        [|enveloped|]
        [|sinTheta|]
    |]
    let colNames = Array.concat [|
        channelNames "raw"
        channelNames "smoothed"
        channelNames "attacks"
        [|"enveloped"|]
        [|"sin"|]
    |]
    let df =
        frame [
            for i in 0..(columns.Length - 1) do
                colNames.[i] => columns.[i]
        ]
    df.SaveCsv("/dev/stdout")
