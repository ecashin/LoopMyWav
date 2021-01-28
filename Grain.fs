module Grain

open System

open MathNet.Numerics.Random

type Grain = {
    Start: int
    Bound: int
    Pos: int
}

let env (g: Grain) =
    let fourth = (g.Bound - g.Start) / 4 |> float
    let ramp = min (g.Pos - g.Start) (g.Bound - g.Pos) |> float
    Math.Clamp(ramp, 0.0, fourth) / fourth

let makeGrainMaker minLen maxLen (source: int [] []) =
    let rng = SystemRandomSource.Default
    let newStart () =
        let extent = source.Length - maxLen
        Math.Round((float extent) * rng.NextDouble()) |> int
    let newLen () =
        let extra = (maxLen - minLen) |> float
        minLen + (Math.Round((float extra) * (rng.NextDouble())) |> int)
    fun () ->
        let start = newStart ()
        {
            Start = start
            Bound = start + (newLen ())
            Pos = start
        }

let advanceGrain makeGrain (g: Grain) (source: int [] []): (int [] * Grain) =
    let sample =
        source.[g.Pos]
        |> Array.map (fun chan ->
            Math.Round((env g) * (float chan)) |> int
        )
    let nextPos = g.Pos + 1
    if nextPos >= g.Bound then
        sample, (makeGrain ())
    else
        sample, {
            g with Pos = g.Pos + 1
        }

let advanceGrains makeGrain (grains: Grain []) (source: int [] []): (int [] * Grain []) =
    let samplesAndNextGrains =
        grains
        |> Array.map (fun g -> advanceGrain makeGrain g source)
    let samples =
        samplesAndNextGrains
        |> Array.map fst
    let zero = Array.zeroCreate<int> source.[0].Length
    let mix =
        samples
        |> Array.fold (fun (acc: int []) (next: int []) ->
            acc
            |> Array.mapi (fun i s -> s + next.[i])
        ) zero
        |> Array.map (fun chan ->
            Math.Round((float chan) / (float grains.Length)) |> int
        )
    let nextGrains =
        samplesAndNextGrains
        |> Array.map snd
    mix, nextGrains

let granulate (makeGrain: unit -> Grain) (nGrains: int) (source: int [] []) =
    let grains = Array.init nGrains (fun _ -> makeGrain ())
    let gen currGrains =
        let sample, nextGrains = advanceGrains makeGrain currGrains source
        Some(sample, nextGrains)
    Seq.unfold gen grains