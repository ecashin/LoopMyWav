module Grain

open System

open MathNet.Numerics
open MathNet.Numerics.Random

type Grain = {
    Start: int
    Bound: int
    Pos: int
}

let makeEnv maxLen =
    let win = Window.Tukey(maxLen)
    let env (g: Grain) =
        let grainPos = g.Pos - g.Start
        let grainLen = g.Bound - g.Start
        let midGrainPos = grainLen / 2
        let nRemaining = grainLen - grainPos
        if grainPos < midGrainPos then
            win.[grainPos]
        else
            win.[maxLen - nRemaining]
    env

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

let advanceGrain env makeGrain (g: Grain) (source: int [] []): (int [] * Grain) =
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

let Int16Scale = (int Int16.MaxValue) - (int Int16.MinValue) |> float

// https://ccrma.stanford.edu/~jos/pasp/Soft_Clipping.html
let softClip smpl =
    let s = (float smpl) / Int16Scale
    let clipped =
        if s < -1.0 then
            -(2.0 / 3.0)
        else if s > 1.0 then
            2.0 / 3.0
        else
            s - (s * s * s) / 3.0
    clipped * Int16Scale * 0.8

let mix (samples: int [] []) =
    samples.[1..]
    |> Array.fold (fun (acc: int []) (next: int []) ->
        let safeNext =
            if acc.Length <> next.Length then
                eprintfn "acc.Length:%d next.Length:%d" acc.Length next.Length
                eprintfn "samples:%A" samples
                Array.zeroCreate<int> acc.Length
            else
                next
        acc
        |> Array.mapi (fun i s -> s + safeNext.[i])
    ) samples.[0]
    |> Array.map (softClip >> Math.Round >> int)

let advanceGrains env makeGrain (grains: Grain []) (source: int [] []): (int [] * Grain []) =
    let samples, nextGrains =
        grains
        |> Array.map (fun g -> advanceGrain env makeGrain g source)
        |> Array.unzip
    mix samples, nextGrains

type Envs = (Grain -> float) []
type GrainMakers = (unit -> Grain) []
type Sources = int [] [] []  // .[source].[time].[channel]

let granulate (envs: Envs) (grainMakers: GrainMakers) (nGrains: int) (sources: Sources) =
    let grainsPerSource =
        grainMakers
        |> Array.map (fun makeGrain -> Array.init nGrains (fun _ -> makeGrain ()))
    let gen currGrainsPerSource =
        let samples, nextGrainsPerSource =
            currGrainsPerSource
            |> Array.mapi (fun i currGrains ->
                advanceGrains envs.[i] grainMakers.[i] currGrains sources.[i]
            )
            |> Array.unzip
        Some(mix samples, nextGrainsPerSource)
    Seq.unfold gen grainsPerSource