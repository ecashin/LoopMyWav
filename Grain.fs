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

let mix (samples: int [] []) (weights: option<float []>) =
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
    mix samples None, nextGrains

type Envs = (Grain -> float) []
type GrainMakers = (unit -> Grain) []
type Sources = int [] [] []  // .[source].[time].[channel]

type ExpiringTarget = {
    Target: float []
    TTL: int
}

let makeExpiringTarget nWeights minTTL maxTTL =
    let rng = SystemRandomSource.Default
    let weights = Array.init nWeights (fun _ -> rng.NextDouble())
    let rint (x: float) = (Math.Round(x)) |> int
    let ttl = minTTL + rint ((rng.NextDouble()) * (maxTTL - minTTL |> double))
    let sumWeights = weights |> Array.sum
    {
        Target = weights |> Array.map (fun x -> x / sumWeights)
        TTL = ttl
    }

let nextExpiringtarget (e: ExpiringTarget) minTTL maxTTL =
    if e.TTL = 0 then
        makeExpiringTarget e.Target.Length minTTL maxTTL
    else
        { e with TTL = e.TTL - 1 }

type SourceMix = {
    CurrWeights: float []
    ExpiringTarget: ExpiringTarget
}

let nextSourceMix minTTL maxTTL currSourceMix =
    let nextTarget = nextExpiringtarget currSourceMix.ExpiringTarget minTTL maxTTL
    let inertia =
        currSourceMix.CurrWeights
        |> Array.map (fun x -> x * 0.9)
    let pull =
        currSourceMix.ExpiringTarget.Target
        |> Array.map (fun x -> x * 0.1)
    {
        CurrWeights = Array.init pull.Length (fun i -> inertia.[i] + pull.[i])
        ExpiringTarget = nextTarget
    }

let makeSourceMix nSources minTTL maxTTL =
    {
        CurrWeights = Array.zeroCreate nSources
        ExpiringTarget = makeExpiringTarget nSources minTTL maxTTL
    }

let granulate (sr: int) (envs: Envs) (grainMakers: GrainMakers) (nGrains: int) (sources: Sources) =
    let grainsPerSource =
        grainMakers
        |> Array.map (fun makeGrain -> Array.init nGrains (fun _ -> makeGrain ()))
    let minTTL = sr  // / 10
    let maxTTL = sr * 5
    let sourceMix = makeSourceMix sources.Length minTTL maxTTL
    let nextSrcMix = nextSourceMix minTTL maxTTL
    let gen (currGrainsPerSource, currSourceMix) =
        let samples, nextGrainsPerSource =
            currGrainsPerSource
            |> Array.mapi (fun i currGrains ->
                advanceGrains envs.[i] grainMakers.[i] currGrains sources.[i]
            )
            |> Array.unzip
        let sample = mix samples (Some(currSourceMix.CurrWeights))
        Some(sample, (nextGrainsPerSource, nextSrcMix currSourceMix))
    Seq.unfold gen (grainsPerSource, sourceMix)