module Audio

open Accord.Audio.Formats
open Accord.Audio.Windows
open Accord.Math
open Accord.Statistics

// argTopK reimplemented by N, modified to return indices
let argTopK k (a: 'T []) =
    let folder acc nextIdx =
        if a.[nextIdx] > (a.[Array.head acc]) then
           acc.[0] <- nextIdx
           Array.sortInPlaceBy (fun i -> a.[i]) acc
           acc
        else acc
    match Array.length a with
    | n when n <= k ->
        Array.indexed a
        |> Array.sortBy snd
        |> Array.map fst
    | _ ->
        let ai = Array.mapi (fun i _ -> i) a
        (Array.sortBy (fun i -> a.[i]) ai.[..k - 1], ai.[k..])
        ||> Array.fold folder
        |> Array.rev

type FFT = struct
        val freqs: float []     // same for each chunk
        val powers: float [] [] // different for each chunk
        new(freqs, powers) = {
                freqs = freqs
                powers = powers
        }
end

let chanFFTs (i: int) (complex: Accord.Audio.ComplexSignal array) (split: int) (sr: int) : FFT =
        let getChan i (c: Accord.Audio.ComplexSignal) = c.GetChannel(i)
        let channels = Array.map (getChan i) complex
        let powers = Array.map (fun c -> Accord.Audio.Tools.GetPowerSpectrum(c)) channels
        let freqvs = Array.map (fun c -> Accord.Audio.Tools.GetFrequencyVector(split, sr)) channels
        FFT(freqvs.[0], powers)

let chanFFTsFromWindows (split: int) (sr: int) (windows: Accord.Audio.Signal []) : FFT [] =
        let complex = windows.Apply(fun w -> Accord.Audio.ComplexSignal.FromSignal(w))
        complex.ForwardFourierTransform()
        [|
                for i in 0..(complex.[0].Channels - 1) do
                chanFFTs i complex split sr
        |]

let scaleToRange (targetMin: float) (targetMax: float) (inValues: float[]) : float [] =
    let inMax = inValues |> Array.max
    let inMin = inValues |> Array.min
    let sign = if targetMax > targetMin then 1.0 else -1.0
    let scale = sign * (System.Math.Abs(targetMax - targetMin) / (inMax - inMin))
    inValues
    |> Array.map (fun x -> scale * x)

exception InsufficientSamplesException of string

// "frames per second" - we chunk audio into "frames" that result in images
type Track(fileName: string, fps: int) =
        let signal = AudioDecoder.DecodeFromFile(fileName)
        let window = RaisedCosineWindow.Hamming(256)
        let split = signal.SampleRate / fps
        let windows = signal.Split(window, split)
        let signalSamples (w: Accord.Audio.Signal) = w.Samples
        let ffts = chanFFTsFromWindows split signal.SampleRate windows
        let powers = [| for f in ffts do f.powers |]
        let freqs = ffts.[0].freqs
        member this.FPS with get() = fps
        member this.Powers with get() = powers
        member this.Signal with get() = signal
        member this.Freqs with get() = freqs
        member this.PowerStats(stat: (float [] -> float)) =
                [|
                        for chanPowers in powers do
                        [|
                                for p in chanPowers do
                                p |> stat
                        |]
                |]
        member this.PowerSumsRaw with get() =
                this.PowerStats(Array.fold (+) 0.0)
        member this.PowerSums with get() =
                [|
                        for chanPowerSums in this.PowerSumsRaw do
                        chanPowerSums |> (scaleToRange 0.0 1.0)
                |]
        member this.PowerMeans with get() =
                this.PowerStats(Array.average)
        member this.PowerSds with get() =
                this.PowerStats(Measures.StandardDeviation)
        member this.PowerEntropies with get() =
                this.PowerStats(Measures.Entropy)
        member this.PowerChanArgMaxes with get(chan: int) =
                let off = 5
                [|
                        for i in 0..(powers.[chan].[0].Length - 1) do
                        Accord.Math.Matrix.ArgMax([| powers.[chan].[i].[off..]|])
                        |> (fun x -> (snd x) + off)
                |]
        member this.FreqRangePowerSums with get(chan: int, fMin: float, fMax: float) =
                let rec findFirstBin predicate idxValPairs =
                        match idxValPairs with
                        | [] -> None
                        | (hdIdx, hdVal)::tail ->
                                if predicate hdVal then
                                        Some(hdIdx)
                                else
                                        findFirstBin predicate tail
                let firstBin = findFirstBin (fun x -> x >= fMin) (freqs |> Array.indexed |> Array.toList)
                let lastBin = findFirstBin (fun x -> x <= fMax) (freqs |> Array.indexed |> Array.rev |> Array.toList)
                match (firstBin, lastBin) with
                | (None, None) | (None, _) | (_, None) -> raise (InsufficientSamplesException("no matching bins"))
                | (Some(first), Some(last)) -> powers.[chan] |> Array.map (fun p -> p.[first..last] |> Array.fold (+) 0.0)

// https://www.extremeoptimization.com/Documentation/Mathematics/Curve-Fitting/Smoothing.aspx
// Costs $1000.
// http://accord-framework.net/docs/html/M_Accord_Math_Matrix_Transpose__1_8.htm
let smoothed (windowSize: int) (p: float[] []) : (float [] []) =
        let mavgers = [|
                for i in 0..(p.Length) do
                Accord.Statistics.Moving.MovingNormalStatistics(windowSize)
        |]
        let mutable smooth = [|
                for i in 1..(p.Length) do
                [| for j in 1..(p.[0].Length) do 0.0 |]
        |]
        for i in 0..(p.Length - 1) do
                for j in 0..(p.[0].Length - 1) do
                        mavgers.[i].Push(p.[i].[j])
                        smooth.[i].[j] <- mavgers.[i].Mean
        smooth

let rec sqrtize times a =
        let s = a |> Array.map System.Math.Sqrt |> (scaleToRange 0.0 1.0)
        if times = 1 then
                s
        else
                sqrtize (times - 1) s